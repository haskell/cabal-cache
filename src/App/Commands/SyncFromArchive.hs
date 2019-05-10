{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core                     (runResAws, toText)
import Antiope.Env                      (LogLevel, mkEnv)
import App.Commands.Options.Parser      (optsSyncFromArchive)
import App.Static                       (homeDirectory)
import Control.Lens                     hiding ((<.>))
import Control.Monad                    (unless, void, when)
import Control.Monad.Except
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.List                        (nub, sort)
import Data.Maybe
import Data.Semigroup                   ((<>))
import Data.Text                        (Text)
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Core     (PackageInfo (..), Presence (..), Tagged (..), getPackages, loadPlan)
import HaskellWorks.CabalCache.IO.Error (exceptWarn, maybeToExcept, maybeToExceptM)
import HaskellWorks.CabalCache.Location ((<.>), (</>))
import HaskellWorks.CabalCache.Metadata (deleteMetadata, loadMetadata)
import HaskellWorks.CabalCache.Show
import HaskellWorks.CabalCache.Topology (buildPlanData)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Network.AWS.Types                (Region (Oregon))
import Options.Applicative              hiding (columns)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types                       as Z
import qualified Codec.Archive.Tar                                as F
import qualified Codec.Compression.GZip                           as F
import qualified Control.Concurrent                               as IO
import qualified Control.Concurrent.STM                           as STM
import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Char8                            as C8
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.Map                                         as M
import qualified Data.Map.Strict                                  as Map
import qualified Data.Set                                         as S
import qualified Data.Text                                        as T
import qualified HaskellWorks.CabalCache.AWS.Env                  as AWS
import qualified HaskellWorks.CabalCache.Concurrent.DownloadQueue as DQ
import qualified HaskellWorks.CabalCache.Concurrent.Fork          as IO
import qualified HaskellWorks.CabalCache.Data.Relation            as R
import qualified HaskellWorks.CabalCache.GhcPkg                   as GhcPkg
import qualified HaskellWorks.CabalCache.Hash                     as H
import qualified HaskellWorks.CabalCache.IO.Console               as CIO
import qualified HaskellWorks.CabalCache.IO.Lazy                  as IO
import qualified HaskellWorks.CabalCache.IO.Tar                   as IO
import qualified HaskellWorks.CabalCache.Types                    as Z
import qualified System.Directory                                 as IO
import qualified System.IO                                        as IO
import qualified System.IO.Temp                                   as IO
import qualified UnliftIO.Async                                   as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

skippable :: Z.Package -> Bool
skippable package = (package ^. the @"packageType" == "pre-existing")

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let storePath           = opts ^. the @"storePath"
  let archiveUri          = opts ^. the @"archiveUri"
  let threads             = opts ^. the @"threads"
  let awsLogLevel         = opts ^. the @"awsLogLevel"
  let versionedArchiveUri = archiveUri </> archiveVersion
  let storePathHash       = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  CIO.putStrLn $ "Archive URI: "      <> toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  GhcPkg.testAvailability

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      envAws <- mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
      let compilerId                  = planJson ^. the @"compilerId"
      let archivePath                 = versionedArchiveUri </> compilerId
      let storeCompilerPath           = storePath </> T.unpack compilerId
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
      let storeCompilerLibPath        = storeCompilerPath </> "lib"

      CIO.putStrLn "Creating store directories"
      createDirectoryIfMissing True storePath
      createDirectoryIfMissing True storeCompilerPath
      createDirectoryIfMissing True storeCompilerLibPath

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $ do
        CIO.putStrLn "Package DB missing. Creating Package DB"
        GhcPkg.init storeCompilerPackageDbPath

      packages <- getPackages storePath planJson

      let installPlan = planJson ^. the @"installPlan"
      let planPackages = M.fromList $ fmap (\p -> (p ^. the @"id", p)) installPlan

      let planData = buildPlanData planJson (packages ^.. each . the @"packageId")

      let planDeps0 = installPlan >>= \p -> fmap (p ^. the @"id", ) $ mempty
            <> (p ^. the @"depends")
            <> (p ^. the @"exeDepends")
            <> (p ^.. the @"components" . each . the @"lib" . each . the @"depends"    . each)
            <> (p ^.. the @"components" . each . the @"lib" . each . the @"exeDepends" . each)
      let planDeps  = planDeps0 <> fmap (\p -> ("[universe]", p ^. the @"id")) installPlan

      downloadQueue <- STM.atomically $ DQ.createDownloadQueue planDeps

      let pInfos = M.fromList $ fmap (\p -> (p ^. the @"packageId", p)) packages

      -- forM_ planDeps $ \(a, b) -> do
      --   let maybeName = M.lookup a planPackages <&> (^. the @"name")
      --   case maybeName of
      --     Just name -> CIO.putStrLn $ name <> " " <> a <> " -> " <> b
      --     Nothing   -> CIO.putStrLn $ "*********" <> a <> " -> " <> b

      IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
        IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

        IO.forkThreadsWait threads $ DQ.runQueue downloadQueue $ \packageId -> case M.lookup packageId pInfos of
          Just pInfo -> do
            let archiveBaseName   = packageDir pInfo <.> ".tar.gz"
            let archiveFile       = versionedArchiveUri </> T.pack archiveBaseName
            let scopedArchiveFile = scopedArchiveUri </> T.pack archiveBaseName
            let packageStorePath  = storePath </> packageDir pInfo
            storeDirectoryExists <- doesDirectoryExist packageStorePath
            let maybePackage = M.lookup packageId planPackages

            case maybePackage of
              Nothing -> do
                CIO.hPutStrLn IO.stderr $ "Warning: package not found" <> packageId
                return True
              Just package -> if skippable package
                then do
                  CIO.putStrLn $ "Skipping: " <> packageId
                  return True
                else if storeDirectoryExists
                  then return True
                  else do
                    maybeExistingArchiveFile <- IO.firstExistingResource envAws [scopedArchiveFile, archiveFile]
                    case maybeExistingArchiveFile of
                      Just existingArchiveFile -> do
                        CIO.putStrLn $ "Extracting: " <> toText existingArchiveFile
                        runResAws envAws $ onErrorClean packageStorePath False $ do
                          maybeArchiveFileContents <- IO.readResource envAws existingArchiveFile

                          case maybeArchiveFileContents of
                            Just archiveFileContents -> do
                              existingArchiveFileContents <- IO.readResource envAws existingArchiveFile & maybeToExceptM (GenericAppError ("Archive unavailable: " <> tshow (toText archiveFile)))
                              let tempArchiveFile = tempPath </> archiveBaseName
                              liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents
                              IO.extractTar tempArchiveFile storePath

                              meta <- loadMetadata packageStorePath
                              oldStorePath <- maybeToExcept (GenericAppError "store-path is missing from Metadata") (Map.lookup "store-path" meta)

                              case confPath pInfo of
                                Tagged conf _ -> do
                                  let theConfPath = storePath </> conf
                                  let tempConfPath = tempPath </> conf
                                  confPathExists <- liftIO $ IO.doesFileExist theConfPath
                                  when confPathExists $ do
                                    confContents <- liftIO $ LBS.readFile theConfPath
                                    liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
                                    liftIO $ IO.renamePath tempConfPath theConfPath
                                  return True
                            Nothing -> do
                              CIO.putStrLn $ "Archive unavailable: " <> toText existingArchiveFile
                              deleteMetadata packageStorePath
                              return False
                      Nothing -> do
                        CIO.hPutStrLn IO.stderr $ "Warning: Sync failure: " <> packageId
                        return False
          Nothing -> do
            CIO.hPutStrLn IO.stderr $ "Warning: Invalid package id: " <> packageId
            return True

      dependenciesRemaining <- STM.atomically $ STM.readTVar (downloadQueue ^. the @"tDependencies")

      CIO.putStrLn "Recaching package database"
      GhcPkg.recache storeCompilerPackageDbPath

      failures <- STM.atomically $ STM.readTVar $ downloadQueue ^. the @"tFailures"

      forM_ failures $ \packageId -> CIO.hPutStrLn IO.stderr $ "Failed to download: " <> packageId

    Left appError -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  return ()

onErrorClean :: MonadIO m => FilePath -> a -> ExceptT AppError m a -> m a
onErrorClean pkgStorePath failureValue f = do
  result <- runExceptT $ catchError (exceptWarn f) handler
  case result of
    Left a  -> return failureValue
    Right a -> return a
  where handler e = liftIO (IO.removeDirectoryRecursive pkgStorePath) >> return failureValue

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-archive"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
