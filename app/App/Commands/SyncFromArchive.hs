{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core                     (Region (..), runResAws, toText)
import Antiope.Env                      (mkEnv)
import Antiope.Options.Applicative      (autoText)
import App.Commands.Options.Parser      (text)
import App.Commands.Options.Types       (SyncFromArchiveOptions (SyncFromArchiveOptions))
import Control.Applicative              (Alternative(..))
import Control.Lens                     ((<&>), (&), (^..), (^.), (%~), (.~), Each(each))
import Control.Monad                    (void, when, unless, forM_)
import Control.Monad.Except             (MonadError(..), MonadIO(..), ExceptT(..), MonadTrans(lift), runExceptT)
import Control.Monad.Trans.AWS          (envOverride, setEndpoint, AWST', Env)
import Control.Monad.Trans.Resource     (ResourceT)
import Data.ByteString                  (ByteString)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.Maybe                       (fromMaybe)
import Data.Monoid                      (Dual(Dual), Endo(Endo))
import HaskellWorks.CabalCache.AppError (displayAppError, AppError)
import HaskellWorks.CabalCache.IO.Error (exceptWarn, maybeToExcept)
import HaskellWorks.CabalCache.Location (toLocation, (<.>), (</>))
import HaskellWorks.CabalCache.Metadata (loadMetadata)
import HaskellWorks.CabalCache.Show     (tshow)
import HaskellWorks.CabalCache.Types    (PackageId)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              (Mod, Parser, CommandFields)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types                       as Z
import qualified App.Static                                       as AS
import qualified Control.Concurrent.STM                           as STM
import qualified Data.ByteString.Char8                            as C8
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.List                                        as L
import qualified Data.Map                                         as M
import qualified Data.Map.Strict                                  as Map
import qualified Data.Text                                        as T
import qualified HaskellWorks.CabalCache.Aws.Env                  as AWS
import qualified HaskellWorks.CabalCache.Concurrent.DownloadQueue as DQ
import qualified HaskellWorks.CabalCache.Concurrent.Fork          as IO
import qualified HaskellWorks.CabalCache.Core                     as Z
import qualified HaskellWorks.CabalCache.Data.List                as L
import qualified HaskellWorks.CabalCache.GhcPkg                   as GhcPkg
import qualified HaskellWorks.CabalCache.Hash                     as H
import qualified HaskellWorks.CabalCache.IO.Console               as CIO
import qualified HaskellWorks.CabalCache.IO.Lazy                  as IO
import qualified HaskellWorks.CabalCache.IO.Tar                   as IO
import qualified HaskellWorks.CabalCache.Types                    as Z
import qualified Options.Applicative                              as OA
import qualified System.Directory                                 as IO
import qualified System.IO                                        as IO
import qualified System.IO.Temp                                   as IO
import qualified System.IO.Unsafe                                 as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

skippable :: Z.Package -> Bool
skippable package = package ^. the @"packageType" == "pre-existing"

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let hostEndpoint          = opts ^. the @"hostEndpoint"
  let storePath             = opts ^. the @"storePath"
  let archiveUris           = opts ^. the @"archiveUris"
  let threads               = opts ^. the @"threads"
  let awsLogLevel           = opts ^. the @"awsLogLevel"
  let versionedArchiveUris  = archiveUris & each %~ (</> archiveVersion)
  let storePathHash         = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUris     = versionedArchiveUris & each %~ (</> T.pack storePathHash)

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  forM_ archiveUris $ \archiveUri -> do
    CIO.putStrLn $ "Archive URI: "      <> toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  mbPlan <- Z.loadPlan $ opts ^. the @"buildPath"

  case mbPlan of
    Right planJson -> do
      compilerContextResult <- runExceptT $ Z.mkCompilerContext planJson

      case compilerContextResult of
        Right compilerContext -> do
          GhcPkg.testAvailability compilerContext

          envAws <- IO.unsafeInterleaveIO $ (<&> envOverride .~ Dual (Endo $ \s -> case hostEndpoint of
            Just (hostname, port, ssl) -> setEndpoint ssl hostname port s
            Nothing -> s))
            $ mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
          let compilerId                  = planJson ^. the @"compilerId"
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
            GhcPkg.init compilerContext storeCompilerPackageDbPath

          packages <- Z.getPackages storePath planJson

          let installPlan = planJson ^. the @"installPlan"
          let planPackages = M.fromList $ fmap (\p -> (p ^. the @"id", p)) installPlan

          let planDeps0 = installPlan >>= \p -> fmap (p ^. the @"id", ) $ mempty
                <> (p ^. the @"depends")
                <> (p ^. the @"exeDepends")
                <> (p ^.. the @"components" . each . the @"lib" . each . the @"depends"    . each)
                <> (p ^.. the @"components" . each . the @"lib" . each . the @"exeDepends" . each)
          let planDeps  = planDeps0 <> fmap (\p -> ("[universe]", p ^. the @"id")) installPlan

          downloadQueue <- STM.atomically $ DQ.createDownloadQueue planDeps

          let pInfos = M.fromList $ fmap (\p -> (p ^. the @"packageId", p)) packages

          IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
            IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

            IO.forkThreadsWait threads $ DQ.runQueue downloadQueue $ \packageId -> case M.lookup packageId pInfos of
              Just pInfo -> do
                let archiveBaseName     = Z.packageDir pInfo <.> ".tar.gz"
                let archiveFiles        = versionedArchiveUris & each %~ (</> T.pack archiveBaseName)
                let scopedArchiveFiles  = scopedArchiveUris & each %~ (</> T.pack archiveBaseName)
                let packageStorePath    = storePath </> Z.packageDir pInfo
                let maybePackage        = M.lookup packageId planPackages

                storeDirectoryExists <- doesDirectoryExist packageStorePath

                case maybePackage of
                  Nothing -> do
                    CIO.hPutStrLn IO.stderr $ "Warning: package not found" <> packageId
                    return DQ.DownloadSuccess
                  Just package -> if skippable package
                    then do
                      CIO.putStrLn $ "Skipping: " <> packageId
                      return DQ.DownloadSuccess
                    else if storeDirectoryExists
                      then return DQ.DownloadSuccess
                      else runResAws envAws $ onError (cleanupStorePath packageStorePath packageId) DQ.DownloadFailure $ do
                        (existingArchiveFileContents, existingArchiveFile) <- ExceptT $ IO.readFirstAvailableResource envAws (foldMap L.tuple2ToList (L.zip archiveFiles scopedArchiveFiles))
                        CIO.putStrLn $ "Extracting: " <> toText existingArchiveFile

                        let tempArchiveFile = tempPath </> archiveBaseName
                        liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents
                        IO.extractTar tempArchiveFile storePath

                        meta <- loadMetadata packageStorePath
                        oldStorePath <- maybeToExcept "store-path is missing from Metadata" (Map.lookup "store-path" meta)

                        case Z.confPath pInfo of
                          Z.Tagged conf _ -> do
                            let theConfPath = storePath </> conf
                            let tempConfPath = tempPath </> conf
                            confPathExists <- liftIO $ IO.doesFileExist theConfPath
                            when confPathExists $ do
                              confContents <- liftIO $ LBS.readFile theConfPath
                              liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
                              liftIO $ IO.copyFile tempConfPath theConfPath >> IO.removeFile tempConfPath

                            return DQ.DownloadSuccess
              Nothing -> do
                CIO.hPutStrLn IO.stderr $ "Warning: Invalid package id: " <> packageId
                return DQ.DownloadSuccess

          CIO.putStrLn "Recaching package database"
          GhcPkg.recache compilerContext storeCompilerPackageDbPath

          failures <- STM.atomically $ STM.readTVar $ downloadQueue ^. the @"tFailures"

          forM_ failures $ \packageId -> CIO.hPutStrLn IO.stderr $ "Failed to download: " <> packageId
        Left msg -> CIO.hPutStrLn IO.stderr msg
    Left appError -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  return ()

cleanupStorePath :: FilePath -> PackageId -> AppError -> AWST' Env (ResourceT IO) ()
cleanupStorePath packageStorePath packageId e = do
  CIO.hPutStrLn IO.stderr $ "Warning: Sync failure: " <> packageId <> ", reason: " <> displayAppError e
  pathExists <- liftIO $ IO.doesPathExist packageStorePath
  when pathExists $ void $ IO.removePathRecursive packageStorePath

onError :: ()
  => (AppError -> AWST' Env (ResourceT IO) ())
  -> DQ.DownloadStatus
  -> ExceptT AppError (AWST' Env (ResourceT IO)) DQ.DownloadStatus
  -> AWST' Env (ResourceT IO) DQ.DownloadStatus
onError h failureValue f = do
  result <- runExceptT $ catchError (exceptWarn f) handler
  case result of
    Left _  -> return failureValue
    Right a -> return a
  where handler e = lift (h e) >> return failureValue

optsSyncFromArchive :: Parser SyncFromArchiveOptions
optsSyncFromArchive = SyncFromArchiveOptions
  <$> OA.option (OA.auto <|> text)
      (  OA.long "region"
      <> OA.metavar "AWS_REGION"
      <> OA.showDefault <> OA.value Oregon
      <> OA.help "The AWS region in which to operate"
      )
  <*> some
      (  OA.option (OA.maybeReader (toLocation . T.pack))
        (   OA.long "archive-uri"
        <>  OA.help "Archive URI to sync to"
        <>  OA.metavar "S3_URI"
        )
      )
  <*> OA.strOption
      (   OA.long "build-path"
      <>  OA.help ("Path to cabal build directory.  Defaults to " <> show AS.buildPath)
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.buildPath
      )
  <*> OA.strOption
      (   OA.long "store-path"
      <>  OA.help ("Path to cabal store.  Defaults to " <> show AS.cabalDirectory)
      <>  OA.metavar "DIRECTORY"
      <>  OA.value (AS.cabalDirectory </> "store")
      )
  <*> OA.optional
      ( OA.strOption
        (   OA.long "store-path-hash"
        <>  OA.help "Store path hash (do not use)"
        <>  OA.metavar "HASH"
        )
      )
  <*> OA.option OA.auto
      (   OA.long "threads"
      <>  OA.help "Number of concurrent threads"
      <>  OA.metavar "NUM_THREADS"
      <>  OA.value 4
      )
  <*> OA.optional
      ( OA.option autoText
        (   OA.long "aws-log-level"
        <>  OA.help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
        <>  OA.metavar "AWS_LOG_LEVEL"
        )
      )
  <*> OA.optional parseEndpoint

parseEndpoint :: Parser (ByteString, Int, Bool)
parseEndpoint =
  (,,)
  <$> OA.option autoText
        (   OA.long "host-name-override"
        <>  OA.help "Override the host name (default: s3.amazonaws.com)"
        <>  OA.metavar "HOST_NAME"
        )
  <*> OA.option OA.auto
        (   OA.long "host-port-override"
        <>  OA.help "Override the host port"
        <>  OA.metavar "HOST_PORT"
        )
  <*> OA.option OA.auto
        (   OA.long "host-ssl-override"
        <>  OA.help "Override the host SSL"
        <>  OA.metavar "HOST_SSL"
        )

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = OA.command "sync-from-archive"  $ flip OA.info OA.idm $ runSyncFromArchive <$> optsSyncFromArchive
