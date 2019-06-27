{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive
  ) where

import Antiope.Core                     (toText)
import Antiope.Env                      (mkEnv)
import App.Commands.Options.Parser      (optsSyncToArchive)
import Control.Lens                     hiding ((<.>))
import Control.Monad                    (filterM, unless, when)
import Control.Monad.Except
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Generics.Product.Any        (the)
import Data.List                        ((\\))
import Data.Maybe
import Data.Semigroup                   ((<>))
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Location ((<.>), (</>))
import HaskellWorks.CabalCache.Metadata (createMetadata)
import HaskellWorks.CabalCache.Show
import HaskellWorks.CabalCache.Topology (buildPlanData, canShare)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              hiding (columns)
import System.Directory                 (doesDirectoryExist)

import qualified App.Commands.Options.Types         as Z
import qualified Control.Concurrent.STM             as STM
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.AWS.Env    as AWS
import qualified HaskellWorks.CabalCache.Core       as Z
import qualified HaskellWorks.CabalCache.GhcPkg     as GhcPkg
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.IO.Error   as IO
import qualified HaskellWorks.CabalCache.IO.File    as IO
import qualified HaskellWorks.CabalCache.IO.Lazy    as IO
import qualified HaskellWorks.CabalCache.IO.Tar     as IO
import qualified Network.HTTP.Types                 as HTTP
import qualified System.Directory                   as IO
import qualified System.IO                          as IO
import qualified System.IO.Temp                     as IO
import qualified System.IO.Unsafe                   as IO
import qualified UnliftIO.Async                     as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  let storePath            = opts ^. the @"storePath"
  let archiveUris          = opts ^. the @"archiveUri"
  let threads              = opts ^. the @"threads"
  let awsLogLevel          = opts ^. the @"awsLogLevel"
  let versionedArchiveUris = archiveUris <&> (</> archiveVersion)
  let storePathHash        = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUris    = versionedArchiveUris <&> (</> T.pack storePathHash)

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  forM_ archiveUris $ \u -> do
    CIO.putStrLn $ "Archive URI(s): " <> toText u
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  tEarlyExit <- STM.newTVarIO False

  mbPlan <- Z.loadPlan
  case mbPlan of
    Right planJson -> do
      let compilerId = planJson ^. the @"compilerId"
      envAws <- IO.unsafeInterleaveIO $ mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
      let archivePaths       = versionedArchiveUris <&> (</> compilerId)
      let scopedArchivePaths = scopedArchiveUris <&> (</> compilerId)
      forM_ archivePaths       IO.createLocalDirectoryIfMissing
      forM_ scopedArchivePaths IO.createLocalDirectoryIfMissing

      packages     <- Z.getPackages storePath planJson
      nonShareable <- packages & filterM (fmap not . isShareable storePath)
      let planData = buildPlanData planJson (nonShareable ^.. each . the @"packageId")

      let storeCompilerPath           = storePath </> T.unpack compilerId
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $
        GhcPkg.init storeCompilerPackageDbPath

      CIO.putStrLn $ "Syncing " <> tshow (length packages) <> " packages"

      IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
        CIO.putStrLn $ "Temp path: " <> tshow tempPath

        IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
          earlyExit <- STM.readTVarIO tEarlyExit
          unless earlyExit $ do
            let archiveFileBasename = Z.packageDir pInfo <.> ".tar.gz"
            let archiveFiles        = versionedArchiveUris <&> (</> T.pack archiveFileBasename)
            let scopedArchiveFiles  = versionedArchiveUris <&> (</> T.pack storePathHash </> T.pack archiveFileBasename)
            let packageStorePath    = storePath </> Z.packageDir pInfo

            -- either write "normal" package, or a user-specific one if the package cannot be shared
            let targetFiles = if canShare planData (Z.packageId pInfo) then archiveFiles else scopedArchiveFiles

            forM_ targetFiles $ \targetFile -> do
              archiveFileExists <- runResourceT $ IO.resourceExists envAws targetFile

              unless archiveFileExists $ do
                packageStorePathExists <- doesDirectoryExist packageStorePath

                when packageStorePathExists $ void $ runExceptT $ IO.exceptWarn $ do
                  let workingStorePackagePath = tempPath </> Z.packageDir pInfo
                  liftIO $ IO.createDirectoryIfMissing True workingStorePackagePath

                  let rp2 = Z.relativePaths storePath pInfo

                  CIO.putStrLn $ "Creating " <> toText targetFile

                  let tempArchiveFile = tempPath </> archiveFileBasename

                  metas <- createMetadata tempPath pInfo [("store-path", LC8.pack storePath)]

                  IO.createTar tempArchiveFile (rp2 <> [metas])

                  void $ catchError (liftIO (LBS.readFile tempArchiveFile) >>= IO.writeResource envAws targetFile) $ \case
                    e@(AwsAppError (HTTP.Status 301 _)) -> do
                      liftIO $ STM.atomically $ STM.writeTVar tEarlyExit True
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "ERROR: No write access to archive uris: "
                        <> tshow [toText <$> scopedArchiveFiles, toText <$> archiveFiles]
                        <> " " <> displayAppError e

                    _ -> return ()

    Left (appError :: AppError) -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  earlyExit <- STM.readTVarIO tEarlyExit

  when earlyExit $ CIO.hPutStrLn IO.stderr $ "Early exit due to error"

isShareable :: MonadIO m => FilePath -> Z.PackageInfo -> m Bool
isShareable storePath pkg =
  let packageSharePath = storePath </> Z.packageDir pkg </> "share"
  in IO.listMaybeDirectory packageSharePath <&> (\\ ["doc"]) <&> null

cmdSyncToArchive :: Mod CommandFields (IO ())
cmdSyncToArchive = command "sync-to-archive"  $ flip info idm $ runSyncToArchive <$> optsSyncToArchive
