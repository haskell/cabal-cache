{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core                         (runResAws)
import Antiope.Env                          (LogLevel, mkEnv)
import App.Commands.Options.Parser          (optsSyncFromArchive)
import App.Static                           (homeDirectory)
import Control.Lens
import Control.Monad                        (when)
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Trans.Resource         (runResourceT)
import Data.Generics.Product.Any            (the)
import Data.Semigroup                       ((<>))
import HaskellWorks.Ci.Assist.Core
import HaskellWorks.Ci.Assist.Options
import HaskellWorks.Ci.Assist.PackageConfig (unTemplateConfig)
import HaskellWorks.Ci.Assist.Tar           (mapEntriesWith)
import Network.AWS.Types                    (Region (Oregon))
import Options.Applicative                  hiding (columns)
import System.FilePath                      ((</>))

import qualified App.Commands.Options.Types        as Z
import qualified Codec.Archive.Tar                 as F
import qualified Codec.Compression.GZip            as F
import qualified Control.Monad.Trans.AWS           as AWS
import qualified Data.ByteString.Lazy.Char8        as LBSC
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import qualified HaskellWorks.Ci.Assist.GhcPkg     as GhcPkg
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified HaskellWorks.Ci.Assist.IO.Lazy    as IO
import qualified HaskellWorks.Ci.Assist.Types      as Z
import qualified System.Directory                  as IO
import qualified System.Exit                       as IO
import qualified System.IO                         as IO
import qualified System.Process                    as IO
import qualified UnliftIO.Async                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

logger :: LogLevel -> LBSC.ByteString -> IO ()
logger _ _ = return ()

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let archiveUri = opts ^. the @"archiveUri"
  CIO.putStrLn $ "Archive URI: " <> archiveUri

  GhcPkg.testAvailability

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      env <- mkEnv (opts ^. the @"region") logger
      let archivePath                 = archiveUri <> "/" <> (planJson ^. the @"compilerId")
      let baseDir                     = opts ^. the @"storePath"
      let storeCompilerPath           = baseDir </> (planJson ^. the @"compilerId" . to T.unpack)
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
      let storeCompilerLibPath        = storeCompilerPath </> "lib"

      IO.putStrLn "Creating store directories"
      IO.createDirectoryIfMissing True baseDir
      IO.createDirectoryIfMissing True storeCompilerPath
      IO.createDirectoryIfMissing True storeCompilerLibPath

      packages <- getPackages baseDir planJson

      IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
        let archiveFile = archiveUri <> "/" <> T.pack (packageDir pInfo) <> ".tar.gz"
        let packageStorePath = baseDir </> packageDir pInfo
        storeDirectoryExists <- IO.doesDirectoryExist packageStorePath
        arhiveFileExists <- runResourceT $ IO.resourceExists env archiveFile
        when (not storeDirectoryExists && arhiveFileExists) $ do
          runResAws env $ do
            maybeArchiveFileContents <- IO.readResource env archiveFile
            case maybeArchiveFileContents of
              Just archiveFileContents -> do
                liftIO $ CIO.putStrLn $ "Extracting " <> archiveFile
                let entries = F.read (F.decompress archiveFileContents)
                let entries' = case confPath pInfo of
                                  Nothing   -> entries
                                  Just conf -> mapEntriesWith (== conf) (unTemplateConfig baseDir) entries

                liftIO $ F.unpack baseDir entries'
              Nothing -> do
                liftIO $ CIO.putStrLn $ "Archive unavilable: " <> archiveFile

      CIO.putStrLn "Recaching package database"
      GhcPkg.recache storeCompilerPackageDbPath

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-archive"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
