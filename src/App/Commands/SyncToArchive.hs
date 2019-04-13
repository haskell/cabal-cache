{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive
  ) where

import Antiope.Core                         (toText)
import Antiope.Env                          (LogLevel, mkEnv)
import App.Commands.Options.Parser          (optsSyncToArchive)
import App.Static                           (homeDirectory)
import Control.Lens                         hiding ((<.>))
import Control.Monad                        (when)
import Control.Monad.Trans.Resource         (runResourceT)
import Data.Generics.Product.Any            (the)
import Data.Semigroup                       ((<>))
import HaskellWorks.Ci.Assist.Core          (PackageInfo (..), getPackages, loadPlan, relativePaths)
import HaskellWorks.Ci.Assist.Location      ((<.>), (</>))
import HaskellWorks.Ci.Assist.PackageConfig (templateConfig)
import HaskellWorks.Ci.Assist.Tar           (updateEntryWith)
import Options.Applicative                  hiding (columns)
import System.Directory                     (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types        as Z
import qualified Codec.Archive.Tar                 as F
import qualified Codec.Compression.GZip            as F
import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified HaskellWorks.Ci.Assist.IO.Lazy    as IO
import qualified HaskellWorks.Ci.Assist.Types      as Z
import qualified System.IO                         as IO
import qualified System.Process                    as IO
import qualified UnliftIO.Async                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  let archiveUri = opts ^. the @"archiveUri"
  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      envAws <- mkEnv (opts ^. the @"region") (\_ _ -> pure ())
      let archivePath = homeDirectory </> ".cabal" </> "archive" </> (planJson ^. the @"compilerId" . to T.unpack)
      createDirectoryIfMissing True archivePath
      let baseDir = opts ^. the @"storePath"
      packages <- getPackages baseDir planJson

      let storeCompilerPath           = baseDir <> "/" <> (planJson ^. the @"compilerId")
      let storeCompilerPackageDbPath  = storeCompilerPath <> "/package.db"

      storeCompilerPackageDbPathExists <- IO.doesDirectoryExist (T.unpack storeCompilerPackageDbPath)

      unless storeCompilerPackageDbPathExists $ do
        CIO.putStrLn "Package DB missing.  Creating Package DB"
        hGhcPkg <- IO.spawnProcess "ghc-pkg" ["init", T.unpack storeCompilerPackageDbPath]

        exitCodeGhcPkg <- IO.waitForProcess hGhcPkg
        case exitCodeGhcPkg of
          IO.ExitFailure _ -> do
            CIO.hPutStrLn IO.stderr "ERROR: Failed to create Package DB"
            IO.exitWith (IO.ExitFailure 1)
          _ -> return ()

      IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
        let archiveFile = archiveUri </> T.pack (packageDir pInfo) <.> ".tar.gz"
        let packageStorePath = baseDir </> packageDir pInfo
        packageStorePathExists <- doesDirectoryExist packageStorePath
        archiveFileExists <- runResourceT $ IO.resourceExists envAws archiveFile
        when (not archiveFileExists && packageStorePathExists) $ do
          CIO.putStrLn $ "Creating " <> toText archiveFile
          entries <- F.pack baseDir (relativePaths pInfo)

          let entries' = case confPath pInfo of
                          Nothing   -> entries
                          Just conf -> updateEntryWith (== conf) (templateConfig baseDir) <$> entries

          IO.writeResource envAws archiveFile . F.compress . F.write $ entries

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

cmdSyncToArchive :: Mod CommandFields (IO ())
cmdSyncToArchive = command "sync-to-archive"  $ flip info idm $ runSyncToArchive <$> optsSyncToArchive
