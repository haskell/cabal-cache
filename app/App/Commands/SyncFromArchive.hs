{- HLINT ignore "Redundant id"   -}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive,
  ) where

import App.Amazonka
import App.Commands.Options.Parser      (optsPackageIds, text)
import App.Commands.Options.Types       (SyncFromArchiveOptions (SyncFromArchiveOptions))
import App.Run
import Control.Lens                     ((^..), (%~), Each(each))
import Control.Lens.Combinators         (traverse1)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.List.NonEmpty               (NonEmpty)
import Effectful
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Lazy.Dynamic
import HaskellWorks.CabalCache.AppError (AwsStatusError, HttpError (..), displayAwsStatusError, displayHttpError)
import HaskellWorks.CabalCache.Concurrent.Fork
import HaskellWorks.CabalCache.Concurrent.Type
import HaskellWorks.CabalCache.Error    (DecodeError(..), ExitFailure(..), InvalidUrl(..), NotFound, UnsupportedUri(..))
import HaskellWorks.CabalCache.IO.Lazy  (readFirstAvailableResource)
import HaskellWorks.CabalCache.IO.Tar   (ArchiveError(..))
import HaskellWorks.CabalCache.Location (toLocation, (<.>), (</>), Location)
import HaskellWorks.CabalCache.Metadata (loadMetadata)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import HaskellWorks.Prelude
import Options.Applicative              (CommandFields, Mod, Parser)
import Options.Applicative.NonEmpty     (some1)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)

import qualified Amazonka                                         as AWS
import qualified Amazonka.Data                                    as AWS
import qualified App.Commands.Options.Types                       as Z
import qualified App.Static                                       as AS
import qualified Control.Concurrent.STM                           as STM
import qualified Data.ByteString.Char8                            as C8
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.List.NonEmpty                               as NEL
import qualified Data.Map                                         as M
import qualified Data.Map.Strict                                  as Map
import qualified Data.Set                                         as S
import qualified Data.Text                                        as T
import qualified HaskellWorks.CabalCache.Concurrent.DownloadQueue as DQ
import qualified HaskellWorks.CabalCache.Core                     as Z
import qualified HaskellWorks.CabalCache.Data.List                as L
import qualified HaskellWorks.CabalCache.GhcPkg                   as GhcPkg
import qualified HaskellWorks.CabalCache.Hash                     as H
import qualified HaskellWorks.CabalCache.IO.Console               as CIO
import qualified HaskellWorks.CabalCache.IO.Tar                   as IO
import qualified HaskellWorks.CabalCache.Store                    as M
import qualified HaskellWorks.CabalCache.Types                    as Z
import qualified Options.Applicative                              as OA
import qualified System.Directory                                 as IO
import qualified System.IO                                        as IO
import qualified System.IO.Temp                                   as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

skippable :: Z.Package -> Bool
skippable package = package.packageType == "pre-existing"

recoverOrVoid :: forall x r. ()
  => Eff (Error x : r)  Void
  -> Eff r x
recoverOrVoid f =
  f & fmap absurd
    & trap pure

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = runApp do
  let mHostEndpoint         = opts.hostEndpoint :: Maybe (ByteString, Int, Bool)
  let storePath             = opts.storePath
  let archiveUris           = opts.archiveUris :: NonEmpty Location
  let threads               = opts.threads
  let awsLogLevel           = opts.awsLogLevel
  let versionedArchiveUris  = archiveUris & traverse1 %~ (</> archiveVersion) :: NonEmpty Location
  let storePathHash         = opts.storePathHash & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUris     = versionedArchiveUris & traverse1 %~ (</> T.pack storePathHash)
  let maxRetries            = opts.maxRetries
  let ignorePackages        = opts.ignorePackages

  CIO.putStrLn $ "Store path: "       <> AWS.toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  forM_ archiveUris $ \archiveUri -> do
    CIO.putStrLn $ "Archive URI: "      <> AWS.toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  planJson <- Z.loadPlan (opts.path </> opts.buildPath)
    & do trap @DecodeError \e -> do
          CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> tshow e
          throw ExitFailure

  compilerContext <- Z.mkCompilerContext planJson
    & do trap @Text \e -> do
          CIO.hPutStrLn IO.stderr e
          throw ExitFailure

  liftIO $ GhcPkg.testAvailability compilerContext

  let compilerId                  = planJson.compilerId
  let storeCompilerPath           = storePath </> T.unpack compilerId
  let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
  let storeCompilerLibPath        = storeCompilerPath </> "lib"

  CIO.putStrLn "Creating store directories"
  liftIO $ createDirectoryIfMissing True storePath
  liftIO $ createDirectoryIfMissing True storeCompilerPath
  liftIO $ createDirectoryIfMissing True storeCompilerLibPath

  storeCompilerPackageDbPathExists <- liftIO $ doesDirectoryExist storeCompilerPackageDbPath

  unless storeCompilerPackageDbPathExists do
    CIO.putStrLn "Package DB missing. Creating Package DB"
    liftIO $ GhcPkg.contextInit compilerContext storeCompilerPackageDbPath

  packages <- liftIO $ Z.getPackages storePath planJson

  let installPlan = planJson.installPlan
  let planPackages = M.fromList $ fmap (\p -> (p.id, p)) installPlan

  let planDeps0 = installPlan >>= \p -> fmap (p.id, ) $ mempty
        <> p.depends
        <> p.exeDepends
        <> (p ^.. the @"components" . each . the @"lib" . each . the @"depends"    . each)
        <> (p ^.. the @"components" . each . the @"lib" . each . the @"exeDepends" . each)
  let planDeps  = planDeps0 <> fmap (\p -> ("[universe]", p.id)) installPlan

  downloadQueue <- liftIO $ STM.atomically $ DQ.createDownloadQueue planDeps

  let pInfos = M.fromList $ fmap (\p -> (p.packageId, p)) packages

  runLazy (mkAwsEnv opts.region mHostEndpoint awsLogLevel) do
    IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
      liftIO $ IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

      forkThreadsWait threads $ DQ.runQueue downloadQueue $ \packageId -> do
        recoverOrVoid @DQ.DownloadStatus do
          pInfo <- pure (M.lookup packageId pInfos)
            & do onNothingM do
                  CIO.hPutStrLn IO.stderr $ "Warning: Invalid package id: " <> packageId
                  DQ.downloadSucceed

          let archiveBaseName     = pInfo.packageDir <.> ".tar.gz"
          let archiveFiles        = versionedArchiveUris & traverse1 %~ (</> T.pack archiveBaseName)
          let scopedArchiveFiles  = scopedArchiveUris & traverse1 %~ (</> T.pack archiveBaseName)
          let packageStorePath    = storePath </> pInfo.packageDir
          let packageName         = pInfo.packageName

          storeDirectoryExists <- liftIO $ doesDirectoryExist packageStorePath

          package <- pure (M.lookup packageId planPackages)
            & do onNothingM do
                  CIO.hPutStrLn IO.stderr $ "Warning: package not found" <> packageName
                  DQ.downloadSucceed

          when (skippable package) do
            CIO.putStrLn $ "Skipping: " <> packageName
            DQ.downloadSucceed

          when (packageName `S.member` ignorePackages) do
            CIO.putStrLn $ "Ignoring: " <> packageName
            DQ.downloadFail

          when storeDirectoryExists DQ.downloadSucceed

          ensureStorePathCleanup packageStorePath do
            let locations = sconcat $ fmap L.tuple2ToNel (NEL.zip archiveFiles scopedArchiveFiles)

            (existingArchiveFileContents, existingArchiveFile) <- readFirstAvailableResource locations maxRetries
              & do trap @AwsError \e -> do
                    CIO.putStrLn $ "Unable to download any of: " <> tshow locations <> " because: " <> tshow e
                    DQ.downloadFail
              & do trap @AwsStatusError \e -> do
                    CIO.putStrLn $ "Unable to download any of: " <> tshow locations <> " because: " <> displayAwsStatusError e
                    DQ.downloadFail
              & do trap @HttpError \e -> do
                    CIO.putStrLn $ "Unable to download any of: " <> tshow locations <> " because: " <> displayHttpError e
                    DQ.downloadFail
              & do trap @NotFound \_ -> do
                    CIO.putStrLn $ "Not found: " <> tshow locations
                    DQ.downloadFail
              & do trap @InvalidUrl \(InvalidUrl url' reason') -> do
                    CIO.hPutStrLn IO.stderr $ "Invalid URL: " <> tshow url' <> ", " <> reason'
                    DQ.downloadFail
              & do trap @UnsupportedUri \e -> do
                    CIO.hPutStrLn IO.stderr $ tshow e
                    DQ.downloadFail

            CIO.putStrLn $ "Extracting: " <> AWS.toText existingArchiveFile

            let tempArchiveFile = tempPath </> archiveBaseName
            liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents

            IO.extractTar tempArchiveFile storePath
              & do trap @ArchiveError \(ArchiveError reason') -> do
                    CIO.putStrLn $ "Unable to extract tar at " <> tshow tempArchiveFile <> " because: " <> reason'
                    DQ.downloadFail

            meta <- loadMetadata packageStorePath
            oldStorePath <- pure (Map.lookup "store-path" meta)
              & do onNothingM do
                    CIO.putStrLn "store-path is missing from Metadata"
                    DQ.downloadFail

            let Z.Tagged conf _ = pInfo.confPath
            
            let theConfPath = storePath </> conf
            let tempConfPath = tempPath </> conf
            confPathExists <- liftIO $ IO.doesFileExist theConfPath
            when confPathExists do
              confContents <- liftIO $ LBS.readFile theConfPath
              liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
              liftIO $ IO.copyFile tempConfPath theConfPath >> IO.removeFile tempConfPath

            DQ.downloadSucceed

  CIO.putStrLn "Recaching package database"

  liftIO $ GhcPkg.recache compilerContext storeCompilerPackageDbPath

  failures <- liftIO $ STM.atomically $ STM.readTVar downloadQueue.tFailures

  forM_ failures $ \packageId -> CIO.hPutStrLn IO.stderr $ "Failed to download: " <> packageId

ensureStorePathCleanup :: ()
  => r <: Error DQ.DownloadStatus
  => r <: IOE
  => FilePath
  -> Eff r a
  -> Eff r a
ensureStorePathCleanup packageStorePath = 
  trapIn @DQ.DownloadStatus \downloadStatus -> do
    case downloadStatus of
      DQ.DownloadFailure -> M.cleanupStorePath packageStorePath
      DQ.DownloadSuccess ->
        CIO.hPutStrLn IO.stdout $ "Successfully cleaned up store path: " <> tshow packageStorePath
    throw downloadStatus

optsSyncFromArchive :: Parser SyncFromArchiveOptions
optsSyncFromArchive = SyncFromArchiveOptions
  <$> OA.option (OA.auto <|> text)
      (  OA.long "region"
      <> OA.metavar "AWS_REGION"
      <> OA.showDefault
      <> OA.value AWS.Oregon
      <> OA.help "The AWS region in which to operate"
      )
  <*> some1
      (  OA.option (OA.maybeReader (toLocation . T.pack))
        (   OA.long "archive-uri"
        <>  OA.help "Archive URI to sync to"
        <>  OA.metavar "S3_URI"
        )
      )
  <*> OA.strOption
      (   OA.long "path"
      <>  OA.help "Path to cabal project directory.  Defaults to \".\""
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.path
      )
  <*> OA.strOption
      (   OA.long "build-path"
      <>  OA.help ("Path to cabal build directory.  Defaults to " <> show AS.buildPath)
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.buildPath
      )
  <*> OA.strOption
      (   OA.long "store-path"
      <>  OA.help ("Path to cabal store.  Defaults to " <> show AS.cabalStoreDirectory)
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.cabalStoreDirectory
      )
  <*> optional
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
  <*> optional
      ( OA.option (OA.eitherReader (AWS.fromText . T.pack))
        (   OA.long "aws-log-level"
        <>  OA.help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
        <>  OA.metavar "AWS_LOG_LEVEL"
        )
      )
  <*> optional parseEndpoint
  <*> OA.option OA.auto
      (   OA.long "max-retries"
      <>  OA.help "Max retries for S3 requests"
      <>  OA.metavar "NUM_RETRIES"
      <>  OA.value 3
      )
  <*> optsPackageIds

parseEndpoint :: Parser (ByteString, Int, Bool)
parseEndpoint =
  (,,)
  <$> OA.option (OA.eitherReader (AWS.fromText . T.pack))
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
cmdSyncFromArchive = OA.command "sync-from-archive" $ flip OA.info OA.idm $ runSyncFromArchive <$> optsSyncFromArchive
