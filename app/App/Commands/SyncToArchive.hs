{- HLINT ignore "Functor law" -}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive,
  ) where

import App.Amazonka
import App.Commands.Options.Parser      (optsPackageIds, text)
import App.Commands.Options.Types       (SyncToArchiveOptions (SyncToArchiveOptions))
import App.Run
import Control.Lens                     ((^..), Each(each))
import Data.Generics.Product.Any        (the)
import Data.List                        ((\\))
import Effectful
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Lazy.Dynamic
import HaskellWorks.CabalCache.AppError (AwsStatusError, HttpError (..), displayAwsStatusError, displayHttpError)
import HaskellWorks.CabalCache.Error    (DecodeError, ExitFailure(..), InvalidUrl(..), NotImplemented(..), UnsupportedUri(..))
import HaskellWorks.CabalCache.IO.Tar   (ArchiveError)
import HaskellWorks.CabalCache.Location (Location (..), toLocation, (<.>), (</>))
import HaskellWorks.CabalCache.Metadata (createMetadata)
import HaskellWorks.CabalCache.Topology (buildPlanData, canShare)
import HaskellWorks.CabalCache.Types
import HaskellWorks.CabalCache.Version  (archiveVersion)
import HaskellWorks.Prelude
import Options.Applicative              (Parser, Mod, CommandFields)
import System.Directory                 (doesDirectoryExist)
import System.FilePath                  (takeDirectory)

import qualified Amazonka                           as AWS
import qualified Amazonka.Data                      as AWS
import qualified App.Commands.Options.Types         as Z
import qualified App.Static                         as AS
import qualified Control.Concurrent.STM             as STM
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Text                          as Text
import qualified HaskellWorks.CabalCache.Core       as Z
import qualified HaskellWorks.CabalCache.GhcPkg     as GhcPkg
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.IO.File    as IO
import qualified HaskellWorks.CabalCache.IO.Lazy    as IO
import qualified HaskellWorks.CabalCache.IO.Tar     as IO
import qualified Options.Applicative                as OA
import qualified System.Directory                   as IO
import qualified System.IO                          as IO
import qualified System.IO.Temp                     as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

data WorkResult = WorkSkipped | WorkFatal
  deriving (Eq, Show)

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = runApp do
  tEarlyExit <- newTVarIO False

  let mHostEndpoint       = opts.hostEndpoint
  let storePath           = opts.storePath
  let archiveUri          = opts.archiveUri
  let threads             = opts.threads
  let awsLogLevel         = opts.awsLogLevel
  let versionedArchiveUri = archiveUri </> archiveVersion
  let storePathHash       = opts.storePathHash & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash
  let maxRetries          = opts.maxRetries
  let ignorePackages      = opts.ignorePackages

  CIO.putStrLn $ "Store path: "       <> AWS.toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
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

  let compilerId = planJson.compilerId

  let archivePath       = versionedArchiveUri </> compilerId
  let scopedArchivePath = scopedArchiveUri </> compilerId

  liftIO $ IO.createLocalDirectoryIfMissing archivePath
  liftIO $ IO.createLocalDirectoryIfMissing scopedArchivePath

  packages <- liftIO $ Z.getPackages storePath planJson

  nonShareable <- packages & filterM (fmap not . isShareable storePath)

  let planData = buildPlanData planJson (nonShareable ^.. each . the @"packageId")

  let storeCompilerPath           = storePath </> T.unpack compilerId
  let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"

  storeCompilerPackageDbPathExists <- liftIO $ doesDirectoryExist storeCompilerPackageDbPath

  unless storeCompilerPackageDbPathExists $
    liftIO $ GhcPkg.contextInit compilerContext storeCompilerPackageDbPath

  CIO.putStrLn $ "Syncing " <> tshow (length packages) <> " packages"

  runLazy (mkAwsEnv opts.region mHostEndpoint awsLogLevel) do
    IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
      CIO.putStrLn $ "Temp path: " <> tshow tempPath

      pooledForConcurrentlyN_ opts.threads packages $ \pInfo -> do
        workLoop tEarlyExit do
          let archiveFileBasename = pInfo.packageDir <.> ".tar.gz"
          let archiveFile         = versionedArchiveUri </> T.pack archiveFileBasename
          let scopedArchiveFile   = versionedArchiveUri </> T.pack storePathHash </> T.pack archiveFileBasename
          let packageStorePath    = storePath </> pInfo.packageDir
          let packageName         = pInfo.packageName

          when (packageName `S.member` ignorePackages) do
            CIO.hPutStrLn IO.stderr $ "Ignoring package: " <> packageName
            throw WorkSkipped

          -- either write "normal" package, or a user-specific one if the package cannot be shared
          let targetFile = if canShare planData pInfo.packageId then archiveFile else scopedArchiveFile

          archiveFileExists <- IO.resourceExists targetFile
            & do trap @InvalidUrl \(InvalidUrl url' reason') -> do
                  CIO.hPutStrLn IO.stderr $ "Invalid URL: " <> tshow url' <> ", " <> reason'
                  throw WorkSkipped
            & do trap @UnsupportedUri \e -> do
                  CIO.hPutStrLn IO.stderr $ "Unsupported URI: " <> tshow e
                  throw WorkSkipped
            & do trap @AwsError \e -> do
                  CIO.hPutStrLn IO.stderr $ "Unsupported URI: " <> tshow e
                  throw WorkSkipped
            & do trap @AwsStatusError \e -> do
                  CIO.hPutStrLn IO.stderr $ "Unsupported URI: " <> tshow e
                  throw WorkSkipped

          unless archiveFileExists do
            packageStorePathExists <- liftIO $ doesDirectoryExist packageStorePath

            when packageStorePathExists do
              let workingStorePackagePath = tempPath </> pInfo.packageDir
              liftIO $ IO.createDirectoryIfMissing True workingStorePackagePath

              let rp2 = Z.relativePaths storePath pInfo

              CIO.putStrLn $ "Creating " <> AWS.toText targetFile

              let tempArchiveFile = tempPath </> archiveFileBasename

              metas <- createMetadata tempPath pInfo [("store-path", LC8.pack storePath)]

              IO.createTar tempArchiveFile (rp2 <> [metas])
                & do trap @ArchiveError \_ -> do
                      CIO.hPutStrLn IO.stderr $ "Unable tar " <> tshow tempArchiveFile
                      throw WorkSkipped

              (liftIO (LBS.readFile tempArchiveFile) >>= IO.writeResource targetFile maxRetries)
                & do trap @AwsError \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "ERROR: No write access to archive uris: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> tshow e
                      throw WorkFatal
                & do trap @AwsStatusError \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "ERROR: No write access to archive uris: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> displayAwsStatusError e
                      throw WorkFatal
                & do trap @HttpError \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "ERROR: No write access to archive uris: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> displayHttpError e
                      throw WorkFatal
                & do trap @NotImplemented \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "Operation not implemented: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> tshow e
                      throw WorkFatal
                & do trap @UnsupportedUri \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "Unsupported URI: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> ": " <> tshow e
                      throw WorkFatal

    earlyExit <- readTVarIO tEarlyExit

    when earlyExit $ CIO.hPutStrLn IO.stderr "Early exit due to error"

workLoop :: ()
  => r <: Concurrent
  => TVar Bool
  -> Eff (Error WorkResult : r) ()
  -> Eff r ()
workLoop tEarlyExit f = do
  earlyExit <- readTVarIO tEarlyExit

  unless earlyExit do
    f & trap @WorkResult \case
      WorkSkipped -> pure ()
      WorkFatal   -> atomically $ STM.writeTVar tEarlyExit True

isShareable :: MonadIO m => FilePath -> Z.PackageInfo -> m Bool
isShareable storePath pkg =
  let packageSharePath = storePath </> pkg.packageDir </> "share"
  in IO.listMaybeDirectory packageSharePath <&> (\\ ["doc"]) <&> null

optsSyncToArchive :: Parser SyncToArchiveOptions
optsSyncToArchive = SyncToArchiveOptions
  <$> OA.option (OA.auto <|> text)
      (  OA.long "region"
      <> OA.metavar "AWS_REGION"
      <> OA.showDefault
      <> OA.value AWS.Oregon
      <> OA.help "The AWS region in which to operate"
      )
  <*> OA.option (OA.maybeReader (toLocation . Text.pack))
      (   OA.long "archive-uri"
      <>  OA.help "Archive URI to sync to"
      <>  OA.metavar "S3_URI"
      <>  OA.value (LocalFile $ takeDirectory AS.cabalStoreDirectory </> "archive")
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
      <>  OA.help "Path to cabal store"
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
  <$>  OA.option (OA.eitherReader (AWS.fromText . T.pack))
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

cmdSyncToArchive :: Mod CommandFields (IO ())
cmdSyncToArchive = OA.command "sync-to-archive" $ flip OA.info OA.idm $ runSyncToArchive <$> optsSyncToArchive
