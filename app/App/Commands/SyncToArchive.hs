{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive,
  ) where

import App.Commands.Options.Parser      (optsPackageIds, text)
import App.Commands.Options.Types       (SyncToArchiveOptions (SyncToArchiveOptions))
import Control.Concurrent.STM           (TVar)
import Control.Lens                     ((^..), (^.), (.~), (%~), Each(each))
import Data.Generics.Product.Any        (the)
import Data.List                        ((\\))
import HaskellWorks.CabalCache.AppError (AwsError, HttpError (..), displayAwsError, displayHttpError)
import HaskellWorks.CabalCache.Error    (DecodeError, ExitFailure(..), InvalidUrl(..), NotImplemented(..), UnsupportedUri(..))
import HaskellWorks.CabalCache.Location (Location (..), toLocation, (<.>), (</>))
import HaskellWorks.CabalCache.IO.Tar   (ArchiveError)
import HaskellWorks.CabalCache.Metadata (createMetadata)
import HaskellWorks.CabalCache.Topology (buildPlanData, canShare)
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
import qualified Control.Monad.Oops                 as OO
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Text                          as Text
import qualified HaskellWorks.CabalCache.AWS.Env    as AWS
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
import qualified System.IO.Unsafe                   as IO
import qualified UnliftIO.Async                     as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

data WorkResult = WorkSkipped | WorkFatal
  deriving (Eq, Show)

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  tEarlyExit <- STM.newTVarIO False

  OO.runOops $ OO.catchAndExitFailure @ExitFailure do
    let mHostEndpoint       = opts ^. the @"hostEndpoint"
    let storePath           = opts ^. the @"storePath"
    let archiveUri          = opts ^. the @"archiveUri"
    let threads             = opts ^. the @"threads"
    let awsLogLevel         = opts ^. the @"awsLogLevel"
    let versionedArchiveUri = archiveUri </> archiveVersion
    let storePathHash       = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
    let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash
    let maxRetries          = opts ^. the @"maxRetries"
    let ignorePackages      = opts ^. the @"ignorePackages"

    CIO.putStrLn $ "Store path: "       <> AWS.toText storePath
    CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
    CIO.putStrLn $ "Archive URI: "      <> AWS.toText archiveUri
    CIO.putStrLn $ "Archive version: "  <> archiveVersion
    CIO.putStrLn $ "Threads: "          <> tshow threads
    CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

    planJson <- Z.loadPlan (opts ^. the @"path" </> opts ^. the @"buildPath")
      & do OO.catch @DecodeError \e -> do
            CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> tshow e
            OO.throw ExitFailure

    compilerContext <- Z.mkCompilerContext planJson
      & do OO.catch @Text \e -> do
            CIO.hPutStrLn IO.stderr e
            OO.throw ExitFailure

    let compilerId = planJson ^. the @"compilerId"

    envAws <-
      liftIO (IO.unsafeInterleaveIO (AWS.mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)))
        <&> case mHostEndpoint of
              Just (host, port, ssl) ->
                \env ->
                  env
                    & the @"overrides" .~ \svc ->
                        svc & the @"endpoint" %~ \mkEndpoint region ->
                          mkEndpoint region
                            & the @"host"   .~ host
                            & the @"port"   .~ port
                            & the @"secure" .~ ssl
              Nothing -> id

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

    IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
      CIO.putStrLn $ "Temp path: " <> tshow tempPath

      liftIO $ IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
        OO.runOops $ workLoop tEarlyExit do
          let archiveFileBasename = Z.packageDir pInfo <.> ".tar.gz"
          let archiveFile         = versionedArchiveUri </> T.pack archiveFileBasename
          let scopedArchiveFile   = versionedArchiveUri </> T.pack storePathHash </> T.pack archiveFileBasename
          let packageStorePath    = storePath </> Z.packageDir pInfo
          let packageName         = pInfo ^. the @"packageName"

          when (packageName `S.member` ignorePackages) do
            CIO.hPutStrLn IO.stderr $ "Ignoring package: " <> packageName
            OO.throw WorkSkipped

          -- either write "normal" package, or a user-specific one if the package cannot be shared
          let targetFile = if canShare planData (Z.packageId pInfo) then archiveFile else scopedArchiveFile

          archiveFileExists <- IO.resourceExists envAws targetFile
            & do OO.catch @InvalidUrl \(InvalidUrl url' reason') -> do
                  CIO.hPutStrLn IO.stderr $ "Invalid URL: " <> tshow url' <> ", " <> reason'
                  OO.throw WorkSkipped
            & do OO.catch @UnsupportedUri \e -> do
                  CIO.hPutStrLn IO.stderr $ "Unsupported URI: " <> tshow e
                  OO.throw WorkSkipped

          unless archiveFileExists do
            packageStorePathExists <- liftIO $ doesDirectoryExist packageStorePath

            when packageStorePathExists do
              let workingStorePackagePath = tempPath </> Z.packageDir pInfo
              liftIO $ IO.createDirectoryIfMissing True workingStorePackagePath

              let rp2 = Z.relativePaths storePath pInfo

              CIO.putStrLn $ "Creating " <> AWS.toText targetFile

              let tempArchiveFile = tempPath </> archiveFileBasename

              metas <- createMetadata tempPath pInfo [("store-path", LC8.pack storePath)]

              IO.createTar tempArchiveFile (rp2 <> [metas])
                & do OO.catch @ArchiveError \_ -> do
                      CIO.hPutStrLn IO.stderr $ "Unable tar " <> tshow tempArchiveFile
                      OO.throw WorkSkipped

              (liftIO (LBS.readFile tempArchiveFile) >>= IO.writeResource envAws targetFile maxRetries)
                & do OO.catch @AwsError \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "ERROR: No write access to archive uris: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> displayAwsError e
                      OO.throw WorkFatal
                & do OO.catch @HttpError \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "ERROR: No write access to archive uris: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> displayHttpError e
                      OO.throw WorkFatal
                & do OO.catch @NotImplemented \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "Operation not implemented: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> " " <> tshow e
                      OO.throw WorkFatal
                & do OO.catch @UnsupportedUri \e -> do
                      CIO.hPutStrLn IO.stderr $ mempty
                        <> "Unsupported URI: "
                        <> tshow (fmap AWS.toText [scopedArchiveFile, archiveFile])
                        <> ": " <> tshow e
                      OO.throw WorkFatal
              
    return ()

  earlyExit <- STM.readTVarIO tEarlyExit

  when earlyExit $ CIO.hPutStrLn IO.stderr "Early exit due to error"

workLoop :: ()
  => MonadIO m
  => TVar Bool
  -> ExceptT (OO.Variant (WorkResult : e')) m ()
  -> ExceptT (OO.Variant e') m ()
workLoop tEarlyExit f = do
  earlyExit <- liftIO $ STM.readTVarIO tEarlyExit

  unless earlyExit do
    f & OO.catch @WorkResult \case
      WorkSkipped -> pure ()
      WorkFatal   -> liftIO $ STM.atomically $ STM.writeTVar tEarlyExit True

isShareable :: MonadIO m => FilePath -> Z.PackageInfo -> m Bool
isShareable storePath pkg =
  let packageSharePath = storePath </> Z.packageDir pkg </> "share"
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
      <>  OA.value (Local $ takeDirectory AS.cabalStoreDirectory </> "archive")
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
