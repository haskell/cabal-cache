{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive,
  ) where

import App.Commands.Options.Parser      (optsPackageIds, text)
import App.Commands.Options.Types       (SyncFromArchiveOptions (SyncFromArchiveOptions))
import Control.Applicative              (optional, Alternative(..))
import Control.Lens                     ((^..), (.~), (<&>), (%~), (&), (^.), Each(each))
import Control.Lens.Combinators         (traverse1)
import Control.Monad                    (when, unless, forM_)
import Control.Monad.Catch              (MonadCatch)
import Control.Monad.Except             (ExceptT)
import Control.Monad.IO.Class           (MonadIO(..))
import Control.Monad.Trans.AWS          (envOverride, setEndpoint)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString                  (ByteString)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.List.NonEmpty               (NonEmpty)
import Data.Maybe                       (fromMaybe)
import Data.Monoid                      (Dual(Dual), Endo(Endo))
import Data.Semigroup                   (Semigroup(..))
import Data.Text                        (Text)
import HaskellWorks.CabalCache.AppError (AwsError, HttpError (..))
import HaskellWorks.CabalCache.Error    (DecodeError(..), ExitFailure(..), InvalidUrl(..), NotFound, UnsupportedUri(..))
import HaskellWorks.CabalCache.IO.Lazy  (readFirstAvailableResource)
import HaskellWorks.CabalCache.IO.Tar   (ArchiveError(..))
import HaskellWorks.CabalCache.Location (toLocation, (<.>), (</>), Location)
import HaskellWorks.CabalCache.Metadata (loadMetadata)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              (CommandFields, Mod, Parser)
import Options.Applicative.NonEmpty     (some1)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types                       as Z
import qualified App.Static                                       as AS
import qualified Control.Concurrent.STM                           as STM
import qualified Control.Monad.Oops                               as OO
import qualified Data.ByteString.Char8                            as C8
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.List.NonEmpty                               as NEL
import qualified Data.Map                                         as M
import qualified Data.Map.Strict                                  as Map
import qualified Data.Set                                         as S
import qualified Data.Text                                        as T
import qualified HaskellWorks.CabalCache.AWS.Env                  as AWS
import qualified HaskellWorks.CabalCache.Concurrent.DownloadQueue as DQ
import qualified HaskellWorks.CabalCache.Concurrent.Fork          as IO
import qualified HaskellWorks.CabalCache.Core                     as Z
import qualified HaskellWorks.CabalCache.Data.List                as L
import qualified HaskellWorks.CabalCache.GhcPkg                   as GhcPkg
import qualified HaskellWorks.CabalCache.Hash                     as H
import qualified HaskellWorks.CabalCache.IO.Console               as CIO
import qualified HaskellWorks.CabalCache.IO.Tar                   as IO
import qualified HaskellWorks.CabalCache.Pretty                   as PP
import qualified HaskellWorks.CabalCache.Store                    as M
import qualified HaskellWorks.CabalCache.Types                    as Z
import qualified Network.AWS                                      as AWS
import qualified Network.AWS.Data                                 as AWS
import qualified Options.Applicative                              as OA
import qualified Prettyprinter                                    as PP
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
runSyncFromArchive opts = OO.runOops $ OO.catchAndExitFailure @ExitFailure do
  let hostEndpoint          = opts ^. the @"hostEndpoint"
  let storePath             = opts ^. the @"storePath"
  let archiveUris           = opts ^. the @"archiveUris" :: NonEmpty Location
  let threads               = opts ^. the @"threads"
  let awsLogLevel           = opts ^. the @"awsLogLevel"
  let versionedArchiveUris  = archiveUris & traverse1 %~ (</> archiveVersion) :: NonEmpty Location
  let storePathHash         = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUris     = versionedArchiveUris & traverse1 %~ (</> T.pack storePathHash)
  let maxRetries            = opts ^. the @"maxRetries"
  let ignorePackages        = opts ^. the @"ignorePackages"

  CIO.putLn $ "Store path: "       <> PP.text storePath
  CIO.putLn $ "Store path hash: "  <> PP.text storePathHash
  forM_ archiveUris $ \archiveUri -> do
    CIO.putLn $ "Archive URI: "      <> PP.text archiveUri
  CIO.putLn $ "Archive version: "  <> PP.pretty @Text archiveVersion
  CIO.putLn $ "Threads: "          <> PP.show threads
  CIO.putLn $ "AWS Log level: "    <> PP.show awsLogLevel

  OO.catchAndExitFailure @ExitFailure do
    planJson <- Z.loadPlan (opts ^. the @"path" </> opts ^. the @"buildPath")
      & do OO.catch @DecodeError \e -> do
            CIO.hPutLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> PP.show e
            OO.throw ExitFailure

    compilerContext <- Z.mkCompilerContext planJson
      & do OO.catch @Text \e -> do
            CIO.hPutLn IO.stderr $ PP.pretty e
            OO.throw ExitFailure

    liftIO $ GhcPkg.testAvailability compilerContext

    envAws <- liftIO $ IO.unsafeInterleaveIO $ (<&> envOverride .~ Dual (Endo $ \s -> case hostEndpoint of
      Just (hostname, port, ssl) -> setEndpoint ssl hostname port s
      Nothing -> s))
      $ AWS.mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
    let compilerId                  = planJson ^. the @"compilerId"
    let storeCompilerPath           = storePath </> T.unpack compilerId
    let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
    let storeCompilerLibPath        = storeCompilerPath </> "lib"

    CIO.putLn "Creating store directories"
    liftIO $ createDirectoryIfMissing True storePath
    liftIO $ createDirectoryIfMissing True storeCompilerPath
    liftIO $ createDirectoryIfMissing True storeCompilerLibPath

    storeCompilerPackageDbPathExists <- liftIO $ doesDirectoryExist storeCompilerPackageDbPath

    unless storeCompilerPackageDbPathExists do
      CIO.putLn "Package DB missing. Creating Package DB"
      liftIO $ GhcPkg.init compilerContext storeCompilerPackageDbPath

    packages <- liftIO $ Z.getPackages storePath planJson

    let installPlan = planJson ^. the @"installPlan"
    let planPackages = M.fromList $ fmap (\p -> (p ^. the @"id", p)) installPlan

    let planDeps0 = installPlan >>= \p -> fmap (p ^. the @"id", ) $ mempty
          <> (p ^. the @"depends")
          <> (p ^. the @"exeDepends")
          <> (p ^.. the @"components" . each . the @"lib" . each . the @"depends"    . each)
          <> (p ^.. the @"components" . each . the @"lib" . each . the @"exeDepends" . each)
    let planDeps  = planDeps0 <> fmap (\p -> ("[universe]", p ^. the @"id")) installPlan

    downloadQueue <- liftIO $ STM.atomically $ DQ.createDownloadQueue planDeps

    let pInfos = M.fromList $ fmap (\p -> (p ^. the @"packageId", p)) packages

    IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
      liftIO $ IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

      liftIO $ IO.forkThreadsWait threads $ OO.runOops $ DQ.runQueue downloadQueue $ \packageId -> do
        OO.recoverOrVoid @DQ.DownloadStatus do
          pInfo <- pure (M.lookup packageId pInfos)
            & do OO.onNothing do
                  CIO.hPutLn IO.stderr $ "Warning: Invalid package id: " <> PP.pretty packageId
                  DQ.succeed

          let archiveBaseName     = Z.packageDir pInfo <.> ".tar.gz"
          let archiveFiles        = versionedArchiveUris & traverse1 %~ (</> T.pack archiveBaseName)
          let scopedArchiveFiles  = scopedArchiveUris & traverse1 %~ (</> T.pack archiveBaseName)
          let packageStorePath    = storePath </> Z.packageDir pInfo
          let packageName         = pInfo ^. the @"packageName"

          storeDirectoryExists <- liftIO $ doesDirectoryExist packageStorePath

          package <- pure (M.lookup packageId planPackages)
            & do OO.onNothing do
                  CIO.hPutLn IO.stderr $ "Warning: package not found" <> PP.pretty packageId
                  DQ.succeed

          when (skippable package) do
            CIO.putLn $ "Skipping: " <> PP.pretty packageId
            DQ.succeed

          when (packageName `S.member` ignorePackages) do
            CIO.putLn $ "Ignoring: " <> PP.text packageName
            DQ.fail

          when storeDirectoryExists DQ.succeed

          OO.suspend runResourceT $ ensureStorePathCleanup packageStorePath do
            let locations = sconcat $ fmap L.tuple2ToNel (NEL.zip archiveFiles scopedArchiveFiles)

            (existingArchiveFileContents, existingArchiveFile) <- readFirstAvailableResource envAws locations maxRetries
              & do OO.catch @AwsError \e -> do
                    CIO.putLn $ "Unable to download any of: " <> PP.show locations <> " because: " <> PP.show e
                    DQ.fail
              & do OO.catch @HttpError \e -> do
                    CIO.putLn $ "Unable to download any of: " <> PP.show locations <> " because: " <> PP.show e
                    DQ.fail
              & do OO.catch @NotFound \_ -> do
                    CIO.putLn $ "Not found: " <> PP.show locations
                    DQ.fail
              & do OO.catch @InvalidUrl \(InvalidUrl url reason) -> do
                    CIO.hPutLn IO.stderr $ "Invalid URL: " <> PP.pretty url <> ", " <> PP.pretty reason
                    DQ.fail
              & do OO.catch @UnsupportedUri \e -> do
                    CIO.hPutLn IO.stderr $ PP.show e
                    DQ.fail

            CIO.putLn $ "Extracting: " <> PP.text existingArchiveFile

            let tempArchiveFile = tempPath </> archiveBaseName
            liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents

            IO.extractTar tempArchiveFile storePath
              & do OO.catch @ArchiveError \(ArchiveError reason) -> do
                    CIO.putLn $ "Unable to extract tar at " <> PP.show tempArchiveFile <> " because: " <> PP.pretty reason
                    DQ.fail

            meta <- loadMetadata packageStorePath
            oldStorePath <- pure (Map.lookup "store-path" meta)
              & do OO.onNothing do
                    CIO.putLn "store-path is missing from Metadata"
                    DQ.fail

            let Z.Tagged conf _ = Z.confPath pInfo
            
            let theConfPath = storePath </> conf
            let tempConfPath = tempPath </> conf
            confPathExists <- liftIO $ IO.doesFileExist theConfPath
            when confPathExists do
              confContents <- liftIO $ LBS.readFile theConfPath
              liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
              liftIO $ IO.copyFile tempConfPath theConfPath >> IO.removeFile tempConfPath

            DQ.succeed

    CIO.putLn "Recaching package database"

    liftIO $ GhcPkg.recache compilerContext storeCompilerPackageDbPath

    failures <- liftIO $ STM.atomically $ STM.readTVar $ downloadQueue ^. the @"tFailures"

    forM_ failures $ \packageId -> CIO.hPutLn IO.stderr $ "Failed to download: " <> PP.text packageId

ensureStorePathCleanup :: ()
  => MonadIO m
  => MonadCatch m
  => e `OO.CouldBe` DQ.DownloadStatus
  => FilePath
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
ensureStorePathCleanup packageStorePath = 
  OO.snatch @DQ.DownloadStatus \downloadStatus -> do
    case downloadStatus of
      DQ.DownloadFailure -> M.cleanupStorePath packageStorePath
      DQ.DownloadSuccess ->
        CIO.hPutLn IO.stdout $ "Successfully cleaned up store path: " <> PP.show packageStorePath
    OO.throw downloadStatus

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
