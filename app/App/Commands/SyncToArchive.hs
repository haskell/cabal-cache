{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive
  ) where

import Antiope.Core                     (Region (..), toText)
import Antiope.Env                      (mkEnv)
import Antiope.Options.Applicative      (autoText)
import App.Commands.Options.Parser      (text)
import App.Commands.Options.Types       (SyncToArchiveOptions (SyncToArchiveOptions))
import Control.Applicative              (Alternative((<|>)))
import Control.Lens                     ((<&>), (&), (^..), (^.), (.~), Each(each))
import Control.Monad                    (void, when, filterM, unless)
import Control.Monad.Except             (MonadError(catchError), MonadIO(..), runExceptT)
import Control.Monad.Trans.Resource     (runResourceT)
import Control.Monad.Trans.AWS          (envOverride, setEndpoint)
import Data.ByteString                  (ByteString)
import Data.Generics.Product.Any        (the)
import Data.List                        ((\\))
import Data.Maybe                       (fromMaybe)
import Data.Monoid                      (Dual(Dual), Endo(Endo))
import HaskellWorks.CabalCache.AppError (displayAppError, AppError(AwsAppError))
import HaskellWorks.CabalCache.Location (Location (..), toLocation, (<.>), (</>))
import HaskellWorks.CabalCache.Metadata (createMetadata)
import HaskellWorks.CabalCache.Show     (tshow)
import HaskellWorks.CabalCache.Topology (buildPlanData, canShare)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              (Mod, Parser, CommandFields)
import System.Directory                 (doesDirectoryExist)

import qualified App.Commands.Options.Types         as Z
import qualified App.Static                         as AS
import qualified Control.Concurrent.STM             as STM
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text                          as T
import qualified Data.Text                          as Text
import qualified HaskellWorks.CabalCache.Aws.Env    as AWS
import qualified HaskellWorks.CabalCache.Core       as Z
import qualified HaskellWorks.CabalCache.GhcPkg     as GhcPkg
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.IO.Error   as IO
import qualified HaskellWorks.CabalCache.IO.File    as IO
import qualified HaskellWorks.CabalCache.IO.Lazy    as IO
import qualified HaskellWorks.CabalCache.IO.Tar     as IO
import qualified Network.HTTP.Types                 as HTTP
import qualified Options.Applicative                as OA
import qualified System.Directory                   as IO
import qualified System.IO                          as IO
import qualified System.IO.Temp                     as IO
import qualified System.IO.Unsafe                   as IO
import qualified UnliftIO.Async                     as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  let hostEndpoint        = opts ^. the @"hostEndpoint"
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

  tEarlyExit <- STM.newTVarIO False

  mbPlan <- Z.loadPlan $ opts ^. the @"buildPath"

  case mbPlan of
    Right planJson -> do
      compilerContextResult <- runExceptT $ Z.mkCompilerContext planJson

      case compilerContextResult of
        Right compilerContext -> do
          let compilerId = planJson ^. the @"compilerId"
          envAws <- IO.unsafeInterleaveIO $ (<&> envOverride .~ Dual (Endo $ \s -> case hostEndpoint of
            Just (hostname, port, ssl) -> setEndpoint ssl hostname port s
            Nothing -> s))
            $ mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
          let archivePath       = versionedArchiveUri </> compilerId
          let scopedArchivePath = scopedArchiveUri </> compilerId
          IO.createLocalDirectoryIfMissing archivePath
          IO.createLocalDirectoryIfMissing scopedArchivePath

          packages     <- Z.getPackages storePath planJson
          nonShareable <- packages & filterM (fmap not . isShareable storePath)
          let planData = buildPlanData planJson (nonShareable ^.. each . the @"packageId")

          let storeCompilerPath           = storePath </> T.unpack compilerId
          let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"

          storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

          unless storeCompilerPackageDbPathExists $
            GhcPkg.init compilerContext storeCompilerPackageDbPath

          CIO.putStrLn $ "Syncing " <> tshow (length packages) <> " packages"

          IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
            CIO.putStrLn $ "Temp path: " <> tshow tempPath

            IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
              earlyExit <- STM.readTVarIO tEarlyExit
              unless earlyExit $ do
                let archiveFileBasename = Z.packageDir pInfo <.> ".tar.gz"
                let archiveFile         = versionedArchiveUri </> T.pack archiveFileBasename
                let scopedArchiveFile   = versionedArchiveUri </> T.pack storePathHash </> T.pack archiveFileBasename
                let packageStorePath    = storePath </> Z.packageDir pInfo

                -- either write "normal" package, or a user-specific one if the package cannot be shared
                let targetFile = if canShare planData (Z.packageId pInfo) then archiveFile else scopedArchiveFile

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
                          <> tshow (fmap toText [scopedArchiveFile, archiveFile])
                          <> " " <> displayAppError e

                      _ -> return ()
        Left msg -> CIO.hPutStrLn IO.stderr msg

    Left (appError :: AppError) -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  earlyExit <- STM.readTVarIO tEarlyExit

  when earlyExit $ CIO.hPutStrLn IO.stderr "Early exit due to error"

isShareable :: MonadIO m => FilePath -> Z.PackageInfo -> m Bool
isShareable storePath pkg =
  let packageSharePath = storePath </> Z.packageDir pkg </> "share"
  in IO.listMaybeDirectory packageSharePath <&> (\\ ["doc"]) <&> null

optsSyncToArchive :: Parser SyncToArchiveOptions
optsSyncToArchive = SyncToArchiveOptions
  <$> OA.option (OA.auto <|> text)
      (   OA.long "region"
      <>  OA.metavar "AWS_REGION"
      <>  OA.showDefault <> OA.value Oregon
      <>  OA.help "The AWS region in which to operate"
      )
  <*> OA.option (OA.maybeReader (toLocation . Text.pack))
      (   OA.long "archive-uri"
      <>  OA.help "Archive URI to sync to"
      <>  OA.metavar "S3_URI"
      <>  OA.value (Local $ AS.cabalDirectory </> "archive")
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
  <$>  OA.option autoText
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
cmdSyncToArchive = OA.command "sync-to-archive"  $ flip OA.info OA.idm $ runSyncToArchive <$> optsSyncToArchive
