module App.Commands.Plan
  ( cmdPlan,
  )
where

import Amazonka.Data qualified as AWS
import App.Commands.Options.Types (PlanOptions (PlanOptions))
import App.Commands.Options.Types qualified as Z
import App.Run
import App.Static qualified as AS
import Control.Lens (Each (each), (%~))
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Effectful.Zoo.Core.Error.Static
import HaskellWorks.CabalCache.Core qualified as Z
import HaskellWorks.CabalCache.Error (DecodeError, ExitFailure (..))
import HaskellWorks.CabalCache.Hash qualified as H
import HaskellWorks.CabalCache.IO.Console qualified as CIO
import HaskellWorks.CabalCache.Location (Location (..), (<.>), (</>))
import HaskellWorks.CabalCache.Version (archiveVersion)
import HaskellWorks.Prelude
import Options.Applicative (CommandFields, Mod, Parser)
import Options.Applicative qualified as OA
import System.IO qualified as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

runPlan :: Z.PlanOptions -> IO ()
runPlan opts = runApp do
  let storePath = opts.storePath
  let archiveUris = [LocalFile ""]
  let storePathHash = opts.storePathHash & fromMaybe (H.hashStorePath storePath)
  let versionedArchiveUris = archiveUris & each %~ (</> archiveVersion)
  let outputFile = opts.outputFile

  CIO.putStrLn $ "Store path: " <> AWS.toText storePath
  CIO.putStrLn $ "Store path hash: " <> T.pack storePathHash
  CIO.putStrLn $ "Archive URIs: " <> tshow archiveUris
  CIO.putStrLn $ "Archive version: " <> archiveVersion

  planJson <-
    Z.loadPlan (opts.path </> opts.buildPath)
      & do
        trap @DecodeError \e -> do
          CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> tshow e
          throw ExitFailure

  packages <- liftIO $ Z.getPackages storePath planJson

  plan <- forM packages $ \pInfo -> do
    let archiveFileBasename = pInfo.packageDir <.> ".tar.gz"
    let archiveFiles = versionedArchiveUris <&> (</> T.pack archiveFileBasename)
    let scopedArchiveFiles = versionedArchiveUris <&> (</> T.pack storePathHash </> T.pack archiveFileBasename)

    return $ archiveFiles <> scopedArchiveFiles

  if outputFile == "-"
    then liftIO $ LBS.putStr $ J.encode (fmap (fmap AWS.toText) plan)
    else liftIO $ LBS.writeFile outputFile $ J.encode (fmap (fmap AWS.toText) plan)

optsPlan :: Parser PlanOptions
optsPlan =
  PlanOptions
    <$> OA.strOption
      ( OA.long "path"
          <> OA.help "Path to cabal project.  Defaults to \".\""
          <> OA.metavar "DIRECTORY"
          <> OA.value AS.path
      )
    <*> OA.strOption
      ( OA.long "build-path"
          <> OA.help ("Path to cabal build directory.  Defaults to " <> show AS.buildPath)
          <> OA.metavar "DIRECTORY"
          <> OA.value AS.buildPath
      )
    <*> OA.strOption
      ( OA.long "store-path"
          <> OA.help "Path to cabal store"
          <> OA.metavar "DIRECTORY"
          <> OA.value AS.cabalStoreDirectory
      )
    <*> optional
      ( OA.strOption
          ( OA.long "store-path-hash"
              <> OA.help "Store path hash (do not use)"
              <> OA.metavar "HASH"
          )
      )
    <*> OA.strOption
      ( OA.long "output-file"
          <> OA.help "Output file"
          <> OA.metavar "FILE"
          <> OA.value "-"
      )

cmdPlan :: Mod CommandFields (IO ())
cmdPlan = OA.command "plan" $ flip OA.info OA.idm $ runPlan <$> optsPlan
