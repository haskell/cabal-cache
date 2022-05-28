{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Plan
  ( cmdPlan
  ) where

import Antiope.Core                     (toText)
import App.Commands.Options.Types       (PlanOptions (PlanOptions))
import Control.Applicative
import Control.Lens                     hiding ((<.>))
import Control.Monad.Except
import Data.Generics.Product.Any        (the)
import Data.Maybe
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Location (Location (..), (<.>), (</>))
import HaskellWorks.CabalCache.Show
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              hiding (columns)

import qualified App.Commands.Options.Types         as Z
import qualified App.Static                         as AS
import qualified Control.Concurrent.STM             as STM
import qualified Data.Aeson                         as J
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.Core       as Z
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Polysemy                           as PY
import qualified Polysemy.Resource                  as PY
import qualified System.IO                          as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

runPlan :: Z.PlanOptions -> IO ()
runPlan opts = PY.runFinal . PY.resourceToIOFinal . PY.embedToFinal @IO $ do
  let storePath             = opts ^. the @"storePath"
  let archiveUris           = [Local ""]
  let storePathHash         = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let versionedArchiveUris  = archiveUris & each %~ (</> archiveVersion)
  let outputFile            = opts ^. the @"outputFile"

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  CIO.putStrLn $ "Archive URIs: "     <> tshow archiveUris
  CIO.putStrLn $ "Archive version: "  <> archiveVersion

  tEarlyExit <- liftIO $ STM.newTVarIO False

  mbPlan <- liftIO $ Z.loadPlan $ opts ^. the @"buildPath"

  case mbPlan of
    Right planJson -> do
      packages <- liftIO $ Z.getPackages storePath planJson

      plan <- forM packages $ \pInfo -> do
        let archiveFileBasename = Z.packageDir pInfo <.> ".tar.gz"
        let archiveFiles         = versionedArchiveUris <&> (</> T.pack archiveFileBasename)
        let scopedArchiveFiles   = versionedArchiveUris <&> (</> T.pack storePathHash </> T.pack archiveFileBasename)

        return $ archiveFiles <> scopedArchiveFiles

      if outputFile == "-"
        then liftIO $ LBS.putStr $ J.encode (fmap (fmap toText) plan)
        else liftIO $ LBS.writeFile outputFile $ J.encode (fmap (fmap toText) plan)

    Left (appError :: AppError) -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  earlyExit <- liftIO $ STM.readTVarIO tEarlyExit

  when earlyExit $ CIO.hPutStrLn IO.stderr "Early exit due to error"

optsPlan :: Parser PlanOptions
optsPlan = PlanOptions
  <$> strOption
      (   long "build-path"
      <>  help ("Path to cabal build directory.  Defaults to " <> show AS.buildPath)
      <>  metavar "DIRECTORY"
      <>  value AS.buildPath
      )
  <*> strOption
      (   long "store-path"
      <>  help "Path to cabal store"
      <>  metavar "DIRECTORY"
      <>  value (AS.cabalDirectory </> "store")
      )
  <*> optional
      ( strOption
        (   long "store-path-hash"
        <>  help "Store path hash (do not use)"
        <>  metavar "HASH"
        )
      )
  <*> strOption
      (   long "output-file"
      <>  help "Output file"
      <>  metavar "FILE"
      <>  value "-"
      )

cmdPlan :: Mod CommandFields (IO ())
cmdPlan = command "plan"  $ flip info idm $ runPlan <$> optsPlan
