{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Plan
  ( cmdPlan
  ) where

import Antiope.Core                     (toText)
import App.Commands.Options.Types       (PlanOptions (PlanOptions))
import Control.Applicative              (optional)
import Control.Lens                     ((<&>), (&), (^.), (%~), Each(each))
import Control.Monad.Except             (when, forM)
import Data.Generics.Labels             ()
import Data.Generics.Product.Any        (the)
import Data.Maybe                       (fromMaybe)
import HaskellWorks.CabalCache.AppError (displayAppError, AppError)
import HaskellWorks.CabalCache.Location (Location (..), (<.>), (</>))
import HaskellWorks.CabalCache.Show     (tshow)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              (CommandFields, Mod, Parser)

import qualified App.Commands.Options.Types         as Z
import qualified App.Static                         as AS
import qualified Control.Concurrent.STM             as STM
import qualified Data.Aeson                         as J
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.Core       as Z
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Options.Applicative                as OA
import qualified System.IO                          as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

runPlan :: Z.PlanOptions -> IO ()
runPlan opts = do
  let storePath             = opts ^. #storePath
  let archiveUris           = [Local ""]
  let storePathHash         = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let versionedArchiveUris  = archiveUris & each %~ (</> archiveVersion)
  let outputFile            = opts ^. the @"outputFile"

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  CIO.putStrLn $ "Archive URIs: "     <> tshow archiveUris
  CIO.putStrLn $ "Archive version: "  <> archiveVersion

  tEarlyExit <- STM.newTVarIO False

  mbPlan <- Z.loadPlan $ opts ^. the @"buildPath"

  case mbPlan of
    Right planJson -> do
      packages <- Z.getPackages storePath planJson

      plan <- forM packages $ \pInfo -> do
        let archiveFileBasename = Z.packageDir pInfo <.> ".tar.gz"
        let archiveFiles         = versionedArchiveUris <&> (</> T.pack archiveFileBasename)
        let scopedArchiveFiles   = versionedArchiveUris <&> (</> T.pack storePathHash </> T.pack archiveFileBasename)

        return $ archiveFiles <> scopedArchiveFiles

      if outputFile == "-"
        then LBS.putStr $ J.encode (fmap (fmap toText) plan)
        else LBS.writeFile outputFile $ J.encode (fmap (fmap toText) plan)

    Left (appError :: AppError) -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  earlyExit <- STM.readTVarIO tEarlyExit

  when earlyExit $ CIO.hPutStrLn IO.stderr "Early exit due to error"

optsPlan :: Parser PlanOptions
optsPlan = PlanOptions
  <$> OA.strOption
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
  <*> optional
      ( OA.strOption
        (   OA.long "store-path-hash"
        <>  OA.help "Store path hash (do not use)"
        <>  OA.metavar "HASH"
        )
      )
  <*> OA.strOption
      (   OA.long "output-file"
      <>  OA.help "Output file"
      <>  OA.metavar "FILE"
      <>  OA.value "-"
      )

cmdPlan :: Mod CommandFields (IO ())
cmdPlan = OA.command "plan"  $ flip OA.info OA.idm $ runPlan <$> optsPlan
