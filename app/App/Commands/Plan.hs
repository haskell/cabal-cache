{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Plan
  ( cmdPlan,
  ) where

import App.Commands.Options.Types       (PlanOptions (PlanOptions))
import Control.Applicative              (optional)
import Control.Lens                     ((<&>), (&), (^.), (%~), Each(each))
import Control.Monad                    (forM)
import Control.Monad.IO.Class           (MonadIO(liftIO))
import Data.Generics.Product.Any        (the)
import Data.Maybe                       (fromMaybe)
import Data.Text                        (Text)
import HaskellWorks.CabalCache.Error    (DecodeError, ExitFailure(..))
import HaskellWorks.CabalCache.Location (Location (..), (<.>), (</>))
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              (Parser, Mod, CommandFields)

import qualified App.Commands.Options.Types         as Z
import qualified App.Static                         as AS
import qualified Control.Monad.Oops                 as OO
import qualified Data.Aeson                         as J
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.Core       as Z
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.Pretty     as PP
import qualified Network.AWS.Data                   as AWS
import qualified Options.Applicative                as OA
import qualified Prettyprinter                      as PP
import qualified System.IO                          as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Redundant do"              -}
{- HLINT ignore "Reduce duplication"        -}

runPlan :: Z.PlanOptions -> IO ()
runPlan opts = OO.runOops $ OO.catchAndExitFailure @ExitFailure do
  let storePath             = opts ^. the @"storePath"
  let archiveUris           = [Local ""]
  let storePathHash         = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let versionedArchiveUris  = archiveUris & each %~ (</> archiveVersion)
  let outputFile            = opts ^. the @"outputFile"

  CIO.putLn $ "Store path: "       <> PP.text storePath
  CIO.putLn $ "Store path hash: "  <> PP.text storePathHash
  CIO.putLn $ "Archive URIs: "     <> PP.show archiveUris
  CIO.putLn $ "Archive version: "  <> PP.pretty @Text archiveVersion

  planJson <- Z.loadPlan (opts ^. the @"path" </> opts ^. the @"buildPath")
    & do OO.catch @DecodeError \e -> do
          CIO.hPutLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> PP.show e
          OO.throw ExitFailure

  packages <- liftIO $ Z.getPackages storePath planJson

  plan <- forM packages $ \pInfo -> do
    let archiveFileBasename = Z.packageDir pInfo <.> ".tar.gz"
    let archiveFiles         = versionedArchiveUris <&> (</> T.pack archiveFileBasename)
    let scopedArchiveFiles   = versionedArchiveUris <&> (</> T.pack storePathHash </> T.pack archiveFileBasename)

    return $ archiveFiles <> scopedArchiveFiles

  if outputFile == "-"
    then liftIO $ LBS.putStr $ J.encode (fmap (fmap AWS.toText) plan)
    else liftIO $ LBS.writeFile outputFile $ J.encode (fmap (fmap AWS.toText) plan)

optsPlan :: Parser PlanOptions
optsPlan = PlanOptions
  <$> OA.strOption
      (   OA.long "path"
      <>  OA.help "Path to cabal project.  Defaults to \".\""
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
  <*> OA.strOption
      (   OA.long "output-file"
      <>  OA.help "Output file"
      <>  OA.metavar "FILE"
      <>  OA.value "-"
      )

cmdPlan :: Mod CommandFields (IO ())
cmdPlan = OA.command "plan" $ flip OA.info OA.idm $ runPlan <$> optsPlan
