{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.CabalCache.GhcPkg where

import Control.Lens              ((^.))
import Data.Generics.Product.Any (HasAny(the))
import System.Exit               (ExitCode (..), exitWith)
import System.Process            (waitForProcess)

import qualified HaskellWorks.CabalCache.Types as Z
import qualified System.IO                     as IO
import qualified System.Process                as IO

system :: [String] -> IO IO.ProcessHandle
system (cmd:args) = IO.spawnProcess cmd args
system []         = error "No command supplied" -- TODO Better error handling

runGhcPkg :: Z.CompilerContext -> [String] -> IO ()
runGhcPkg cc params = do
  hGhcPkg2 <- system ((cc ^. the @"ghcPkgCmd") <> params)
  exitCodeGhcPkg2 <- waitForProcess hGhcPkg2
  case exitCodeGhcPkg2 of
    ExitFailure _ -> do
      IO.hPutStrLn IO.stderr "ERROR: Unable to recache package db"
      exitWith (ExitFailure 1)
    _ -> return ()

testAvailability :: Z.CompilerContext -> IO ()
testAvailability cc = runGhcPkg cc ["--version"]

recache :: Z.CompilerContext -> FilePath -> IO ()
recache cc packageDb = runGhcPkg cc ["recache", "--package-db", packageDb]

init :: Z.CompilerContext -> FilePath -> IO ()
init cc packageDb = runGhcPkg cc ["init", packageDb]
