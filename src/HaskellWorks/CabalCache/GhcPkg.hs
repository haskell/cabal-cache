{-# LANGUAGE TypeApplications #-}

module HaskellWorks.CabalCache.GhcPkg
  ( system,
    runGhcPkg,
    testAvailability,
    recache,
    init,
  ) where

import Prelude                   hiding (init)

import Control.Lens              ((^.))
import Data.Generics.Product.Any (HasAny(the))
import System.Exit               (ExitCode (..), exitWith)
import System.Process            (waitForProcess)

import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.Types      as Z
import qualified Prettyprinter                      as PP
import qualified System.IO                          as IO
import qualified System.Process                     as IO

system :: [String] -> IO IO.ProcessHandle
system (cmd:args) = IO.spawnProcess cmd args
system []         = error "No command supplied" -- TODO Better error handling

runGhcPkg :: Z.CompilerContext -> [String] -> IO ()
runGhcPkg cc params = do
  hGhcPkg2 <- system ((cc ^. the @"ghcPkgCmd") <> params)
  exitCodeGhcPkg2 <- waitForProcess hGhcPkg2
  case exitCodeGhcPkg2 of
    ExitFailure _ -> do
      CIO.hPutLn IO.stderr $ PP.pretty "ERROR: Unable to recache package db"
      exitWith (ExitFailure 1)
    _ -> return ()

testAvailability :: Z.CompilerContext -> IO ()
testAvailability cc = runGhcPkg cc ["--version"]

recache :: Z.CompilerContext -> FilePath -> IO ()
recache cc packageDb = runGhcPkg cc ["recache", "--package-db", packageDb]

init :: Z.CompilerContext -> FilePath -> IO ()
init cc packageDb = runGhcPkg cc ["init", packageDb]
