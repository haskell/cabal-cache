module HaskellWorks.Ci.Assist.GhcPkg
where

import Data.Text      (Text)
import System.Exit    (ExitCode (..), exitWith)
import System.Process (spawnProcess, waitForProcess)

import qualified Data.Text as Text
import qualified System.IO as IO

runGhcPkg :: [String] -> IO ()
runGhcPkg params = do
  hGhcPkg2 <- spawnProcess "ghc-pkg" params
  exitCodeGhcPkg2 <- waitForProcess hGhcPkg2
  case exitCodeGhcPkg2 of
    ExitFailure _ -> do
      IO.hPutStrLn IO.stderr "ERROR: Unable to recache package db"
      exitWith (ExitFailure 1)
    _ -> return ()

testAvailability :: IO ()
testAvailability = runGhcPkg ["--version"]

recache :: FilePath -> IO ()
recache packageDb = runGhcPkg ["recache", "--package-db", packageDb]
