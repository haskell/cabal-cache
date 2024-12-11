module HaskellWorks.CabalCache.IO.File
  ( copyDirectoryRecursive,
    listMaybeDirectory,
  ) where

import HaskellWorks.Prelude
import Effectful
import Effectful.Zoo.Console.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Data.Text qualified as T
import System.Directory qualified as IO
import System.Exit qualified as IO
import System.Process qualified as IO

copyDirectoryRecursive :: ()
  => MonadIO m
  => r <: IOE
  => r <: Console Text
  => r <: Error String
  => String
  -> String
  -> Eff r ()
copyDirectoryRecursive source target = do
  print $ "Copying recursively from " <> T.pack source <> " to " <> T.pack target
  process <- liftIO $ IO.spawnProcess "cp" ["-r", source, target]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throw $ "cp exited with " <> show n

listMaybeDirectory :: MonadIO m => FilePath -> m [FilePath]
listMaybeDirectory filepath = do
  exists <- liftIO $ IO.doesDirectoryExist filepath
  if exists
    then liftIO $ IO.listDirectory filepath
    else return []
