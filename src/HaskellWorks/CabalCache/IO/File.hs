{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.IO.File
  ( copyDirectoryRecursive,
    listMaybeDirectory,
  ) where

import Control.Monad.Except   (MonadError)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Control.Monad.Oops                 as OO
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.Pretty     as PP
import qualified System.Directory                   as IO
import qualified System.Exit                        as IO
import qualified System.Process                     as IO

copyDirectoryRecursive :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` [Char]
  => String
  -> String
  -> m ()
copyDirectoryRecursive source target = do
  CIO.putLn $ "Copying recursively from " <> PP.text source <> " to " <> PP.text target
  process <- liftIO $ IO.spawnProcess "cp" ["-r", source, target]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> OO.throw $ "cp exited with " <> show n

listMaybeDirectory :: MonadIO m => FilePath -> m [FilePath]
listMaybeDirectory filepath = do
  exists <- liftIO $ IO.doesDirectoryExist filepath
  if exists
    then liftIO $ IO.listDirectory filepath
    else return []
