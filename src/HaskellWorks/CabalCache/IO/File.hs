{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module HaskellWorks.CabalCache.IO.File
  ( copyDirectoryRecursive
  , listMaybeDirectory
  ) where

import Control.Monad.Except (MonadIO(..), ExceptT, MonadTrans(lift), MonadError(throwError))
import Polysemy (Member, Sem)

import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Polysemy                           as PY
import qualified Polysemy.Resource                  as PY
import qualified System.Directory                   as IO
import qualified System.Exit                        as IO
import qualified System.Process                     as IO

copyDirectoryRecursive :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => FilePath
  -> FilePath
  -> ExceptT String (Sem r) ()
copyDirectoryRecursive source target = do
  lift . CIO.putStrLn $ "Copying recursively from " <> T.pack source <> " to " <> T.pack target
  process <- lift . PY.embed $ IO.spawnProcess "cp" ["-r", source, target]
  exitCode <- lift . PY.embed $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throwError $ "cp exited with " <> show n

listMaybeDirectory :: MonadIO m => FilePath -> m [FilePath]
listMaybeDirectory filepath = do
  exists <- liftIO $ IO.doesDirectoryExist filepath
  if exists
    then liftIO $ IO.listDirectory filepath
    else return []
