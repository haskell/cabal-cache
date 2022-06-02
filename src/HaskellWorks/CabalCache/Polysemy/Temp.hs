{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.CabalCache.Polysemy.Temp
  ( createTempDirectory
  , withSystemTempDirectory
  , withTempDirectory
  ) where

import Polysemy (Member, Sem)
import Control.Monad.IO.Class (liftIO)

import qualified System.Directory     as IO
import qualified Polysemy.Embed       as PY
import qualified Polysemy.Resource    as PY
import qualified System.IO.Temp       as Temp
import qualified Control.Monad.Catch  as MC

withSystemTempDirectory :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => String                 -- ^ Directory name template
  -> (FilePath -> Sem r a)  -- ^ Callback that can use the directory
  -> Sem r a
withSystemTempDirectory template action = do
  tmpDir <- liftIO Temp.getCanonicalTemporaryDirectory
  withTempDirectory tmpDir template action

withTempDirectory :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => FilePath               -- ^ Parent directory to create the directory in
  -> String                 -- ^ Directory name template
  -> (FilePath -> Sem r a)  -- ^ Callback that can use the directory
  -> Sem r a
withTempDirectory targetDir template =
  PY.bracket
    (liftIO (Temp.createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . IO.removeDirectoryRecursive)

ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `MC.catch` (\(_ :: IOError) -> return ())

-- | Create a temporary directory.
createTempDirectory :: ()
  => Member (PY.Embed IO) r
  => FilePath -- ^ Parent directory to create the directory in
  -> String   -- ^ Directory name template
  -> Sem r FilePath
createTempDirectory dir template = liftIO $ Temp.createTempDirectory dir template
