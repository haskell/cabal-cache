module HaskellWorks.CabalCache.Store
  ( cleanupStorePath,
  ) where

import Effectful
import Effectful.Zoo.Core
import HaskellWorks.Prelude

import qualified HaskellWorks.CabalCache.IO.Lazy as IO
import qualified System.Directory                as IO

cleanupStorePath :: ()
  => r <: IOE
  => FilePath
  -> Eff r ()
cleanupStorePath packageStorePath = do
  pathExists <- liftIO $ IO.doesPathExist packageStorePath
  when pathExists $ void $ IO.removePathRecursive packageStorePath
