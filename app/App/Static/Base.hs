module App.Static.Base
  ( homeDirectory,
    isPosix,
  ) where

import HaskellWorks.Prelude

import qualified System.Directory as IO
import qualified System.IO.Unsafe as IO
import qualified System.Info      as I

homeDirectory :: FilePath
homeDirectory = IO.unsafePerformIO IO.getHomeDirectory
{-# NOINLINE homeDirectory #-}

isPosix :: Bool
isPosix = I.os /= "mingw32"
{-# NOINLINE isPosix #-}
