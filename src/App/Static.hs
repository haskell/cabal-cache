module App.Static where

import Data.Text (Text)

import qualified Data.Text        as T
import qualified System.Directory as IO
import qualified System.IO.Unsafe as IO

homeDirectory :: FilePath
homeDirectory = IO.unsafePerformIO $ IO.getHomeDirectory
{-# NOINLINE homeDirectory #-}
