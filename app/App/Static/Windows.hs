module App.Static.Windows where

import Data.Maybe                       (fromMaybe)
import HaskellWorks.CabalCache.Location ((</>))

import qualified App.Static.Base    as S
import qualified System.Environment as IO
import qualified System.IO.Unsafe   as IO

appDataDirectory :: FilePath
appDataDirectory = IO.unsafePerformIO $ fmap (fromMaybe S.homeDirectory) (IO.lookupEnv "APPDATA")
{-# NOINLINE appDataDirectory #-}

cabalDirectory :: FilePath
cabalDirectory = appDataDirectory </> "cabal"
