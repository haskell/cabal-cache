module App.Static.Windows
  ( appDataDirectory,
    cabalDirectory,
  ) where

import HaskellWorks.CabalCache.Location ((</>))
import HaskellWorks.Prelude

import qualified App.Static.Base    as S
import qualified System.Environment as IO
import qualified System.IO.Unsafe   as IO

appDataDirectory :: FilePath
appDataDirectory = IO.unsafePerformIO $ fmap (fromMaybe S.homeDirectory) (IO.lookupEnv "APPDATA")
{-# NOINLINE appDataDirectory #-}

cabalDirectory :: FilePath
cabalDirectory = appDataDirectory </> "cabal"
