module App.Static.Posix
  ( cabalDirectory,
  ) where

import HaskellWorks.CabalCache.Location ((</>))
import HaskellWorks.Prelude

import qualified App.Static.Base as S
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

cabalDirectory :: FilePath
cabalDirectory =
  IO.unsafePerformIO do
    IO.lookupEnv "CABAL_DIR" >>= \case
      Just dir -> pure dir
      Nothing -> pure $ S.homeDirectory </> ".cabal"
