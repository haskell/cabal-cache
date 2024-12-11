module App.Static.Posix
  ( cabalDirectory,
  ) where

import HaskellWorks.CabalCache.Location ((</>))
import HaskellWorks.Prelude

import qualified App.Static.Base as S
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

cabalDirectory :: FilePath
cabalDirectory =
  IO.unsafePerformIO do
    IO.lookupEnv "CABAL_DIR" >>= \case
      Just dir -> pure dir
      Nothing -> do
        let oldCabalDir = S.homeDirectory </> ".cabal"
        let newCabaldir = S.homeDirectory </> ".local/state/cabal/store"
        oldExists <- IO.doesDirectoryExist oldCabalDir
        newExists <- IO.doesDirectoryExist newCabaldir
        case (oldExists, newExists) of
          (True, True) -> do
            IO.hPutStrLn IO.stderr "Warning: Both ~/.cabal and ~/.local/state/cabal/store exist"
            pure newCabaldir
          (True, _) -> pure oldCabalDir
          (_, True) -> pure newCabaldir
          _ -> fail "No cabal directory found"
