module App.Static
  ( cabalStoreDirectory,
    buildPath,
    path,
  ) where

import Control.Monad.Catch    (handle)
import HaskellWorks.Prelude

import qualified App.Static.Base    as S
import qualified App.Static.Posix   as P
import qualified App.Static.Windows as W
import qualified Cabal.Config       as CC
import qualified System.IO.Unsafe   as IO


{-# NOINLINE cabalStoreDirectory #-}
cabalStoreDirectory :: FilePath
cabalStoreDirectory = IO.unsafePerformIO $ handle' $
  runIdentity . CC.cfgStoreDir <$> CC.readConfig
 where
  handle' = handle (\(_ :: IOException) -> return $ if S.isPosix then P.cabalDirectory else W.cabalDirectory)

buildPath :: FilePath
buildPath = "dist-newstyle"

path :: FilePath
path = "."
