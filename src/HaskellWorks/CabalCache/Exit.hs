module HaskellWorks.CabalCache.Exit
  ( catchAndExitFailure,
  ) where

import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Log.Api
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude

import qualified System.Exit                        as IO

catchAndExitFailure :: forall e a r. ()
  => Show e
  => r <: IOE
  => r <: Log Text
  => Eff (Error e : r) a
  -> Eff r a
catchAndExitFailure f =
  f & trap @e \e -> do
    crit $ "Error: " <> tshow e
    liftIO IO.exitFailure
