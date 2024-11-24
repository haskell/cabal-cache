module Test.Base 
  ( execCabalCache
  , execCabalCache_
  , execCabalCache'
  , integration
  ) where

import Control.Monad.Catch (MonadCatch)
import HaskellWorks.Prelude
import Hedgehog (MonadTest)
import Hedgehog.Extras.Test.Process (ExecConfig)

import qualified GHC.Stack                    as GHC
import qualified Hedgehog                     as H
import qualified Hedgehog.Extras.Test.Base    as H
import qualified Hedgehog.Extras.Test.Process as H

integration :: HasCallStack => H.Integration () -> H.Property
integration = H.withTests 1 . H.propertyOnce

-- | Run cabal-cache, returning the stdout
execCabalCache
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m String
execCabalCache = GHC.withFrozenCallStack $ H.execFlex "cabal-cache" "CABAL_CACHE"

-- | Run cabal-cache, discarding return value
execCabalCache_
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m ()
execCabalCache_ = void . execCabalCache

-- | Run cabal-cache, returning the stdout
execCabalCache'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> [String]
  -> m String
execCabalCache' execConfig = GHC.withFrozenCallStack $ H.execFlex' execConfig "cabal-cache" "CABAL_CACHE"
