module HaskellWorks.CabalCache.Error
  ( nothingToError
  ) where

import Control.Monad.Except (MonadError(throwError))

nothingToError :: MonadError e m => e -> Maybe a -> m a
nothingToError _ (Just a) = return a
nothingToError e Nothing  = throwError e
