{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.CabalCache.AWS.Error
  ( handleAwsError
  ) where

import Control.Monad.Catch              (MonadCatch(..), MonadThrow(throwM))
import Control.Monad.Except             (MonadError)
import HaskellWorks.CabalCache.AppError (AwsError(..))
import HaskellWorks.Prelude

import qualified Amazonka                             as AWS
import qualified Control.Monad.Oops                   as OO
import qualified Network.HTTP.Types                   as HTTP

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsError :: (MonadCatch m,
 MonadError (OO.Variant e) m,
 OO.CouldBeF e AwsError) =>
 m a -> m a
handleAwsError f = catch f $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status _ _) _ _ _ _)) -> OO.throw $ AwsError s
    _                                                                    -> throwM e
