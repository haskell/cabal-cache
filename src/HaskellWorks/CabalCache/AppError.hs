module HaskellWorks.CabalCache.AppError
  ( AwsStatusError(..),
    HttpError(..),
    HasStatusCode(..),
    HasMaybeStatusCode(..),
    displayAwsStatusError,
    displayHttpError,
  ) where

import HaskellWorks.Prelude

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

newtype AwsStatusError = AwsStatusError
  { status :: HTTP.Status
  }
  deriving (Eq, Show, Generic)

data HttpError = HttpError
  { reasponse :: HTTP.Request
  , content    :: HTTP.HttpExceptionContent
  }
  deriving (Show, Generic)

displayAwsStatusError :: AwsStatusError -> Text
displayAwsStatusError (AwsStatusError s) = tshow s

displayHttpError :: HttpError -> Text
displayHttpError (HttpError _ s) = tshow s

class HasStatusCode a where
  statusCodeOf :: a -> Int

class HasMaybeStatusCode a where
  maybeStatusCodeOf :: a -> Maybe Int

instance HasStatusCode AwsStatusError where
  statusCodeOf (AwsStatusError (HTTP.Status c _)) = c

instance HasMaybeStatusCode AwsStatusError where
  maybeStatusCodeOf (AwsStatusError (HTTP.Status c _)) = Just c

instance HasMaybeStatusCode HttpError where
  maybeStatusCodeOf (HttpError _ content') = case content' of
    HTTP.StatusCodeException response _ -> let HTTP.Status c _ = HTTP.responseStatus response in Just c
    _ -> Nothing
