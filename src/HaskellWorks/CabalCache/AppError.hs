{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.CabalCache.AppError
  ( AwsError(..),
    HttpError(..),
    HasStatusCode(..),
    HasMaybeStatusCode(..),
    displayAwsError,
    displayHttpError,
  ) where

import HaskellWorks.Prelude

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

newtype AwsError = AwsError
  { status :: HTTP.Status
  }
  deriving (Eq, Show, Generic)

data HttpError = HttpError
  { reasponse :: HTTP.Request
  , content    :: HTTP.HttpExceptionContent
  }
  deriving (Show, Generic)

displayAwsError :: AwsError -> Text
displayAwsError (AwsError s) = tshow s

displayHttpError :: HttpError -> Text
displayHttpError (HttpError _ s) = tshow s

class HasStatusCode a where
  statusCodeOf :: a -> Int

class HasMaybeStatusCode a where
  maybeStatusCodeOf :: a -> Maybe Int

instance HasStatusCode AwsError where
  statusCodeOf (AwsError (HTTP.Status c _)) = c

instance HasMaybeStatusCode AwsError where
  maybeStatusCodeOf (AwsError (HTTP.Status c _)) = Just c

instance HasMaybeStatusCode HttpError where
  maybeStatusCodeOf (HttpError _ content') = case content' of
    HTTP.StatusCodeException response _ -> let HTTP.Status c _ = HTTP.responseStatus response in Just c
    _ -> Nothing
