{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.AppError
  ( AppError(..)
  , displayAppError
  , appErrorStatus
  ) where

import Data.String                  (IsString(..))
import Data.Text                    (Text)
import GHC.Generics                 (Generic)
import HaskellWorks.CabalCache.Show (tshow)

import qualified Data.Text          as T
import qualified Network.HTTP.Types as HTTP

data AppError
  = AwsAppError
    { status :: HTTP.Status
    }
  | HttpAppError
    { status :: HTTP.Status
    }
  | RetriesFailedAppError
  | NotFound
  | GenericAppError Text
  deriving (Eq, Show, Generic)

instance IsString AppError where
  fromString = GenericAppError . T.pack

displayAppError :: AppError -> Text
displayAppError (AwsAppError s)       = tshow s
displayAppError (HttpAppError s)      = tshow s
displayAppError RetriesFailedAppError = "Multiple retries failed"
displayAppError NotFound              = "Not found"
displayAppError (GenericAppError msg) = msg

appErrorStatus :: AppError -> Maybe Int
appErrorStatus (AwsAppError (HTTP.Status statusCode _)) = Just statusCode
appErrorStatus _                                        = Nothing
