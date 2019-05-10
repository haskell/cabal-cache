{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.AppError
  ( AppError(..)
  , displayAppError
  , appErrorStatus
  ) where

import Data.Text                    (Text)
import GHC.Generics
import HaskellWorks.CabalCache.Show

import qualified Network.HTTP.Types as HTTP

data AppError
  = AwsAppError
    { status :: HTTP.Status
    }
  | RetriesFailedAppError
  | GenericAppError Text
  deriving (Eq, Show, Generic)

displayAppError :: AppError -> Text
displayAppError (AwsAppError status)  = tshow status
displayAppError RetriesFailedAppError = "Multiple retries failed"
displayAppError (GenericAppError msg) = msg

appErrorStatus :: AppError -> Maybe Int
appErrorStatus (AwsAppError (HTTP.Status statusCode _)) = Just statusCode
appErrorStatus _                                        = Nothing
