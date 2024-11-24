{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.CabalCache.Error
  ( DecodeError(DecodeError),
    ExitFailure(ExitFailure),
    CopyFailed(CopyFailed),
    InvalidUrl(InvalidUrl),
    NotFound(NotFound),
    NotImplemented(NotImplemented),
    UnsupportedUri(UnsupportedUri),
  ) where

import HaskellWorks.Prelude
import Network.URI  (URI)

data DecodeError = DecodeError Text deriving (Eq, Show, Generic)

data ExitFailure = ExitFailure deriving (Eq, Show, Generic)

data CopyFailed = CopyFailed deriving (Eq, Show, Generic)

data InvalidUrl = InvalidUrl
  { url    :: Text
  , reason :: Text
  } deriving (Eq, Show, Generic)

data NotFound = NotFound deriving (Eq, Show, Generic)

data NotImplemented = NotImplemented Text deriving (Eq, Show, Generic)

data UnsupportedUri = UnsupportedUri
  { uri    :: URI
  , reason :: Text
  } deriving (Eq, Show, Generic)
