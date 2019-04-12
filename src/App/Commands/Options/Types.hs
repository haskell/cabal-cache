{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Data.Text         (Text)
import GHC.Generics
import GHC.Word          (Word8)
import Network.AWS.Types (Region)

data SyncToArchiveOptions = SyncToArchiveOptions
  { archiveUri :: Text
  , storePath  :: FilePath
  , threads    :: Int
  , region     :: Region
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { archiveUri :: Text
  , storePath  :: FilePath
  , threads    :: Int
  , region     :: Region
  } deriving (Eq, Show, Generic)
