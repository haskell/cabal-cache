{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Antiope.Env       (Region)
import Data.Text         (Text)
import GHC.Generics
import GHC.Word          (Word8)
import Network.AWS.Types (Region)

data SyncToArchiveOptions = SyncToArchiveOptions
  { region     :: Region
  , archiveUri :: Text
  , storePath  :: FilePath
  , threads    :: Int
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region     :: Region
  , archiveUri :: Text
  , storePath  :: FilePath
  , threads    :: Int
  } deriving (Eq, Show, Generic)
