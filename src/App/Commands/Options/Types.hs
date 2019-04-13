{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Antiope.Env                     (Region)
import Data.Text                       (Text)
import GHC.Generics
import GHC.Word                        (Word8)
import HaskellWorks.Ci.Assist.Location
import Network.AWS.Types               (Region)

data SyncToArchiveOptions = SyncToArchiveOptions
  { region     :: Region
  , archiveUri :: Location
  , storePath  :: FilePath
  , threads    :: Int
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region     :: Region
  , archiveUri :: Location
  , storePath  :: FilePath
  , threads    :: Int
  } deriving (Eq, Show, Generic)
