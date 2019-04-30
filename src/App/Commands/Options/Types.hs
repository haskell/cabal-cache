{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Antiope.Env                      (Region)
import Data.Text                        (Text)
import GHC.Generics
import GHC.Word                         (Word8)
import HaskellWorks.CabalCache.Location
import Network.AWS.Types                (Region)

data SyncToArchiveOptions = SyncToArchiveOptions
  { region        :: Region
  , archiveUri    :: Location
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region        :: Region
  , archiveUri    :: Location
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
