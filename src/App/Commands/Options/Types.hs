{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Antiope.Env                      (Region)
import GHC.Generics
import HaskellWorks.CabalCache.Location

import qualified Antiope.Env as AWS

data SyncToArchiveOptions = SyncToArchiveOptions
  { region        :: Region
  , archiveUri    :: Location
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  , awsLogLevel   :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region            :: Region
  , archiveUris       :: [Location]
  , buildPath         :: FilePath
  , storePath         :: FilePath
  , storePathHash     :: Maybe String
  , threads           :: Int
  , awsLogLevel       :: Maybe AWS.LogLevel
  , outputRestoreList :: Maybe FilePath
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
