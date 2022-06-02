{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Antiope.Env (Region)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import HaskellWorks.CabalCache.Location (Location)

import qualified Antiope.Env as AWS

data SyncToArchiveOptions = SyncToArchiveOptions
  { region        :: Region
  , archiveUri    :: Location
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  } deriving (Eq, Show, Generic)

data PlanOptions = PlanOptions
  { buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , outputFile    :: FilePath
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region        :: Region
  , archiveUris   :: [Location]
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
