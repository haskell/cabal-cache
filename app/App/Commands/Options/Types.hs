{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types
  ( CpOptions(..),
    PlanOptions(..),
    SyncFromArchiveOptions(..),
    SyncToArchiveOptions(..),
    VersionOptions(..),
  ) where

import Data.Set                         (Set)
import HaskellWorks.CabalCache.Location (Location)
import HaskellWorks.CabalCache.Types    (PackageId)
import HaskellWorks.Prelude
import Network.URI                      (URI)

import qualified Amazonka           as AWS
import qualified Data.List.NonEmpty as NEL

data CpOptions = CpOptions
  { region        :: AWS.Region
  , srcUri        :: URI
  , dstUri        :: URI
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  } deriving (Eq, Show, Generic)

data SyncToArchiveOptions = SyncToArchiveOptions
  { region          :: AWS.Region
  , archiveUri      :: Location
  , path            :: FilePath
  , buildPath       :: FilePath
  , storePath       :: FilePath
  , storePathHash   :: Maybe String
  , threads         :: Int
  , awsLogLevel     :: Maybe AWS.LogLevel
  , hostEndpoint    :: Maybe (ByteString, Int, Bool)
  , maxRetries      :: Int
  , ignorePackages  :: Set PackageId
  } deriving (Eq, Show, Generic)

data PlanOptions = PlanOptions
  { path          :: FilePath
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , outputFile    :: FilePath
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region          :: AWS.Region
  , archiveUris     :: NEL.NonEmpty Location
  , path            :: FilePath
  , buildPath       :: FilePath
  , storePath       :: FilePath
  , storePathHash   :: Maybe String
  , threads         :: Int
  , awsLogLevel     :: Maybe AWS.LogLevel
  , hostEndpoint    :: Maybe (ByteString, Int, Bool)
  , maxRetries      :: Int
  , ignorePackages  :: Set PackageId
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
