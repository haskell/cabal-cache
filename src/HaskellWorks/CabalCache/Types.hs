{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.CabalCache.Types where

import Data.Aeson
import Data.Maybe   (fromMaybe)
import Data.Text    (Text)
import GHC.Generics

type CompilerId = Text
type PackageId  = Text

data PlanJson = PlanJson
  { compilerId  :: CompilerId
  , installPlan :: [Package]
  } deriving (Eq, Show, Generic)

data Package = Package
  { packageType   :: Text
  , id            :: PackageId
  , name          :: Text
  , version       :: Text
  , style         :: Maybe Text
  , componentName :: Maybe Text
  , depends       :: [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON PlanJson where
  parseJSON = withObject "PlanJson" $ \v -> PlanJson
    <$> v .: "compiler-id"
    <*> v .: "install-plan"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \v -> Package
    <$> v .:  "type"
    <*> v .:  "id"
    <*> v .:  "pkg-name"
    <*> v .:  "pkg-version"
    <*> v .:? "style"
    <*> v .:? "component-name"
    <*> (fromMaybe [] <$> (v .:?  "depends"))
