module HaskellWorks.CabalCache.Types
  ( CompilerId,
    PackageId,
    PackageName,
    CompilerContext(..),
    Components(..),
    PlanJson(..),
    Package(..),
    Lib(..),
  ) where

import Data.Aeson   (FromJSON(parseJSON), (.!=), (.:), (.:?))
import Data.Text    (Text)
import GHC.Generics (Generic)
import Prelude      hiding (id)

import qualified Data.Aeson as J

type CompilerId   = Text
type PackageId    = Text
type PackageName  = Text

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
  , components    :: Maybe Components
  , depends       :: [Text]
  , exeDepends    :: [Text]
  } deriving (Eq, Show, Generic)

newtype Components = Components
  { lib :: Maybe Lib
  } deriving (Eq, Show, Generic)

data Lib = Lib
  { depends    :: [Text]
  , exeDepends :: [Text]
  } deriving (Eq, Show, Generic)

newtype CompilerContext = CompilerContext
  { ghcPkgCmd :: [String]
  } deriving (Show, Eq, Generic)

instance FromJSON PlanJson where
  parseJSON = J.withObject "PlanJson" $ \v -> PlanJson
    <$> v .: "compiler-id"
    <*> v .: "install-plan"

instance FromJSON Package where
  parseJSON = J.withObject "Package" $ \v -> do
    packageType   <- v .:  "type"
    id            <- v .:  "id"
    name          <- v .:  "pkg-name"
    version       <- v .:  "pkg-version"
    style         <- v .:? "style"
    componentName <- v .:? "component-name"
    components    <- v .:? "components"
    depends       <- v .:? "depends"     .!= []
    exeDepends    <- v .:? "exe-depends" .!= []
    return Package {..}

instance FromJSON Components where
  parseJSON = J.withObject "Components" $ \v -> Components
    <$> v .:? "lib"

instance FromJSON Lib where
  parseJSON = J.withObject "Lib" $ \v -> Lib
    <$> v .:? "depends"     .!= []
    <*> v .:? "exe-depends" .!= []
