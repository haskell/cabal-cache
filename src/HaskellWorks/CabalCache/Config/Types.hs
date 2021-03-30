{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.Config.Types
  ( CabalCacheConfig(..)
  , Plugin(..)

  , defaultConfig
  ) where

import Control.Lens              hiding ((.=))
import Data.Aeson                (FromJSON (..), ToJSON, Value, (.:), (.=))
import Data.Aeson.Types          (Parser)
import Data.Generics.Product.Any
import Data.Text                 (Text)
import GHC.Generics              (Generic (..))
import Network.URI               (URI)

import qualified Data.Aeson       as J
import qualified Data.Aeson.Types as J
import qualified Network.URI      as URI

newtype CabalCacheConfig = CabalCacheConfig
  { plugins :: [Plugin]
  } deriving (Eq, Show, Generic)

data Plugin = Plugin
  { name      :: Text
  , binary    :: FilePath
  , uriPrefix :: URI
  } deriving (Eq, Show, Generic)

defaultConfig :: CabalCacheConfig
defaultConfig = CabalCacheConfig
  { plugins = []
  }

instance FromJSON CabalCacheConfig where
  parseJSON = J.withObject "CabalCacheConfig" $ \v -> CabalCacheConfig
    <$> v .: "plugins"

instance ToJSON CabalCacheConfig where
  toJSON config = J.object
    [ "plugins" .= J.toJSON (config ^. the @"plugins")
    ]

instance FromJSON Plugin where
  parseJSON = J.withObject "Plugin" $ \v -> Plugin
    <$> v .: "name"
    <*> v .: "binary"
    <*> ((v .: "uriPrefix") >>= parseUri)

instance ToJSON Plugin where
  toJSON config = J.object
    [ "name"      .= J.toJSON (config ^. the @"name")
    , "binary"    .= J.toJSON (config ^. the @"binary")
    , "uriPrefix" .= J.toJSON (config ^. the @"uriPrefix" & show)
    ]

parseUri :: Value -> Parser URI
parseUri v = do
  s <- parseJSON @String v

  case URI.parseURI s of
    Just uri -> return uri
    Nothing  -> J.typeMismatch "URI" v
