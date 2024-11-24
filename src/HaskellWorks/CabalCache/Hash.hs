module HaskellWorks.CabalCache.Hash
  ( hashStorePath
  ) where

import HaskellWorks.Prelude

import qualified Crypto.Hash        as CH
import qualified Data.List          as L
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

hashStorePath :: String -> String
hashStorePath = L.take 10 . show . CH.hashWith CH.SHA256 . T.encodeUtf8 . T.pack
