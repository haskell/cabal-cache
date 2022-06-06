module HaskellWorks.CabalCache.Text
  ( maybeStripPrefix
  ) where

import Data.Maybe (fromMaybe)
import Data.Text  (Text)

import qualified Data.Text as T

maybeStripPrefix :: Text -> Text -> Text
maybeStripPrefix prefix text = fromMaybe text (T.stripPrefix prefix text)
