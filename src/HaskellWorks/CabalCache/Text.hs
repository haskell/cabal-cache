{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.Text
  ( maybeStripPrefix
  ,indentLines
  ) where

import Data.Maybe
import Data.Text  (Text)

import qualified Data.Text as T

maybeStripPrefix :: Text -> Text -> Text
maybeStripPrefix prefix text = fromMaybe text (T.stripPrefix prefix text)

indentLines :: Text -> Text
indentLines = T.unlines . fmap ("  " <>) . T.lines
