{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.Version where

import Data.String (IsString)

archiveVersion :: IsString s => s
archiveVersion = "v2"
