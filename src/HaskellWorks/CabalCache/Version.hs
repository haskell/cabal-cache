module HaskellWorks.CabalCache.Version
  ( archiveVersion,
  ) where

import Data.String (IsString)

archiveVersion :: IsString s => s
archiveVersion = "v2"
