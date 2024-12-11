module HaskellWorks.CabalCache.URI
  ( reslashUri,
  ) where

import Data.Generics.Product.Any (HasAny(the))
import HaskellWorks.Prelude
import Lens.Micro
import Network.URI               (URI)

reslashUri :: URI -> URI
reslashUri uri = uri & the @"uriPath" %~ fmap reslashChar
  where reslashChar :: Char -> Char
        reslashChar '\\' = '/'
        reslashChar c    = c
