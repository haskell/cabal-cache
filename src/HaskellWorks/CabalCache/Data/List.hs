module HaskellWorks.CabalCache.Data.List
  ( tuple2ToDL,
    tuple2ToList,
    tuple2ToNel,
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import HaskellWorks.Prelude

tuple2ToDL :: (a, a) -> [a] -> [a]
tuple2ToDL (a, b) = (a:) . (b:)

tuple2ToList :: (a, a) -> [a]
tuple2ToList ab = tuple2ToDL ab []

tuple2ToNel :: (a, a) -> NonEmpty a
tuple2ToNel (a, b) = a :| [b]
