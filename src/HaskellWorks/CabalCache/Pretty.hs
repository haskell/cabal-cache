module HaskellWorks.CabalCache.Pretty
  ( Ann(..)
  , HaskellWorks.CabalCache.Pretty.show
  , text
  ) where

import Network.AWS.Data (ToText(..))
import Prelude hiding (Show(..))

import qualified Prelude       as P
import qualified Prettyprinter as PP

data Ann = Ann deriving (Eq, P.Show)

show :: P.Show a => a -> PP.Doc Ann
show = PP.pretty . P.show

text :: ToText a => a -> PP.Doc Ann
text = PP.pretty . toText
