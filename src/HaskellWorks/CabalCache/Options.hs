module HaskellWorks.CabalCache.Options
  ( readOrFromTextOption,
  ) where

import Amazonka.Data.Text    (FromText (..), fromText)
import HaskellWorks.Prelude
import Options.Applicative   (Parser, Mod, OptionFields)

import qualified Data.Text            as T
import qualified Options.Applicative  as OA

orElse :: Either e a -> Either e a -> Either e a
orElse a b =
  either (const b) Right a

readOrFromTextOption :: (Read a, FromText a) => Mod OptionFields a -> Parser a
readOrFromTextOption =
  let fromStr s = readEither s `orElse` fromText (T.pack s)
  in OA.option $ OA.eitherReader fromStr
