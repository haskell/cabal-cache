module HaskellWorks.CabalCache.Location
( IsPath(..)
, Location(..)
, toLocation
)
where

import Data.Generics.Product.Any          (HasAny(the))
import HaskellWorks.CabalCache.AWS.S3.URI (S3Uri (..))
import HaskellWorks.Prelude
import Lens.Micro
import Network.URI                        (URI)

import qualified Amazonka.Data    as AWS
import qualified Amazonka.S3      as AWS
import qualified Data.Text        as T
import qualified Network.URI      as URI
import qualified System.FilePath  as FP

class IsPath a s | a -> s where
  (</>) :: a -> s -> a
  (<.>) :: a -> s -> a

infixr 5 </>
infixr 7 <.>

data Location
  = Uri URI
  | LocalFile FilePath
  deriving (Show, Eq, Generic)

instance AWS.ToText Location where
  toText (Uri uri) = tshow uri
  toText (LocalFile p) = T.pack p

instance IsPath Location Text where
  Uri   b </> p = Uri   (b </> p)
  LocalFile b </> p = LocalFile (b </> T.unpack p)

  Uri   b <.> e = Uri   (b <.> e)
  LocalFile b <.> e = LocalFile (b <.> T.unpack e)

instance IsPath Text Text where
  b </> p = T.pack (T.unpack b FP.</> T.unpack p)
  b <.> e = T.pack (T.unpack b FP.<.> T.unpack e)

instance IsPath URI Text where
  b </> p = b & the @"uriPath" %~ (<> "/" <> T.unpack p)
  b <.> e = b & the @"uriPath" %~ (<> "." <> T.unpack e)

instance (a ~ Char) => IsPath [a] [a] where
  b </> p = b FP.</> p
  b <.> e = b FP.<.> e

instance IsPath S3Uri Text where
  S3Uri b (AWS.ObjectKey k) </> p =
    S3Uri b (AWS.ObjectKey (stripEnd "/" k <> "/" <> stripStart "/" p))

  S3Uri b (AWS.ObjectKey k) <.> e =
    S3Uri b (AWS.ObjectKey (stripEnd "." k <> "." <> stripStart "." e))

toLocation :: Text -> Maybe Location
toLocation t = case URI.parseURI (T.unpack t) of
  Just uri -> Just (Uri uri)
  Nothing  -> Just (LocalFile (T.unpack t))

-------------------------------------------------------------------------------
stripStart :: Text -> Text -> Text
stripStart what txt = fromMaybe txt (T.stripPrefix what txt)

stripEnd :: Text -> Text -> Text
stripEnd what txt = fromMaybe txt (T.stripSuffix what txt)
