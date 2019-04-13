{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
module HaskellWorks.Ci.Assist.Location
( IsPath(..)
, Location(..)
, toLocation
)
where

import Antiope.Core (ToText (..), fromText)
import Antiope.S3   (BucketName, ObjectKey (..), S3Uri (..))
import Data.Maybe   (fromMaybe)
import Data.Text    (Text)
import GHC.Generics (Generic)

import qualified Data.Text       as Text
import qualified System.FilePath as FP

class IsPath a s | a -> s where
  (</>) :: a -> s -> a
  (<.>) :: a -> s -> a

infixr 5 </>
infixr 7 <.>

data Location
  = S3 S3Uri
  | Local FilePath
  deriving (Show, Eq, Generic)

instance ToText Location where
  toText (S3 uri)   = toText uri
  toText (Local p)  = Text.pack p

instance IsPath Location Text where
  (S3 b)    </> p = S3    (b </> p)
  (Local b) </> p = Local (b </> Text.unpack p)

  (S3 b)    <.> e = S3    (b <.> e)
  (Local b) <.> e = Local (b <.> Text.unpack e)

instance IsPath Text Text where
  b </> p = Text.pack (Text.unpack b FP.</> Text.unpack p)
  b <.> e = Text.pack (Text.unpack b FP.<.> Text.unpack e)

instance (a ~ Char) => IsPath [a] [a] where
  b </> p = b FP.</> p
  b <.> e = b FP.<.> e

instance IsPath S3Uri Text where
  S3Uri b (ObjectKey k) </> p =
    S3Uri b (ObjectKey (stripEnd "/" k <> "/" <> stripStart "/" p))

  S3Uri b (ObjectKey k) <.> e =
    S3Uri b (ObjectKey (stripEnd "." k <> "." <> stripStart "." e))

toLocation :: Text -> Maybe Location
toLocation txt = if
  | Text.isPrefixOf "s3://" txt'    -> either (const Nothing) (Just . S3) (fromText txt')
  | Text.isPrefixOf "file://" txt'  -> Just (Local (Text.unpack txt'))
  | Text.isInfixOf  "://" txt'      -> Nothing
  | otherwise                       -> Just (Local (Text.unpack txt'))
  where
    txt' = Text.strip txt

-------------------------------------------------------------------------------
stripStart :: Text -> Text -> Text
stripStart what txt = fromMaybe txt (Text.stripPrefix what txt)

stripEnd :: Text -> Text -> Text
stripEnd what txt = fromMaybe txt (Text.stripSuffix what txt)
