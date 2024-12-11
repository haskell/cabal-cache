module HaskellWorks.CabalCache.LocationSpec
  ( spec,
  ) where

import Data.Maybe                       (fromJust)
import HaskellWorks.CabalCache.Location
import HaskellWorks.Hspec.Hedgehog
import HaskellWorks.Prelude
import Hedgehog
import Network.URI                      (URI)
import Test.Hspec

import qualified Amazonka.Data    as AWS
import qualified Data.List        as L
import qualified Data.List        as List
import qualified Data.Text        as Text
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import qualified Network.URI      as URI
import qualified System.FilePath  as FP

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

s3Uri :: MonadGen m => m URI
s3Uri = do
  let partGen = Gen.string (Range.linear 3 10) Gen.alphaNum
  bkt <- partGen
  parts <- Gen.list (Range.linear 1 5) partGen
  ext <- Gen.string (Range.linear 2 4) Gen.alphaNum
  pure $ fromJust $ URI.parseURI $ "s3://" <> bkt <> "/" <> L.intercalate "/" parts <> "." <> ext
localPath :: MonadGen m => m FilePath
localPath = do
  let partGen = Gen.string (Range.linear 3 10) Gen.alphaNum
  parts <- Gen.list (Range.linear 1 5) partGen
  ext <- Gen.string (Range.linear 2 4) Gen.alphaNum
  pure $ "/" <> List.intercalate "/" parts <> "." <> ext

spec :: Spec
spec = describe "HaskellWorks.Assist.LocationSpec" do
  it "URI bucket-only" $ requireTest do
    fromJust (URI.parseURI "s3://bucket") </> "directory" === fromJust (URI.parseURI "s3://bucket/directory")

  it "Location bucket-only" $ requireTest do
    fromJust (toLocation "s3://bucket") </> "directory" === fromJust (toLocation "s3://bucket/directory")

  it "S3 should roundtrip from and to text" $ require $ property do
    uri <- forAll s3Uri
    tripping (Uri uri) AWS.toText toLocation

  it "LocalLocation should roundtrip from and to text" $ require $ property do
    path <- forAll localPath
    tripping (LocalFile path) AWS.toText toLocation

  it "Should append s3 path" $ require $ property do
    loc  <- Uri <$> forAll s3Uri
    part <- forAll $ Gen.text (Range.linear 3 10) Gen.alphaNum
    ext  <- forAll $ Gen.text (Range.linear 2 4)  Gen.alphaNum
    AWS.toText (loc </> part <.> ext) === AWS.toText loc <> "/" <> part <> "." <> ext

  it "Should append s3 path" $ require $ property do
    loc  <- LocalFile <$> forAll localPath
    part <- forAll $ Gen.string (Range.linear 3 10) Gen.alphaNum
    ext  <- forAll $ Gen.string (Range.linear 2 4)  Gen.alphaNum
    AWS.toText (loc </> Text.pack part <.> Text.pack ext) === Text.pack ((Text.unpack $ AWS.toText loc) FP.</> part FP.<.> ext)
