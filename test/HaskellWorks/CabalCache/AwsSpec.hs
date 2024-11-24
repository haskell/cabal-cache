{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HaskellWorks.CabalCache.AwsSpec
  ( spec
  ) where

import HaskellWorks.CabalCache.AppError (AwsError(..))
import HaskellWorks.CabalCache.Error    (UnsupportedUri)
import HaskellWorks.Hspec.Hedgehog
import HaskellWorks.Prelude
import Hedgehog
import Test.Hspec

import qualified Amazonka                         as AWS
import qualified Control.Monad.Oops               as OO
import qualified Data.ByteString.Lazy.Char8       as LBSC
import qualified Data.Variant                     as OO
import qualified HaskellWorks.CabalCache.AWS.Env  as AWS
import qualified HaskellWorks.CabalCache.AWS.S3   as AWS
import qualified Hedgehog                         as H
import qualified Network.HTTP.Types               as HTTP
import qualified Network.URI                      as URI
import qualified System.Environment               as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.CabalCache.QuerySpec" do
  it "stub" $ requireTest do
    ci <- liftIO $ IO.lookupEnv "CI" <&> isJust
    unless ci do
      envAws <- liftIO $ AWS.mkEnv AWS.Oregon (const LBSC.putStrLn)
      let Just uri = URI.parseURI "s3://cache.haskellworks.io/hjddhd"
      result :: Either (OO.Variant '[AwsError, UnsupportedUri]) ()
        <- liftIO $ runExceptT $ OO.suspend AWS.runResourceT $ void (AWS.headS3Uri envAws uri)

      case result of
        Right _ -> H.failure
        Left e -> OO.toEithers e === Left AwsError
          { status = HTTP.Status { HTTP.statusCode = 404 , HTTP.statusMessage = "Not Found" }
          }
