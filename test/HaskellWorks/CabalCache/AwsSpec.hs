{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HaskellWorks.CabalCache.AwsSpec
  ( spec
  ) where

import Antiope.Core
import Antiope.Env
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe                       (isJust)
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.IO.Lazy
import HaskellWorks.CabalCache.RT       (runRT)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Network.HTTP.Types         as HTTP
import qualified Network.URI                as URI
import qualified System.Environment         as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.CabalCache.QuerySpec" $ do
  xit "stub" $ requireTest $ do
    ci <- liftIO $ IO.lookupEnv "CI" <&> isJust
    unless ci $ do
      envAws <- liftIO $ mkEnv Oregon (const LBSC.putStrLn)
      let Just uri = URI.parseURI "s3://jky-mayhem/hjddhd"
      result <- liftIO $ runRT $ headS3Uri envAws uri
      result === Left AwsAppError
        { status = HTTP.Status { HTTP.statusCode = 404 , HTTP.statusMessage = "Not Found" }
        }
