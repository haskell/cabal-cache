{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module HaskellWorks.CabalCache.QuerySpec
  ( spec
  ) where

import Control.Lens
import Data.Generics.Product.Any
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec
import Text.RawString.QQ

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as LBS
import qualified HaskellWorks.CabalCache.Types as Z

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Assist.QuerySpec" $ do
  it "stub" $ requireTest $ do
    let Right planJson = A.eitherDecode exampleJson
    planJson === Z.PlanJson
      { Z.compilerId  = "ghc-8.6.4"
      , Z.installPlan =
        [ Z.Package
          { Z.packageType   = "pre-existing"
          , Z.id            = "Cabal-2.4.0.1"
          , Z.name          = "Cabal"
          , Z.version       = "2.4.0.1"
          , Z.style         = Nothing
          , Z.componentName = Nothing
          , Z.depends       =
            [ "array-0.5.3.0"
            , "base-4.12.0.0"
            ]
          }
        ]
      }

exampleJson :: LBS.ByteString
exampleJson = [r|
{
  "cabal-version": "2.4.1.0",
  "cabal-lib-version": "2.4.1.0",
  "compiler-id": "ghc-8.6.4",
  "os": "osx",
  "arch": "x86_64",
  "install-plan": [
    {
      "type": "pre-existing",
      "id": "Cabal-2.4.0.1",
      "pkg-name": "Cabal",
      "pkg-version": "2.4.0.1",
      "depends": [
        "array-0.5.3.0",
        "base-4.12.0.0"
      ]
    }
  ]
}
|]
