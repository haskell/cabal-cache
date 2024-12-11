module HaskellWorks.CabalCache.QuerySpec
  ( spec,
  ) where

import HaskellWorks.Hspec.Hedgehog
import HaskellWorks.Prelude
import Hedgehog
import Test.Hspec
import Text.RawString.QQ

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as LBS
import qualified HaskellWorks.CabalCache.Types as Z

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.Assist.QuerySpec" do
  it "stub" $ requireTest do
    case A.eitherDecode exampleJson of
      Right planJson -> do
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
              , Z.components    = Nothing
              , Z.depends       =
                [ "array-0.5.3.0"
                , "base-4.12.0.0"
                ]
              , Z.exeDepends    = []
              }
            ]
          }
      Left msg -> fail msg

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
