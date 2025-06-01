module HaskellWorks.CabalCache.AwsSpec
  ( spec
  ) where

import Data.Generics.Product.Any
import Effectful
import Effectful.Concurrent
import Effectful.Environment
import Effectful.Resource
import Effectful.Zoo.Amazonka.Api.Discover
import Effectful.Zoo.Amazonka.Data.AwsEnv
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Amazonka.Data.AwsLogEntry
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api
import Effectful.Zoo.Hedgehog.Data
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Hedgehog.Effect.Run
import Effectful.Zoo.Lazy.Dynamic
import HaskellWorks.CabalCache.AppError (AwsStatusError(..))
import HaskellWorks.CabalCache.Error    (UnsupportedUri)
import HaskellWorks.Prelude
import Lens.Micro

import qualified HaskellWorks.CabalCache.AWS.S3   as AWS
import qualified Network.URI                      as URI

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

data Success = Success deriving Show

catchSuccess :: forall r. ()
  => r <: Hedgehog
  => r <: IOE
  => Eff (Error Success : r) Void
  -> Eff r ()
catchSuccess f = do
  result <- f & runError @Success
  case result of
    Left (_, Success) -> pure ()
    Right a -> absurd a

runTestEnv :: forall a r. ()
  => r <: IOE
  => Eff
      ( Lazy AwsEnv
      : Concurrent
      : Environment
      : Resource
      : r
      ) a
  -> Eff r a
runTestEnv f =
  f
    & runLazy discoverAwsEnv
    & runConcurrent
    & runEnvironment
    & runResource

spec :: UnitTest
spec = unit $ runTestEnv $ catchSuccess $ do
    uri <- URI.parseURI "s3://cache.haskellworks.io/test/cabal-cache/ci"
      & onNothingFail

    void (AWS.headS3Uri uri)
      & jotShowDataLog @AwsLogEntry
      & do trap @AwsStatusError \e -> do
            assert $ e.status ^. the @"statusCode" == 404 || e.status ^. the @"statusCode" == 301
            throw Success
      & do trap_ @AwsError failure
      & do trap_ @AwsStatusError failure
      & do trap_ @UnsupportedUri failure
    
    failure
