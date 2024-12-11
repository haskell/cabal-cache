module App.Amazonka
  ( mkAwsEnv
  ) where

import Effectful
import Effectful.Zoo.Amazonka.Data.AwsEnv
import Effectful.Zoo.Amazonka.Api.Discover
import Effectful.Zoo.Core
import HaskellWorks.Prelude
import HaskellWorks.Tuple

import qualified Amazonka                        as AWS
import qualified HaskellWorks.CabalCache.AWS.Env as AWS

mkAwsEnv :: ()
  => r <: IOE
  => AWS.Region
  -> Maybe (ByteString, Int, Bool)
  -> Maybe AWS.LogLevel
  -> Eff r AwsEnv
mkAwsEnv region mHostEndpoint awsLogLevel =
  liftIO (AWS.mkEnv region (AWS.awsLogger awsLogLevel))
    <&> maybe id (uncurry3 setAwsEnvEndpointOverride) mHostEndpoint
