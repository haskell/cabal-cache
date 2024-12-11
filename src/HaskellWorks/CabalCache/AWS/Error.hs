module HaskellWorks.CabalCache.AWS.Error
  ( handleAwsStatusError
  ) where

import Effectful
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import HaskellWorks.CabalCache.AppError (AwsStatusError(..))
import HaskellWorks.Prelude

import qualified Amazonka                             as AWS
import qualified Network.HTTP.Types                   as HTTP

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsStatusError :: ()
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => Eff r a
  -> Eff r a
handleAwsStatusError f = f & trapIn @AwsError \case
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status _ _) _ _ _ _)) -> throw $ AwsStatusError s
    e                                                                    -> throw e
