{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HaskellWorks.CabalCache.AwsSpec
  ( spec
  ) where

import Antiope.Core (Region(Oregon))
import Antiope.Env (mkEnv)
import Control.Exception (SomeException)
import Control.Lens ((<&>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (isJust)
import HaskellWorks.CabalCache.AppError (AppError(AwsAppError, status))
import HaskellWorks.CabalCache.IO.Lazy (headS3Uri)
import Hedgehog ((===))
import Test.Hspec (Spec, describe, xit)

import qualified Data.ByteString.Lazy.Char8   as LBSC
import qualified HaskellWorks.Hspec.Hedgehog  as H
import qualified Network.HTTP.Types           as HTTP
import qualified Network.URI                  as URI
import qualified Polysemy                     as PY
import qualified Polysemy.Error               as PY
import qualified Polysemy.Managed             as PY
import qualified Polysemy.Resource            as PY
import qualified System.Environment           as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.CabalCache.QuerySpec" $ do
  xit "stub" $ H.requireTest $ do
    ci <- liftIO $ IO.lookupEnv "CI" <&> isJust
    unless ci $ do
      envAws <- liftIO $ mkEnv Oregon (const LBSC.putStrLn)
      let Just uri = URI.parseURI "s3://jky-mayhem/hjddhd"
      result <- liftIO $ PY.runFinal
        . PY.resourceToIOFinal
        . PY.embedToFinal
        . PY.runManaged
        . PY.runError @String
        . PY.mapError (show @SomeException)
        . PY.runError @AppError
        $ headS3Uri envAws uri
      result === Right (Left AwsAppError
        { status = HTTP.Status { HTTP.statusCode = 404 , HTTP.statusMessage = "Not Found" }
        })
