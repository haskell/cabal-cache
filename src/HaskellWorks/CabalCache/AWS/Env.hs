{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.CabalCache.AWS.Env
  ( awsLogger
  , mkEnv
  , setEnvEndpoint
  ) where

import Control.Concurrent           (myThreadId)
import Data.ByteString.Builder      (toLazyByteString)
import Data.Generics.Product.Any    (the)
import HaskellWorks.Prelude
import Lens.Micro
import Network.HTTP.Client          (HttpException (..), HttpExceptionContent (..))

import qualified Amazonka                           as AWS
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text.Encoding                 as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

setEnvEndpoint :: Maybe (ByteString, Int, Bool) -> IO AWS.Env -> IO AWS.Env
setEnvEndpoint mHostEndpoint getEnv = do
  env <- getEnv
  case mHostEndpoint of
    Just (host, port, ssl) ->
      pure $ env
        & the @"overrides" .~ \svc -> do
            svc & the @"endpoint" %~ \mkEndpoint region -> do
              mkEndpoint region
                & the @"host" .~ host
                & the @"port" .~ port
                & the @"secure" .~ ssl
    Nothing -> pure env

mkEnv :: AWS.Region -> (AWS.LogLevel -> LBS.ByteString -> IO ()) -> IO AWS.Env
mkEnv region lg = do
  lgr <- newAwsLogger lg
  discoveredEnv <- AWS.newEnv AWS.discover

  pure discoveredEnv
    { AWS.logger = lgr
    , AWS.region = region
    , AWS.retryCheck = retryPolicy 5
    }

newAwsLogger :: Monad m => (AWS.LogLevel -> LBS.ByteString -> IO ()) -> m AWS.Logger
newAwsLogger lg = return $ \y b ->
  let lazyMsg = toLazyByteString b
  in case L.toStrict lazyMsg of
      msg | BS.isInfixOf "404 Not Found" msg    -> lg AWS.Debug lazyMsg
      msg | BS.isInfixOf "304 Not Modified" msg -> lg AWS.Debug lazyMsg
      _                                         -> lg y lazyMsg

retryPolicy :: Int -> Int -> AWS.HttpException -> Bool
retryPolicy maxNum attempt ex = (attempt <= maxNum) && shouldRetry ex

shouldRetry :: AWS.HttpException -> Bool
shouldRetry ex = case ex of
  HttpExceptionRequest _ ctx -> case ctx of
    ResponseTimeout          -> True
    ConnectionTimeout        -> True
    ConnectionFailure _      -> True
    InvalidChunkHeaders      -> True
    ConnectionClosed         -> True
    InternalException _      -> True
    NoResponseDataReceived   -> True
    ResponseBodyTooShort _ _ -> True
    _                        -> False
  _ -> False

awsLogger :: Maybe AWS.LogLevel -> AWS.LogLevel -> LC8.ByteString -> IO ()
awsLogger maybeConfigLogLevel msgLogLevel message =
  forM_ maybeConfigLogLevel $ \configLogLevel ->
    when (msgLogLevel <= configLogLevel) do
      threadId <- myThreadId
      CIO.hPutStrLn IO.stderr $ "[" <> tshow msgLogLevel <> "] [tid: " <> tshow threadId <> "]"  <> text
  where text = T.decodeUtf8 $ LBS.toStrict message
