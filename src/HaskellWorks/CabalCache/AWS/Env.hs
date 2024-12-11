module HaskellWorks.CabalCache.AWS.Env
  ( awsLogger
  , mkEnv
  ) where

import Control.Concurrent           (myThreadId)
import Data.ByteString.Builder      (toLazyByteString)
import Effectful.Zoo.Amazonka.Data.AwsEnv
import HaskellWorks.Prelude
import Network.HTTP.Client          (HttpException (..), HttpExceptionContent (..))

import qualified Amazonka                           as AWS
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text.Encoding                 as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

mkEnv :: AWS.Region -> (AWS.LogLevel -> LBS.ByteString -> IO ()) -> IO AwsEnv
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
