{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.Aws.Env
  ( awsLogger
  ) where

import Antiope.Env                  (LogLevel (..))
import Control.Concurrent           (myThreadId)
import Control.Monad                (when, forM_)
import HaskellWorks.CabalCache.Show (tshow)

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text.Encoding                 as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

awsLogger :: Maybe LogLevel -> LogLevel -> LC8.ByteString -> IO ()
awsLogger maybeConfigLogLevel msgLogLevel message =
  forM_ maybeConfigLogLevel $ \configLogLevel ->
    when (msgLogLevel <= configLogLevel) $ do
      threadId <- myThreadId
      CIO.hPutStrLn IO.stderr $ "[" <> tshow msgLogLevel <> "] [tid: " <> tshow threadId <> "]"  <> text
  where text = T.decodeUtf8 $ LBS.toStrict message
