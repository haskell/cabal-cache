{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.CabalCache.Aws.Env
  ( awsLogger
  ) where

import Antiope.Env (LogLevel (..))
import Control.Concurrent (myThreadId)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import HaskellWorks.CabalCache.Show (tshow)
import Polysemy (Member, Sem)

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text.Encoding                 as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Polysemy.Embed                     as PY
import qualified Polysemy.Resource                  as PY
import qualified System.IO                          as IO

awsLogger :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => Maybe LogLevel
  -> LogLevel
  -> LC8.ByteString
  -> Sem r ()
awsLogger maybeConfigLogLevel msgLogLevel message =
  forM_ maybeConfigLogLevel $ \configLogLevel ->
    when (msgLogLevel <= configLogLevel) $ do
      threadId <- liftIO myThreadId
      CIO.hPutStrLn IO.stderr $ "[" <> tshow msgLogLevel <> "] [tid: " <> tshow threadId <> "]"  <> text
  where text = T.decodeUtf8 $ LBS.toStrict message
