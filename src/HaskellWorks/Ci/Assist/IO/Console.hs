module HaskellWorks.Ci.Assist.IO.Console
  ( putStrLn
  , print
  , hPutStrLn
  , hPrint
  ) where

import Control.Exception      (bracket_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)
import Prelude                (IO, Show (..), ($), (.))

import qualified Control.Concurrent.QSem as IO
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified System.IO               as IO
import qualified System.IO.Unsafe        as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . consoleBracket . T.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . consoleBracket . IO.print

hPutStrLn :: MonadIO m => IO.Handle -> Text -> m ()
hPutStrLn h = liftIO . consoleBracket . T.hPutStrLn h

hPrint :: (MonadIO m, Show a) => IO.Handle -> a -> m ()
hPrint h = liftIO . consoleBracket . IO.hPrint h
