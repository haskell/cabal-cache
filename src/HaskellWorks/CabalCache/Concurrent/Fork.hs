module HaskellWorks.CabalCache.Concurrent.Fork where

import Control.Exception (finally)
import Control.Monad

import qualified Control.Concurrent     as IO
import qualified Control.Concurrent.STM as STM

forkThreadsWait :: Int -> IO () -> IO ()
forkThreadsWait n f = do
  tDone <- STM.atomically $ STM.newTVar (0 :: Int)
  forM_ [1 .. n] $ \_ -> IO.forkIO $ do
    f `finally` (STM.atomically $ STM.modifyTVar tDone (+1))

  STM.atomically $ do
    done <- STM.readTVar tDone
    when (done < n) STM.retry
