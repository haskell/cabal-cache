module HaskellWorks.CabalCache.Concurrent.Fork
  ( forkThreadsWait,
  ) where

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Exception
import Effectful.Zoo.Core
import HaskellWorks.Prelude

forkThreadsWait :: ()
  => r <: Concurrent
  => Int
  -> Eff r ()
  -> Eff r ()
forkThreadsWait n f = do
  tDone <- newTVarIO (0 :: Int)

  forM_ [1 .. n] $ \_ -> forkIO do
    f `finally` atomically (modifyTVar tDone (+1))

  atomically do
    done <- readTVar tDone
    when (done < n) retry
