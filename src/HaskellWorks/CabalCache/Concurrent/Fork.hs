{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.Concurrent.Fork where

import Control.Exception            (finally)
import Control.Monad
import HaskellWorks.CabalCache.Show (tshow)

import qualified Control.Concurrent                 as IO
import qualified Control.Concurrent.STM             as STM
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

forkThreadsWait :: Int -> IO () -> IO ()
forkThreadsWait n f = do
  tDone <- STM.atomically $ STM.newTVar (0 :: Int)
  forM_ [1 .. n] $ \_ -> IO.forkFinally
    do f `finally` STM.atomically (STM.modifyTVar tDone (+1))
    \case
      Left e -> do
        CIO.hPutStrLn IO.stderr $ "Fatal exception occurred: " <> tshow e
        CIO.hPutStrLn IO.stderr "Waiting for any remaining downlad threads to complete"
        STM.atomically $ do
          done <- STM.readTVar tDone
          when (done < n) STM.retry
      Right () -> do
        STM.atomically $ do
          done <- STM.readTVar tDone
          when (done < n) STM.retry
