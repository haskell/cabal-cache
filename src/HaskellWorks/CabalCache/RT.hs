{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellWorks.CabalCache.RT
  ( RT(..)
  , runRT
  ) where

import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ask, MonadReader (local))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, createInternalState)
import Control.Monad.IO.Unlift (MonadIO(..), MonadUnliftIO(withRunInIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))

import qualified Data.IORef as IO
import qualified Control.Monad.Trans.Resource.Internal as RI
import qualified Control.Exception as E

newtype RT m a = RT { unRT :: IO.IORef RI.ReleaseMap -> m a }

runRT :: MonadUnliftIO m => RT m a -> m a
runRT (RT r) = withRunInIO $ \run -> do
  istate <- createInternalState
  E.mask $ \restore -> do
    res <- restore (run (r istate)) `E.catch` \e -> do
      RI.stateCleanupChecked (Just e) istate
      E.throwIO e
    RI.stateCleanupChecked Nothing istate
    return res

instance Functor m => Functor (RT m) where
  fmap f (RT m) = RT $ \r -> fmap f (m r)

instance Applicative m => Applicative (RT m) where
  pure = RT . const . pure
  RT mf <*> RT ma = RT $ \r -> mf r <*> ma r
  RT mf  *> RT ma = RT $ \r -> mf r  *> ma r
  RT mf <*  RT ma = RT $ \r -> mf r <*  ma r

instance Monad m => Monad (RT m) where
  return = pure
  RT ma >>= f = RT $ \r -> do
    a <- ma r
    let RT f' = f a
    f' r

instance MonadTrans RT where
  lift = RT . const

instance MonadReader r m => MonadReader r (RT m) where
  ask = lift ask
  local = mapRT . local

mapRT :: (m a -> n b) -> RT m a -> RT n b
mapRT f = RT . (f .) . unRT

instance MonadUnliftIO m => MonadUnliftIO (RT m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    RT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip unRT r)

instance MonadIO m => MonadIO (RT m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (RT m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (RT m) where
  catch (RT m) c = RT $ \r -> m r `catch` \e -> unRT (c e) r

instance (MonadIO m, MonadIO (RT m)) => MonadResource (RT m) where
  liftResourceT = transRT liftIO

transRT :: (m a -> n b) -> ResourceT m a -> RT n b
transRT f (RI.ResourceT mx) = RT (f . mx)
