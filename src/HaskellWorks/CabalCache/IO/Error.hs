{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptFatal
  , exceptWarn
  , maybeToExcept
  , maybeToExceptM
  , catchErrno
  ) where

import Control.Monad                    (void)
import Control.Monad.Except             (MonadError(throwError, catchError), MonadIO(..), ExceptT(..))
import Foreign.C.Error                  (Errno, getErrno)
import HaskellWorks.CabalCache.AppError (displayAppError, AppError)
import System.IO.Error                  (catchIOError)

import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.Exit                        as IO
import qualified System.IO                          as IO

exceptFatal :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
exceptFatal f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Fatal Error: " <> displayAppError e
          void $ liftIO IO.exitFailure
          throwError e

exceptWarn :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
exceptWarn f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
          throwError e

maybeToExcept :: Monad m => AppError -> Maybe a -> ExceptT AppError m a
maybeToExcept message = maybe (throwError message) pure

maybeToExceptM :: Monad m => AppError -> m (Maybe a) -> ExceptT AppError m a
maybeToExceptM message = ExceptT . fmap (maybe (Left message) Right)


-- |Carries out an action, then checks if there is an IOException and
-- a specific errno. If so, then it carries out another action, otherwise
-- it rethrows the error.
catchErrno :: [Errno] -- ^ errno to catch
           -> IO a    -- ^ action to try, which can raise an IOException
           -> IO a    -- ^ action to carry out in case of an IOException and
                      --   if errno matches
           -> IO a
catchErrno en a1 a2 =
  catchIOError a1 $ \e -> do
    errno <- getErrno
    if errno `elem` en
      then a2
      else ioError e
