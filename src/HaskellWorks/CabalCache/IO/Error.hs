{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptFatal
  , exceptWarn
  , maybeToExcept
  , maybeToExceptM
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class
import HaskellWorks.CabalCache.AppError

import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.Exit                        as IO
import qualified System.IO                          as IO

exceptFatal :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
exceptFatal f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Fatal Error: " <> displayAppError e
          liftIO IO.exitFailure
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
