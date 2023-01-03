{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptWarn
  , exceptWarn_
  , maybeToExcept
  , maybeToExcept_
  ) where

import Control.Algebra
import Control.Effect.Error (Error)
import Control.Monad.Except
import HaskellWorks.CabalCache.AppError

import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO
import qualified Control.Effect.Error               as FE

exceptWarn :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
exceptWarn f = catchError f handler
   where handler e = do
           liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
           throwError e

exceptWarn_ :: ()
  => MonadIO m
  => Has (Error AppError) sig m
  => m a
  -> m a
exceptWarn_ f = FE.catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
          FE.throwError e

maybeToExcept :: Monad m => AppError -> Maybe a -> ExceptT AppError m a
maybeToExcept message = maybe (throwError message) pure

maybeToExcept_ :: ()
  => Monad m
  => Has (Error AppError) sig m
  => AppError
  -> Maybe a
  -> m a
maybeToExcept_ message = maybe (FE.throwError message) pure
