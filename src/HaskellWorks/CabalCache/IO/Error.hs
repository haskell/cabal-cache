{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptFatal
  , exceptWarn
  , maybeToExcept
  , maybeToExceptM
  , catchErrno
  ) where

import Control.Monad.Except (void, MonadIO(liftIO), ExceptT(..), MonadTrans(lift), MonadError(catchError, throwError))
import Foreign.C.Error (Errno, getErrno)
import HaskellWorks.CabalCache.AppError (displayAppError, AppError)
import Polysemy (Member, Sem)
import System.IO.Error (catchIOError)

import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Polysemy.Embed                     as PY
import qualified Polysemy.Error                     as PY
import qualified Polysemy.Resource                  as PY
import qualified System.Exit                        as IO
import qualified System.IO                          as IO

exceptFatal :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => ExceptT AppError (Sem r) a
  -> ExceptT AppError (Sem r) a
exceptFatal f = catchError f handler
  where handler e = do
          lift . CIO.hPutStrLn IO.stderr $ "Fatal Error: " <> displayAppError e
          void $ liftIO IO.exitFailure
          throwError e

exceptWarn :: ()
  => Member (PY.Embed IO) r
  => Member (PY.Error AppError) r
  => Member (PY.Resource) r
  => Sem r a
  -> Sem r a
exceptWarn f = PY.catch f handler
  where handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
          PY.throw e

maybeToExcept :: ()
  => Member (PY.Error AppError) r
  => AppError
  -> Maybe a
  -> Sem r a
maybeToExcept message = maybe (PY.throw message) pure

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
