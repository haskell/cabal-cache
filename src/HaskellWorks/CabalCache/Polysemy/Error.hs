{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}

module HaskellWorks.CabalCache.Polysemy.Error
  ( FatalError(..)
  , absorbError
  , onError
  , onFatalErrorExit
  , onSomeExceptionErrorExit
  ) where

import Control.Exception (SomeException)
import Polysemy (Member, Sem)
import Polysemy.Error (Error)

import qualified Polysemy.Embed as PY
import qualified Polysemy.Error as PY
import qualified System.Exit    as IO
import qualified System.IO      as IO

data FatalError = FatalError

absorbError :: forall e a r. ()
  => (e -> Sem r a)
  -> Sem (Error e ': r) a
  -> Sem r a
absorbError h f = do
  result <- PY.runError f
  case result of
    Right a -> return a
    Left e -> h e
{-# INLINE absorbError #-}

onError :: forall e r a. ()
  => Member (Error e) r
  => (e -> Sem r a)
  -> Sem r a
  -> Sem r a
onError = flip PY.catch
{-# INLINE onError #-}

onFatalErrorExit :: ()
  => Member (PY.Embed IO) r
  => Sem (Error FatalError ': r) a
  -> Sem r a
onFatalErrorExit f = do
  result <- PY.runError @FatalError f
  case result of
    Right a         -> return a
    Left FatalError -> PY.embed IO.exitFailure

onSomeExceptionErrorExit :: ()
  => Member (PY.Embed IO) r
  => Sem (Error SomeException ': r) a
  -> Sem r a
onSomeExceptionErrorExit f = do
  result <- PY.runError @SomeException f
  case result of
    Right a -> return a
    Left e  -> do
      PY.embed $ IO.hPrint IO.stderr e
      PY.embed IO.exitFailure
