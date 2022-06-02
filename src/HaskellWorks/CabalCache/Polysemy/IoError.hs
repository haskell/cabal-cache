{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HaskellWorks.CabalCache.Polysemy.IoError
  ( IoError(..)

  , withIoError
  , withException
  , lowerIoError
  , ioCatch
  , ioThrow
  ) where

import Polysemy (Member, Embed, Sem)
import Polysemy.Error (throw, Error)

import qualified Polysemy           as PY
import qualified Control.Exception  as E

data IoError m a where
  IoThrow :: E.Exception e => e -> IoError m a
  IoCatch :: E.Exception e => m a -> (e -> m a) -> IoError m a

PY.makeSem ''IoError

withIoError :: forall e r a . (E.Exception e, Member (Embed IO) r, Member (Error e) r) => IO a -> Sem r a
withIoError action = do
  res <- PY.embed $ E.try action
  case res of
       Left e  -> throw @e e
       Right x -> pure x

withException :: forall e r a . (E.Exception e, Member IoError r, Member (Error e) r) => Sem r a -> Sem r a
withException action = ioCatch @_ @e action throw

lowerIoError :: Member (Embed IO) r => (forall x. Sem r x -> IO x) -> Sem (IoError ': r) a -> Sem r a
lowerIoError lower = PY.interpretH $ \case
  IoThrow e -> PY.embed $ E.throwIO e
  IoCatch m h -> do
    m' <- lowerIoError lower <$> PY.runT m
    h' <- (lowerIoError lower .) <$> PY.bindT h
    s  <- PY.getInitialStateT
    PY.embed $ lower m' `E.catch` \e -> lower (h' (e <$ s))
