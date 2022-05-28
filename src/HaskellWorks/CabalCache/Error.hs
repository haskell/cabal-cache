{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.CabalCache.Error
  ( nothingToError
  ) where

import Polysemy (Member, Sem)

import qualified Polysemy.Error as PY

nothingToError :: ()
  => Member (PY.Error e) r
  => e
  -> Maybe a
  -> Sem r a
nothingToError _ (Just a) = return a
nothingToError e Nothing  = PY.throw e
