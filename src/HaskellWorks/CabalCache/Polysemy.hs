{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Redundant id"      -}

module HaskellWorks.CabalCache.Polysemy
  ( runMainEffects
  ) where

import Polysemy (Embed, Sem, Final)
import Polysemy.Resource (Resource)

import qualified Polysemy           as PY
import qualified Polysemy.Resource  as PY

runMainEffects :: Sem '[Embed IO, Resource, Final IO] a -> IO a
runMainEffects = PY.runFinal
  . PY.resourceToIOFinal
  . PY.embedToFinal
