{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Redundant id"      -}

module HaskellWorks.CabalCache.Polysemy
  ( runMainEffects
  ) where

import Polysemy
import Polysemy.Resource (Resource, resourceToIOFinal)

runMainEffects :: Sem '[Embed IO, Resource, Final IO] a -> IO a
runMainEffects = runFinal
  . resourceToIOFinal
  . embedToFinal
