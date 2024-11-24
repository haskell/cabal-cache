{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HaskellWorks.CabalCache.IntegrationSpec
  ( spec
  ) where

import HaskellWorks.Prelude
import System.FilePath            ((</>))
import Test.Hspec                 (Spec, describe, it)

import qualified Data.List                    as L
import qualified Data.Time.Clock              as DT
import qualified Data.Time.Format             as DT
import qualified HaskellWorks.Hspec.Hedgehog  as H
import qualified Hedgehog                     as H
import qualified Hedgehog.Extras.Test.Base    as H
import qualified System.Directory             as IO
import qualified System.Environment           as IO
import qualified System.FilePath.Glob         as IO
import qualified Test.Base                    as H

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.CabalCache.IntegrationSpec" do
  it "local" $ H.require . H.withTests 1 $ H.integration $ H.runFinallies . H.workspace "local" $ \tempAbsBasePath' -> do
    cwd <- H.noteIO $ IO.getCurrentDirectory
    let archivePath = tempAbsBasePath' </> "archive"
    let storePath   = tempAbsBasePath' </> "store"
    let buildPath   = cwd </> "dist-newstyle"

    H.execCabalCache_
      [ "sync-to-archive"
      , "--archive-uri"
      , archivePath
      , "--build-path"
      , buildPath
      ]

    H.execCabalCache_
      [ "sync-from-archive"
      , "--archive-uri"
      , archivePath
      , "--store-path"
      , tempAbsBasePath' </> "store"
      , "--build-path"
      , buildPath
      ]

    archivedPackages <- H.noteShowIO $ IO.globDir1 (IO.compile "**/*.tar.gz") archivePath
    restoredPackages <- H.noteShowIO $ IO.globDir1 (IO.compile "**/cabal-hash.txt") storePath

    H.assert $ L.length archivedPackages > 20 -- At least some packages should have been archived
    H.assert $ L.length restoredPackages > 20 -- At least some packages should have been archived

  it "remote" $ H.require . H.withTests 1 $ H.integration $ H.runFinallies . H.workspace "local" $ \tempAbsBasePath' -> do
    cwd <- H.noteIO $ IO.getCurrentDirectory
    mBinaryCacheUri <- H.noteShowIO $ IO.lookupEnv "BINARY_CACHE_URI"

    forM_ mBinaryCacheUri \binaryCacheUri -> do
      now <- H.evalIO DT.getCurrentTime

      let formattedNow  = DT.formatTime DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      let storePath     = tempAbsBasePath' </> "store"
      let buildPath     = cwd </> "dist-newstyle"
      let archiveUri    = binaryCacheUri <> "/" <> "cabal-cache-test" <> "/" <> formattedNow

      H.execCabalCache_
        [ "sync-to-archive"
        , "--archive-uri"
        , archiveUri
        , "--build-path"
        , buildPath
        ]

      H.execCabalCache_
        [ "sync-from-archive"
        , "--archive-uri"
        , archiveUri
        , "--store-path"
        , tempAbsBasePath' </> "store"
        , "--build-path"
        , buildPath
        ]

      restoredPackages <- H.noteShowIO $ IO.globDir1 (IO.compile "**/cabal-hash.txt") storePath

      H.assert $ L.length restoredPackages > 20 -- At least some packages should have been archived
