{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.CabalCache.Config
  ( CabalCacheConfig
  , Plugin

  , defaultConfigFile
  , loadConfigFile
  ) where

import HaskellWorks.CabalCache.Config.Types (CabalCacheConfig, Plugin, defaultConfig)
import System.FilePath                      ((</>))

import qualified Data.Yaml        as Y
import qualified System.Directory as IO
import qualified System.Exit      as IO
import qualified System.IO        as IO
import qualified System.IO.Unsafe as IO

defaultConfigFile :: FilePath
defaultConfigFile = IO.unsafePerformIO $ do
  home <- IO.getHomeDirectory
  return $ home </> ".cabal-cache.yaml"
{-# NOINLINE defaultConfigFile #-}

loadConfigFile :: FilePath -> IO CabalCacheConfig
loadConfigFile configFile = do
  exists <- IO.doesFileExist configFile

  if exists
    then do
      result <- Y.decodeFileEither configFile
      case result of
        Right config -> return config
        Left exception -> do
          IO.hPutStrLn IO.stderr $ "Config file error: " <> show exception
          IO.exitFailure
    else return defaultConfig
