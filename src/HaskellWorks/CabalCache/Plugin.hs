{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.CabalCache.Plugin
  ( pluginProc
  , nothingAsError
  , withPluginProcess
  , pluginPut
  , pluginGet
  , pluginHead
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Function                  ((&))
import HaskellWorks.CabalCache.Config (CabalCacheConfig)
import Network.URI                    (URI)
import System.IO                      (Handle)
import System.Process                 (ProcessHandle)

import qualified Data.ByteString.Lazy as LBS
import qualified System.IO            as IO
import qualified System.Process       as IO

newtype PluginError = PluginError String deriving (Eq, Show)

pluginProc :: FilePath -> [String] -> IO.CreateProcess
pluginProc exe args = (IO.proc exe args)
  { IO.std_in   = IO.CreatePipe
  , IO.std_out  = IO.CreatePipe
  , IO.std_err  = IO.CreatePipe
  }

nothingAsError :: Monad m => e -> Maybe a -> ExceptT e m a
nothingAsError e = maybe (throwE e) pure

withPluginProcess
  :: IO.CreateProcess
  -> (Handle -> Handle -> Handle -> ProcessHandle -> IO a)
  -> ExceptT PluginError IO a
withPluginProcess cp f = ExceptT $ do
  IO.withCreateProcess cp $ \mStdin mStdout mStderr hProcess -> runExceptT $ do
    stdin   <- mStdin   & nothingAsError (PluginError "Plugin stdin not open")
    stdout  <- mStdout  & nothingAsError (PluginError "Plugin stdout not open")
    stderr  <- mStderr  & nothingAsError (PluginError "Plugin stdout not open")
    liftIO $ f stdin stdout stderr hProcess

pluginPut :: CabalCacheConfig -> URI -> String -> LBS.ByteString -> ExceptT PluginError IO ()
pluginPut cabalCacheConfig uri subKey lbs = do
  let cp = pluginProc "cabal-cache-s3"
        [ "put"
        , "--uri"     , show uri
        , "--sub-key" , subKey
        ]

  withPluginProcess cp $ \stdin stdout _ _ -> do
    LBS.hPut stdout lbs
    IO.hClose stdin

pluginGet :: CabalCacheConfig -> URI -> String -> ExceptT PluginError IO LBS.ByteString
pluginGet cabalCacheConfig uri subKey = do
  let cp = pluginProc "cabal-cache-s3"
        [ "get"
        , "--uri"     , show uri
        , "--sub-key" , subKey
        ]

  -- TODO Handle closed too early
  withPluginProcess cp $ \stdin stdout _ _ -> do
    lbs <- LBS.hGetContents stdout
    IO.hClose stdin
    return lbs

pluginHead :: CabalCacheConfig -> URI -> String -> ExceptT PluginError IO LBS.ByteString
pluginHead cabalCacheConfig uri subKey = do
  let cp = pluginProc "cabal-cache-s3"
        [ "head"
        , "--uri"     , show uri
        , "--sub-key" , subKey
        ]

  withPluginProcess cp $ \stdout _ _ _ -> LBS.hGetContents stdout
