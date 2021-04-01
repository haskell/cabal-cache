{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.CabalCache.Plugin
  ( pluginProc
  , nothingAsError
  , withPluginProcess
  , pluginPut
  , pluginGet
  , pluginHead
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Generics.Product.Any
import HaskellWorks.CabalCache.Config (CabalCacheConfig)
import HaskellWorks.CabalCache.Error
import Network.URI                    (URI)
import System.Exit                    (ExitCode (ExitFailure, ExitSuccess))
import System.IO                      (Handle)
import System.Process                 (ProcessHandle)

import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.List                            as L
import           HaskellWorks.CabalCache.Config.Types (Plugin)
import qualified System.Process                       as IO

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

lookupPluginExe :: CabalCacheConfig -> URI -> Maybe FilePath
lookupPluginExe cabalCacheConfig uri = fmap (^. the @"binary") (L.find predicate plugins)
  where plugins :: [Plugin]
        plugins = cabalCacheConfig ^. the @"plugins"
        predicate :: Plugin -> Bool
        predicate plugin = show (plugin ^. the @"uriPrefix") `L.isPrefixOf` show uri

pluginPut :: CabalCacheConfig -> URI -> String -> LBS.ByteString -> ExceptT PluginError IO ()
pluginPut cabalCacheConfig uri subKey lbs = do
  exe <- lookupPluginExe cabalCacheConfig uri & nothingToError (PluginError "")

  let cp = pluginProc exe
        [ "put"
        , "--uri"     , show uri
        , "--sub-key" , subKey
        ]

  (mStdin, mStdout, mStderr, hProcess) <- liftIO $ IO.createProcess cp

  _       <- mStdin   & nothingToError (PluginError "")
  stdout  <- mStdout  & nothingToError (PluginError "")
  _       <- mStderr  & nothingToError (PluginError "")

  liftIO $ LBS.hPut stdout lbs

  exitCode <- liftIO $ IO.waitForProcess hProcess

  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure n -> throwE $ PluginError $ "Plugin failed with error code " <> show n <> " during put"

pluginGet :: CabalCacheConfig -> URI -> String -> ExceptT PluginError IO LBS.ByteString
pluginGet cabalCacheConfig uri subKey = do
  exe <- lookupPluginExe cabalCacheConfig uri & nothingToError (PluginError "")

  let cp = pluginProc exe
        [ "get"
        , "--uri"     , show uri
        , "--sub-key" , subKey
        ]

  (mStdin, mStdout, mStderr, hProcess) <- liftIO $ IO.createProcess cp

  _       <- mStdin   & nothingToError (PluginError "")
  stdout  <- mStdout  & nothingToError (PluginError "")
  _       <- mStderr  & nothingToError (PluginError "")

  exitCode <- liftIO $ IO.waitForProcess hProcess

  case exitCode of
    ExitSuccess   -> liftIO $ LBS.hGetContents stdout
    ExitFailure n -> throwE $ PluginError $ "Plugin failed with error code " <> show n <> " during get"

pluginHead :: CabalCacheConfig -> URI -> String -> ExceptT PluginError IO ()
pluginHead cabalCacheConfig uri subKey = do
  exe <- lookupPluginExe cabalCacheConfig uri & nothingToError (PluginError "")

  let cp = pluginProc exe
        [ "head"
        , "--uri"     , show uri
        , "--sub-key" , subKey
        ]

  (mStdin, mStdout, mStderr, hProcess) <- liftIO $ IO.createProcess cp

  _ <- mStdin   & nothingToError (PluginError "")
  _ <- mStdout  & nothingToError (PluginError "")
  _ <- mStderr  & nothingToError (PluginError "")

  exitCode <- liftIO $ IO.waitForProcess hProcess

  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure n -> throwE $ PluginError $ "Plugin failed with error code " <> show n <> " during head"
