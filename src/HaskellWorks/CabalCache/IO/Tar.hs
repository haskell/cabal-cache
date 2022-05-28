{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.IO.Tar
  ( TarGroup(..)
  , createTar
  , extractTar
  ) where

import Control.DeepSeq                  (NFData)
import Control.Lens
import Control.Monad.Except
import Data.Generics.Product.Any
import GHC.Generics
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Show
import Polysemy (Member, Sem)

import qualified System.Exit    as IO
import qualified System.Process as IO
import qualified Polysemy.Embed as PY
import qualified Polysemy.Error as PY

data TarGroup = TarGroup
  { basePath   :: FilePath
  , entryPaths :: [FilePath]
  } deriving (Show, Eq, Generic, NFData)

createTar :: ()
  => Member (PY.Embed IO      ) r
  => Member (PY.Error AppError) r
  => FilePath
  -> [TarGroup]
  -> Sem r ()
createTar tarFile groups = do
  let args = ["-zcf", tarFile] <> foldMap tarGroupToArgs groups
  process <- liftIO $ IO.spawnProcess "tar" args
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> PY.throw $ GenericAppError $ "Failed to create tar. Exit code: " <> tshow n

extractTar :: ()
  => Member (PY.Embed IO      ) r
  => Member (PY.Error AppError) r
  => FilePath
  -> FilePath
  -> Sem r ()
extractTar tarFile targetPath = do
  process <- liftIO $ IO.spawnProcess "tar" ["-C", targetPath, "-zxf", tarFile]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> PY.throw $ GenericAppError $ "Failed to extract tar.  Exit code: " <> tshow n

tarGroupToArgs :: TarGroup -> [String]
tarGroupToArgs tarGroup = ["-C", tarGroup ^. the @"basePath"] <> tarGroup ^. the @"entryPaths"
