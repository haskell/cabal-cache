{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.IO.Tar
  ( ArchiveError(..),
    TarGroup(..),
    createTar,
    extractTar,
  ) where

import Control.DeepSeq                  (NFData)
import Control.Monad.Except             (MonadError)
import Data.Generics.Product.Any        (HasAny(the))
import HaskellWorks.Prelude
import Lens.Micro

import qualified Control.Monad.Oops as OO
import qualified System.Exit        as IO
import qualified System.Process     as IO

data ArchiveError = ArchiveError Text deriving (Eq, Show, Generic)

data TarGroup = TarGroup
  { basePath   :: FilePath
  , entryPaths :: [FilePath]
  } deriving (Show, Eq, Generic, NFData)

createTar :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` ArchiveError
  => Foldable t
  => [Char]
  -> t TarGroup
  -> m ()
createTar tarFile groups = do
  let args = ["-zcf", tarFile] <> foldMap tarGroupToArgs groups
  process <- liftIO $ IO.spawnProcess "tar" args
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> OO.throw $ ArchiveError $ "Failed to create tar. Exit code: " <> tshow n

extractTar :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` ArchiveError
  => String
  -> String
  -> m ()
extractTar tarFile targetPath = do
  process <- liftIO $ IO.spawnProcess "tar" ["-C", targetPath, "-zxf", tarFile]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> OO.throw $ ArchiveError $ "Failed to extract tar.  Exit code: " <> tshow n

tarGroupToArgs :: TarGroup -> [String]
tarGroupToArgs tarGroup = ["-C", tarGroup ^. the @"basePath"] <> tarGroup ^. the @"entryPaths"
