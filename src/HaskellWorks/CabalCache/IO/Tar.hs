module HaskellWorks.CabalCache.IO.Tar
  ( ArchiveError(..),
    TarGroup(..),
    createTar,
    extractTar,
  ) where

import Control.DeepSeq                  (NFData)
import Effectful
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Core
import HaskellWorks.Prelude

import qualified System.Exit        as IO
import qualified System.Process     as IO

newtype ArchiveError = ArchiveError Text deriving (Eq, Show, Generic)

data TarGroup = TarGroup
  { basePath   :: FilePath
  , entryPaths :: [FilePath]
  } deriving (Show, Eq, Generic, NFData)

createTar :: ()
  => r <: Error ArchiveError
  => r <: IOE
  => Foldable t
  => [Char]
  -> t TarGroup
  -> Eff r ()
createTar tarFile groups = do
  let args = ["-zcf", tarFile] <> foldMap tarGroupToArgs groups
  process <- liftIO $ IO.spawnProcess "tar" args
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throw $ ArchiveError $ "Failed to create tar. Exit code: " <> tshow n

extractTar :: ()
  => r <: Error ArchiveError
  => r <: IOE
  => String
  -> String
  -> Eff r ()
extractTar tarFile targetPath = do
  process <- liftIO $ IO.spawnProcess "tar" ["-C", targetPath, "-zxf", tarFile]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throw $ ArchiveError $ "Failed to extract tar.  Exit code: " <> tshow n

tarGroupToArgs :: TarGroup -> [String]
tarGroupToArgs tarGroup = ["-C", tarGroup.basePath] <> tarGroup.entryPaths
