{-# LANGUAGE TupleSections #-}

module HaskellWorks.CabalCache.Metadata
  ( metaDir,
    createMetadata,
    loadMetadata,
    deleteMetadata,
  ) where

import HaskellWorks.CabalCache.Core   (PackageInfo (..))
import HaskellWorks.CabalCache.IO.Tar (TarGroup (..))
import HaskellWorks.Prelude
import System.FilePath                (takeFileName, (</>))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified System.Directory     as IO

metaDir :: String
metaDir = "_CC_METADATA"

createMetadata :: MonadIO m => FilePath -> PackageInfo -> [(T.Text, LBS.ByteString)] -> m TarGroup
createMetadata storePath pkg values = liftIO do
  let pkgMetaPath = storePath </> packageDir pkg </> metaDir
  IO.createDirectoryIfMissing True pkgMetaPath
  forM_ values $ \(k, v) -> LBS.writeFile (pkgMetaPath </> T.unpack k) v
  pure $ TarGroup storePath [packageDir pkg </> metaDir]

loadMetadata :: MonadIO m => FilePath -> m (Map.Map T.Text LBS.ByteString)
loadMetadata pkgStorePath = liftIO do
  let pkgMetaPath = pkgStorePath </> metaDir
  exists <- IO.doesDirectoryExist pkgMetaPath
  if not exists
    then pure Map.empty
    else IO.listDirectory pkgMetaPath
          <&> fmap (pkgMetaPath </>)
          >>= traverse (\mfile -> (T.pack (takeFileName mfile),) <$> LBS.readFile mfile)
          <&> Map.fromList

deleteMetadata :: MonadIO m => FilePath -> m ()
deleteMetadata pkgStorePath =
  liftIO $ IO.removeDirectoryRecursive (pkgStorePath </> metaDir)
