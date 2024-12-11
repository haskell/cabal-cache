module HaskellWorks.CabalCache.Concurrent.DownloadQueue
  ( DownloadStatus(..),
    createDownloadQueue,
    runQueue,
    downloadSucceed,
    downloadFail,
  ) where

import Control.Monad.Catch          (MonadMask(..))
import Data.Set                     ((\\))
import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Prelude                      hiding (fail)
import HaskellWorks.Prelude

import qualified Control.Concurrent.STM                  as STM
import qualified Control.Monad.Catch                     as CMC
import qualified Data.Relation                           as R
import qualified Data.Set                                as S
import qualified HaskellWorks.CabalCache.Concurrent.Type as Z
import qualified HaskellWorks.CabalCache.IO.Console      as CIO
import qualified System.IO                               as IO

data DownloadStatus = DownloadSuccess | DownloadFailure deriving (Eq, Show)

downloadSucceed :: forall a r. ()
  => r <: Error DownloadStatus
  => Eff r a
downloadSucceed =
  throw DownloadSuccess

downloadFail :: forall a r. ()
  => r <: Error DownloadStatus
  => Eff r a
downloadFail =
  throw DownloadFailure

createDownloadQueue :: [(Z.ProviderId, Z.ConsumerId)] -> STM.STM Z.DownloadQueue
createDownloadQueue dependencies = do
  tDependencies <- STM.newTVar (R.fromList dependencies)
  tUploading    <- STM.newTVar S.empty
  tFailures     <- STM.newTVar S.empty
  return Z.DownloadQueue {..}

takeReady :: Z.DownloadQueue -> STM.STM (Maybe Z.PackageId)
takeReady Z.DownloadQueue {..} = do
  dependencies  <- STM.readTVar tDependencies
  uploading     <- STM.readTVar tUploading
  failures      <- STM.readTVar tFailures

  let ready = R.ran dependencies \\ R.dom dependencies \\ uploading \\ failures

  case S.lookupMin ready of
    Just packageId -> do
      STM.writeTVar tUploading (S.insert packageId uploading)
      return (Just packageId)
    Nothing -> if S.null (R.ran dependencies \\ R.dom dependencies \\ failures)
      then return Nothing
      else STM.retry

commit :: Z.DownloadQueue -> Z.PackageId -> STM.STM ()
commit Z.DownloadQueue {..} packageId = do
  dependencies  <- STM.readTVar tDependencies
  uploading     <- STM.readTVar tUploading

  STM.writeTVar tUploading    $ S.delete packageId uploading
  STM.writeTVar tDependencies $ R.withoutRan (S.singleton packageId) dependencies

failDownload :: Z.DownloadQueue -> Z.PackageId -> STM.STM ()
failDownload Z.DownloadQueue {..} packageId = do
  uploading <- STM.readTVar tUploading
  failures  <- STM.readTVar tFailures

  STM.writeTVar tUploading  $ S.delete packageId uploading
  STM.writeTVar tFailures   $ S.insert packageId failures

runQueue :: (MonadIO m, MonadMask m) => Z.DownloadQueue -> (Z.PackageId -> m DownloadStatus) -> m ()
runQueue downloadQueue f = do
  maybePackageId <- liftIO $ STM.atomically $ takeReady downloadQueue

  case maybePackageId of
    Just packageId -> do
      downloadStatus <- f packageId
        & do CMC.handleAll \e -> do
              liftIO $ CIO.hPutStrLn IO.stderr $ "Warning: Unexpected exception during download of " <> packageId <> ": " <> tshow e
              liftIO $ IO.hFlush IO.stderr
              pure DownloadFailure
      case downloadStatus of
        DownloadSuccess -> do
          liftIO $ CIO.hPutStrLn IO.stderr $ "Downloaded " <> packageId
          liftIO $ STM.atomically $ commit downloadQueue packageId
        DownloadFailure -> do
          liftIO $ CIO.hPutStrLn IO.stderr $ "Failed to download " <> packageId
          liftIO $ STM.atomically $ failDownload downloadQueue packageId
      runQueue downloadQueue f

    Nothing -> return ()
