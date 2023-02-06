{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.Concurrent.DownloadQueue
  ( DownloadStatus(..),
    createDownloadQueue,
    runQueue,
    succeed,
    fail,
  ) where

import Control.Monad.Catch          (MonadMask(..))
import Control.Monad.Except         (MonadError)
import Control.Monad.IO.Class       (MonadIO(liftIO))
import Data.Function                ((&))
import Data.Set                     ((\\))
import Prelude                      hiding (fail)

import qualified Control.Concurrent.STM                  as STM
import qualified Control.Monad.Catch                     as CMC
import qualified Control.Monad.Oops                      as OO
import qualified Data.Relation                           as R
import qualified Data.Set                                as S
import qualified HaskellWorks.CabalCache.Concurrent.Type as Z
import qualified HaskellWorks.CabalCache.IO.Console      as CIO
import qualified HaskellWorks.CabalCache.Pretty          as PP
import qualified Prettyprinter                           as PP
import qualified System.IO                               as IO

data DownloadStatus = DownloadSuccess | DownloadFailure deriving (Eq, Show)

succeed :: forall e a m. ()
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` DownloadStatus
  => m a
succeed = OO.throw DownloadSuccess

fail :: forall e a m. ()
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` DownloadStatus
  => m a
fail = OO.throw DownloadFailure

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
              liftIO $ CIO.hPutLn IO.stderr $ "Warning: Unexpected exception during download of " <> PP.pretty packageId <> ": " <> PP.show e
              liftIO $ IO.hFlush IO.stderr
              pure DownloadFailure
      case downloadStatus of
        DownloadSuccess -> do
          liftIO $ CIO.hPutLn IO.stderr $ "Downloaded " <> PP.pretty packageId
          liftIO $ STM.atomically $ commit downloadQueue packageId
        DownloadFailure -> do
          liftIO $ CIO.hPutLn IO.stderr $ "Failed to download " <> PP.pretty packageId
          liftIO $ STM.atomically $ failDownload downloadQueue packageId
      runQueue downloadQueue f

    Nothing -> return ()
