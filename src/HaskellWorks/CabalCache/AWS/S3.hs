{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.AWS.S3
  ( uriToS3Uri,
    headS3Uri,
    getS3Uri,
    copyS3Uri,
    putObject,

  ) where

import Antiope.S3.Lazy                  (S3Uri)
import Control.Lens                     ((^.))
import Control.Monad                    (void, unless)
import Control.Monad.Catch              (MonadCatch(..))
import Control.Monad.Except             (MonadIO(..), MonadError(..))
import Control.Monad.Trans.AWS          (RsBody)
import Control.Monad.Trans.Except       (ExceptT(..))
import Control.Monad.Trans.Resource     (MonadResource, MonadUnliftIO, liftResourceT, runResourceT)
import Data.Conduit.Lazy                (lazyConsume)
import HaskellWorks.CabalCache.AppError (AppError(..), GenericError(..))
import HaskellWorks.CabalCache.Show     (tshow)
import Network.AWS                      (MonadAWS, HasEnv)
import Network.AWS.Data                 (ToText(..), fromText)
import Network.URI                      (URI)

import qualified Antiope.S3.Lazy                      as AWS
import qualified Control.Monad.Oops                   as OO
import qualified Data.ByteString.Lazy                 as LBS
import qualified HaskellWorks.CabalCache.IO.Console   as CIO
import qualified HaskellWorks.CabalCache.AWS.Error    as AWS
import qualified HaskellWorks.CabalCache.URI          as URI
import qualified Network.AWS                          as AWS
import qualified Network.AWS.Data.Body                as AWS
import qualified Network.AWS.S3.CopyObject            as AWS
import qualified Network.AWS.S3.GetObject             as AWS
import qualified Network.AWS.S3.HeadObject            as AWS
import qualified Network.AWS.S3.PutObject             as AWS
import qualified System.IO                            as IO

-- | Access the response body as a lazy bytestring
lazyByteString :: MonadResource m => RsBody -> m LBS.ByteString
lazyByteString rsBody = liftResourceT $ LBS.fromChunks <$> lazyConsume (AWS._streamBody rsBody)

unsafeDownloadRequest :: (MonadAWS m, MonadResource m)
  => AWS.GetObject
  -> m LBS.ByteString
unsafeDownloadRequest req = do
  resp <- AWS.send req
  lazyByteString (resp ^. AWS.gorsBody)

unsafeDownload :: (MonadAWS m, MonadResource m)
  => AWS.BucketName
  -> AWS.ObjectKey
  -> m LBS.ByteString
unsafeDownload bucketName objectKey = unsafeDownloadRequest (AWS.getObject bucketName objectKey)

uriToS3Uri :: URI -> Either GenericError S3Uri
uriToS3Uri uri = case fromText @S3Uri (tshow uri) of
  Right s3Uri -> Right s3Uri
  Left msg    -> Left . GenericError $ "Unable to parse URI" <> tshow msg

headS3Uri :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadResource m
  => HasEnv r
  => r
  -> URI
  -> m AWS.HeadObjectResponse
headS3Uri envAws uri = do
  AWS.S3Uri b k <- OO.throwLeftM $ uriToS3Uri (URI.reslashUri uri)
  AWS.handleAwsError $ AWS.runAWS envAws $ AWS.send $ AWS.headObject b k

putObject :: ()
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadCatch m
  => MonadUnliftIO m
  => HasEnv r
  => AWS.ToBody a
  => r
  -> URI
  -> a
  -> ExceptT (OO.Variant e) m ()
putObject envAws uri lbs = do
  AWS.S3Uri b k <- OO.throwLeftM $ uriToS3Uri (URI.reslashUri uri)
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  AWS.handleAwsError $ void $ OO.suspendM runResourceT $ AWS.runAWS envAws $ AWS.send po

getS3Uri :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadResource m
  => HasEnv r
  => r
  -> URI
  -> m LBS.ByteString
getS3Uri envAws uri = do
  AWS.S3Uri b k <- OO.throwLeftM $ uriToS3Uri (URI.reslashUri uri)
  AWS.handleAwsError $ AWS.runAWS envAws $ unsafeDownload b k

copyS3Uri :: ()
  => HasEnv r
  => MonadUnliftIO m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => r
  -> URI
  -> URI
  -> ExceptT (OO.Variant e) m ()
copyS3Uri envAws source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- OO.throwLeftM $ uriToS3Uri (URI.reslashUri source)
  AWS.S3Uri targetBucket targetObjectKey <- OO.throwLeftM $ uriToS3Uri (URI.reslashUri target)
  let copyObjectRequest = AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey
  response <- OO.suspendM runResourceT (AWS.runAWS envAws $ AWS.send copyObjectRequest)
  let responseCode = response ^. AWS.corsResponseStatus
  unless (200 <= responseCode && responseCode < 300) do
    liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
    OO.throwM RetriesFailedAppError
