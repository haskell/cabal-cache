{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module HaskellWorks.CabalCache.AWS.S3
  ( uriToS3Uri,
    headS3Uri,
    getS3Uri,
    copyS3Uri,
    putObject,

  ) where

import Amazonka                           (ResponseBody)
import Amazonka.Data                      (ToText(..), fromText)
import Control.Monad.Catch                (MonadCatch(..))
import Control.Monad.Except               (MonadError)
import Control.Monad.Trans.Resource       (MonadResource, MonadUnliftIO, liftResourceT, runResourceT)
import Data.Conduit.Lazy                  (lazyConsume)
import Data.Generics.Product.Any          (the)
import HaskellWorks.CabalCache.AppError   (AwsError(..))
import HaskellWorks.CabalCache.Error      (CopyFailed(..), UnsupportedUri(..))
import HaskellWorks.Prelude
import Lens.Micro
import Network.URI                        (URI)

import qualified Amazonka                             as AWS
-- import qualified Amazonka.Data.Body                   as AWS
import qualified Amazonka.S3                          as AWS
import qualified Control.Monad.Oops                   as OO
import qualified Data.ByteString.Lazy                 as LBS
import qualified HaskellWorks.CabalCache.AWS.Error    as AWS
import qualified HaskellWorks.CabalCache.AWS.S3.URI   as AWS
import qualified HaskellWorks.CabalCache.IO.Console   as CIO
import qualified HaskellWorks.CabalCache.URI          as URI
import qualified System.IO                            as IO

--- | Access the response body as a lazy bytestring
lazyByteString :: MonadResource m => ResponseBody -> m LBS.ByteString
lazyByteString rsBody = liftResourceT $ LBS.fromChunks <$> lazyConsume (rsBody ^. the @"body")

unsafeDownloadRequest :: ()
  => Monad m
  => MonadResource m
  => AWS.Env
  -> AWS.GetObject
  -> m LBS.ByteString
unsafeDownloadRequest awsEnv req = do
  resp <- AWS.send awsEnv req
  lazyByteString (resp ^. the @"body")

unsafeDownload :: ()
  => Monad m
  => MonadResource m
  => AWS.Env
  -> AWS.BucketName
  -> AWS.ObjectKey
  -> m LBS.ByteString
unsafeDownload env bucketName objectKey = unsafeDownloadRequest env (AWS.newGetObject bucketName objectKey)

uriToS3Uri :: URI -> Either UnsupportedUri AWS.S3Uri
uriToS3Uri uri = case fromText @AWS.S3Uri (tshow uri) of
  Right s3Uri -> Right s3Uri
  Left msg    -> Left $ UnsupportedUri uri $ "Unable to parse URI" <> tshow msg

headS3Uri :: ()
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` UnsupportedUri
  => MonadCatch m
  => MonadResource m
  => AWS.Env
  -> URI
  -> m AWS.HeadObjectResponse
headS3Uri envAws uri = do
  AWS.S3Uri b k <- OO.hoistEither $ uriToS3Uri (URI.reslashUri uri)
  AWS.handleAwsError $ AWS.send envAws $ AWS.newHeadObject b k

putObject :: ()
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` UnsupportedUri
  => MonadCatch m
  => MonadUnliftIO m
  => AWS.ToBody a
  => AWS.Env
  -> URI
  -> a
  -> ExceptT (OO.Variant e) m ()
putObject envAws uri lbs = do
  AWS.S3Uri b k <- OO.hoistEither $ uriToS3Uri (URI.reslashUri uri)
  let req = AWS.toBody lbs
  let po  = AWS.newPutObject b k req
  AWS.handleAwsError $ void $ OO.suspend runResourceT $ AWS.send envAws po

getS3Uri :: ()
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` UnsupportedUri
  => MonadCatch m
  => MonadResource m
  => AWS.Env
  -> URI
  -> m LBS.ByteString
getS3Uri envAws uri = do
  AWS.S3Uri b k <- OO.hoistEither $ uriToS3Uri (URI.reslashUri uri)
  AWS.handleAwsError $ unsafeDownload envAws b k

copyS3Uri :: ()
  => MonadUnliftIO m
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` CopyFailed
  => e `OO.CouldBe` UnsupportedUri
  => AWS.Env
  -> URI
  -> URI
  -> ExceptT (OO.Variant e) m ()
copyS3Uri envAws source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- OO.hoistEither $ uriToS3Uri (URI.reslashUri source)
  AWS.S3Uri targetBucket targetObjectKey <- OO.hoistEither $ uriToS3Uri (URI.reslashUri target)
  let copyObjectRequest = AWS.newCopyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey
  response <- OO.suspend runResourceT $ AWS.send envAws copyObjectRequest
  let responseCode = response ^. the @"httpStatus"
  unless (200 <= responseCode && responseCode < 300) do
    liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
    OO.throw CopyFailed
