module HaskellWorks.CabalCache.AWS.S3
  ( uriToS3Uri,
    headS3Uri,
    getS3Uri,
    copyS3Uri,
    putObject,

  ) where

import Amazonka                           (ResponseBody)
import Amazonka.Data                      (ToText(..), fromText)
import Control.Monad.Trans.Resource       (MonadResource, liftResourceT)
import Data.Conduit.Lazy                  (lazyConsume)
import Data.Generics.Product.Any          (the)
import Effectful
import Effectful.Resource
import Effectful.Zoo.Amazonka.Api.Send
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Lazy.Dynamic
import Effectful.Zoo.DataLog.Dynamic
import HaskellWorks.CabalCache.AppError   (AwsStatusError(..))
import HaskellWorks.CabalCache.Error      (CopyFailed(..), UnsupportedUri(..))
import HaskellWorks.Prelude
import Lens.Micro
import Network.URI                        (URI)

import qualified Amazonka                             as AWS
import qualified Amazonka.S3                          as AWS
import qualified Data.ByteString.Lazy                 as LBS
import qualified HaskellWorks.CabalCache.AWS.Error    as AWS
import qualified HaskellWorks.CabalCache.AWS.S3.URI   as AWS
import qualified HaskellWorks.CabalCache.IO.Console   as CIO
import qualified HaskellWorks.CabalCache.URI          as URI
import qualified System.IO                            as IO

--- | Access the response body as a lazy bytestring
lazyByteString :: MonadResource m => ResponseBody -> m LBS.ByteString
lazyByteString rsBody = liftResourceT $ LBS.fromChunks <$> lazyConsume rsBody.body

unsafeDownloadRequest :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => AWS.GetObject
  -> Eff r LBS.ByteString
unsafeDownloadRequest req = do
  resp <- lazySendAws req
  lazyByteString $ resp ^. the @"body"

unsafeDownload :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => AWS.BucketName
  -> AWS.ObjectKey
  -> Eff r LBS.ByteString
unsafeDownload bucketName objectKey =
  unsafeDownloadRequest (AWS.newGetObject bucketName objectKey)

uriToS3Uri :: URI -> Either UnsupportedUri AWS.S3Uri
uriToS3Uri uri = case fromText @AWS.S3Uri (tshow uri) of
  Right s3Uri -> Right s3Uri
  Left msg    -> Left $ UnsupportedUri uri $ "Unable to parse URI" <> tshow msg

headS3Uri :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => URI
  -> Eff r AWS.HeadObjectResponse
headS3Uri uri = do
  AWS.S3Uri b k <- uriToS3Uri (URI.reslashUri uri)
    & onLeft throw

  AWS.handleAwsStatusError $ lazySendAws $ AWS.newHeadObject b k

putObject :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => AWS.ToBody a
  => URI
  -> a
  -> Eff r ()
putObject uri lbs = do
  AWS.S3Uri b k <- uriToS3Uri (URI.reslashUri uri)
    & onLeft throw

  let req = AWS.toBody lbs
  let po  = AWS.newPutObject b k req

  AWS.handleAwsStatusError $ void $ lazySendAws po

getS3Uri :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => URI
  -> Eff r LBS.ByteString
getS3Uri uri = do
  AWS.S3Uri b k <- uriToS3Uri (URI.reslashUri uri)
    & onLeft throw

  AWS.handleAwsStatusError $ unsafeDownload b k

copyS3Uri :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error CopyFailed
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => URI
  -> URI
  -> Eff r ()
copyS3Uri source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- uriToS3Uri (URI.reslashUri source) & onLeft throw
  AWS.S3Uri targetBucket targetObjectKey <- uriToS3Uri (URI.reslashUri target) & onLeft throw
  let copyObjectRequest = AWS.newCopyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey
  response <- lazySendAws copyObjectRequest
  let responseCode = response ^. the @"httpStatus"
  unless (200 <= responseCode && responseCode < 300) do
    liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
    throw CopyFailed
