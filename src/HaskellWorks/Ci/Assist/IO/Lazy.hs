{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaskellWorks.Ci.Assist.IO.Lazy
  ( readResource
  , resourceExists
  , headS3Uri
  , writeResource
  ) where

import Antiope.Core
import Antiope.S3.Lazy
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit.Lazy               (lazyConsume)
import Data.Either                     (isRight)
import Data.Text                       (Text)
import HaskellWorks.Ci.Assist.Location (Location (..))
import Network.AWS                     (MonadAWS, chunkedFile)
import Network.AWS.Data.Body           (_streamBody)

import qualified Antiope.S3.Lazy           as AWS
import qualified Antiope.S3.Types          as AWS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Network.AWS               as AWS
import qualified Network.AWS.Data          as AWS
import qualified Network.AWS.S3.HeadObject as AWS
import qualified Network.AWS.S3.PutObject  as AWS
import qualified Network.HTTP.Types        as HTTP
import qualified System.Directory          as IO
import qualified System.IO                 as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

readResource :: MonadResource m => AWS.Env -> Location -> m (Maybe LBS.ByteString)
readResource envAws = \case
  S3Location s3Uri -> runAws envAws $ AWS.downloadFromS3Uri s3Uri
  Local path       -> liftIO $ Just <$> LBS.readFile path

resourceExists :: (MonadResource m, MonadCatch m, MonadIO m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
  S3Location s3Uri -> isRight <$> headS3Uri envAws s3Uri
  Local path -> liftIO $ IO.doesFileExist path

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either String AWS.HeadObjectResponse)
headS3Uri envAws (AWS.S3Uri b k) =
  catch (Right <$> runAws envAws (AWS.send (AWS.headObject b k))) $ \(e :: AWS.Error) ->
    case e of
      (AWS.ServiceError (AWS.ServiceError' _ (HTTP.Status 404 _) _ _ _ _)) -> return (Left "Not found")
      _                                                                    -> throwM e

chunkSize :: AWS.ChunkSize
chunkSize = AWS.ChunkSize (1024 * 1024)

uploadeToS3 :: MonadUnliftIO m => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m ()
uploadeToS3 envAws (AWS.S3Uri b k) lbs = do
  let req = AWS.toBody lbs
  let po = AWS.putObject b k req
  result <- runResAws envAws $ AWS.send po
  return ()

writeResource :: MonadUnliftIO m => AWS.Env -> Location -> LBS.ByteString -> m ()
writeResource envAws loc lbs = case loc of
  S3Location s3Uri -> uploadeToS3 envAws s3Uri lbs
  Local path       -> liftIO $ LBS.writeFile path lbs
