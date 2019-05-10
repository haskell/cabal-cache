{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaskellWorks.CabalCache.IO.Lazy
  ( readResource
  , resourceExists
  , firstExistingResource
  , headS3Uri
  , writeResource
  , createLocalDirectoryIfMissing
  , linkOrCopyResource
  ) where

import Antiope.Core
import Antiope.S3.Lazy
import Control.Lens
import Control.Monad                    (void)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit.Lazy                (lazyConsume)
import Data.Either                      (isRight)
import Data.Text                        (Text)
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.CabalCache.Show
import Network.AWS                      (MonadAWS, chunkedFile)
import Network.AWS.Data.Body            (_streamBody)

import qualified Antiope.S3.Lazy                    as AWS
import qualified Antiope.S3.Types                   as AWS
import qualified Control.Concurrent                 as IO
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Network.AWS                        as AWS
import qualified Network.AWS.Data                   as AWS
import qualified Network.AWS.S3.CopyObject          as AWS
import qualified Network.AWS.S3.HeadObject          as AWS
import qualified Network.AWS.S3.PutObject           as AWS
import qualified Network.HTTP.Types                 as HTTP
import qualified System.Directory                   as IO
import qualified System.FilePath.Posix              as FP
import qualified System.IO                          as IO
import qualified System.IO.Error                    as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

handleAwsError :: MonadCatch m => m a -> m (Either AppError a)
handleAwsError f = catch (Right <$> f) $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ status@(HTTP.Status 404 _) _ _ _ _)) -> return (Left (AwsAppError status))
    (AWS.ServiceError (AWS.ServiceError' _ status@(HTTP.Status 301 _) _ _ _ _)) -> return (Left (AwsAppError status))
    _                                                                           -> throwM e

readResource :: MonadResource m => AWS.Env -> Location -> m (Maybe LBS.ByteString)
readResource envAws = \case
  S3 s3Uri    -> runAws envAws $ AWS.downloadFromS3Uri s3Uri
  Local path  -> liftIO $ Just <$> LBS.readFile path

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
  S3 s3Uri    -> isRight <$> runResourceT (headS3Uri envAws s3Uri)
  Local path  -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then return True
      else do
        symbolicLinkExists <- liftIO $ safePathIsSymbolLink path
        if symbolicLinkExists
          then do
            target <- liftIO $ IO.getSymbolicLinkTarget path
            resourceExists envAws (Local target)
          else return False

firstExistingResource :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> [Location] -> m (Maybe Location)
firstExistingResource envAws [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either AppError AWS.HeadObjectResponse)
headS3Uri envAws (AWS.S3Uri b k) = handleAwsError $ runAws envAws $ AWS.send $ AWS.headObject b k

chunkSize :: AWS.ChunkSize
chunkSize = AWS.ChunkSize (1024 * 1024)

uploadToS3 :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m (Either AppError ())
uploadToS3 envAws (AWS.S3Uri b k) lbs = do
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  handleAwsError $ void $ runResAws envAws $ AWS.send po

writeResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> LBS.ByteString -> m (Either AppError ())
writeResource envAws loc lbs = case loc of
  S3 s3Uri   -> uploadToS3 envAws s3Uri lbs
  Local path -> liftIO (LBS.writeFile path lbs) >> return (Right ())

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  S3 s3Uri   -> return ()
  Local path -> liftIO $ IO.createDirectoryIfMissing True path

copyS3Uri :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> AWS.S3Uri -> ExceptT AppError m ()
copyS3Uri envAws (AWS.S3Uri sourceBucket sourceObjectKey) (AWS.S3Uri targetBucket targetObjectKey) = ExceptT $ do
  responseResult <- runResourceT $
    handleAwsError $ runAws envAws $ AWS.send (AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey)
  case responseResult of
    Right response -> do
      let responseCode = response ^. AWS.corsResponseStatus
      if 200 <= responseCode && responseCode < 300
        then return (Right ())
        else do
          liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
          return (Left RetriesFailedAppError)
    Left msg -> return (Left msg)

retry :: (Show e, MonadIO m) => Int -> ExceptT e m () -> ExceptT e m ()
retry = retryWhen (const True)

retryWhen :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m () -> ExceptT e m ()
retryWhen p n f = catchError f $ \exception -> if n > 0
  then do
    liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
    liftIO $ IO.threadDelay 1000000
    if (p exception )
      then retry (n - 1) f
      else throwError exception
  else throwError exception

retryUnless :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m () -> ExceptT e m ()
retryUnless p = retryWhen (not . p)

linkOrCopyResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Location -> ExceptT AppError m ()
linkOrCopyResource envAws source target = case source of
  S3 sourceS3Uri -> case target of
    S3 targetS3Uri -> retryUnless ((== Just 301) . appErrorStatus) 3 (copyS3Uri envAws sourceS3Uri targetS3Uri)
    Local _        -> throwError $ GenericAppError "Can't copy between different file backends"
  Local sourcePath -> case target of
    S3 _             -> throwError $ GenericAppError "Can't copy between different file backends"
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
