{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.CabalCache.IO.Lazy
  ( readResource
  , readFirstAvailableResource
  , resourceExists
  , firstExistingResource
  , headS3Uri
  , writeResource
  , createLocalDirectoryIfMissing
  , linkOrCopyResource
  , readHttpUri
  , removePathRecursive
  , handleErrors
  , retryS3
  ) where

import Antiope.Core
import Antiope.S3.Lazy                  (S3Uri)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import Data.Either                      (isRight)
import Data.Generics.Product.Any
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.CabalCache.Show
import Network.URI                      (URI)

import qualified Antiope.S3.Lazy                    as AWS
import qualified Control.Concurrent                 as IO
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Network.AWS                        as AWS
import qualified Network.AWS.S3.CopyObject          as AWS
import qualified Network.AWS.S3.HeadObject          as AWS
import qualified Network.AWS.S3.PutObject           as AWS
import qualified Network.HTTP.Client                as HTTP
import qualified Network.HTTP.Types                 as HTTP
import qualified Network.HTTP.Client.TLS            as HTTPS
import qualified System.Directory                   as IO
import qualified System.FilePath.Posix              as FP
import qualified System.IO                          as IO
import qualified System.IO.Error                    as IO
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}


handleErrors :: MonadCatch m => m a -> m (Either AppError a)
handleErrors f = catches (Right <$> f) [ Handler (\(e :: AWS.Error) -> awsHandler e)
                                       , Handler (\(e :: HTTP.HttpException) -> httpHandler e)
                                       ]
 where
  httpHandler e = case e of
    (HTTP.HttpExceptionRequest _ e') -> case e' of
      HTTP.StatusCodeException resp _ -> return (Left (HttpAppError (resp & HTTP.responseStatus)))
      _                               -> return (Left (GenericAppError (tshow e)))
    _                               -> return (Left (GenericAppError (tshow e)))
  awsHandler e = case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status si _) _ _ _ _))
      | si >= 300
      , si < 600 -> return (Left (AwsAppError s))
    _ -> return (Left (GenericAppError (tshow e)))


getS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> URI -> ExceptT AppError m LBS.ByteString
getS3Uri envAws uri = do
  AWS.S3Uri b k <- except $ uriToS3Uri (reslashUri uri)
  ExceptT . handleErrors $ runAws envAws $ AWS.unsafeDownload b k

uriToS3Uri :: URI -> Either AppError S3Uri
uriToS3Uri uri = case fromText @S3Uri (tshow uri) of
  Right s3Uri -> Right s3Uri
  Left msg    -> Left . GenericAppError $ "Unable to parse URI" <> tshow msg

readResource :: (MonadResource m, MonadCatch m) => AWS.Env -> Location -> m (Either AppError LBS.ByteString)
readResource envAws = \case
  Local path -> liftIO $ do
    fileExists <- IO.doesFileExist path
    if fileExists
      then Right <$> LBS.readFile path
      else pure (Left NotFound)
  Uri uri -> runExceptT $ retryS3 $ case uri ^. the @"uriScheme" of
    "s3:"     -> getS3Uri envAws (reslashUri uri)
    "http:"   -> readHttpUri (reslashUri uri)
    "https:"  -> readHttpUri (reslashUri uri)
    scheme    -> throwE (GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme))

readFirstAvailableResource :: (MonadResource m, MonadCatch m) => AWS.Env -> [Location] -> m (Either AppError (LBS.ByteString, Location))
readFirstAvailableResource _ [] = return (Left (GenericAppError "No resources specified in read"))
readFirstAvailableResource envAws (a:as) = do
  result <- readResource envAws a
  case result of
    Right lbs -> return $ Right (lbs, a)
    Left e -> if null as
      then return $ Left e
      else readFirstAvailableResource envAws as

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
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
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> isRight <$> runResourceT (headS3Uri envAws (reslashUri uri))
    "http:" -> isRight <$> runExceptT (headHttpUri (reslashUri uri))
    _scheme -> return False

firstExistingResource :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> [Location] -> m (Maybe Location)
firstExistingResource _ [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> URI -> m (Either AppError AWS.HeadObjectResponse)
headS3Uri envAws uri = runExceptT $ do
  AWS.S3Uri b k <- except $ uriToS3Uri (reslashUri uri)
  ExceptT . handleErrors $ runAws envAws $ AWS.send $ AWS.headObject b k

uploadToS3 :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> URI -> LBS.ByteString -> ExceptT AppError m ()
uploadToS3 envAws uri lbs = do
  AWS.S3Uri b k <- except $ uriToS3Uri (reslashUri uri)
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  ExceptT . handleErrors $ void $ runResAws envAws $ AWS.send po

reslashUri :: URI -> URI
reslashUri uri = uri & the @"uriPath" %~ fmap reslashChar
  where reslashChar :: Char -> Char
        reslashChar '\\' = '/'
        reslashChar c    = c

writeResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> LBS.ByteString -> ExceptT AppError m ()
writeResource envAws loc lbs = ExceptT $ case loc of
  Local path -> liftIO (LBS.writeFile path lbs) >> return (Right ())
  Uri uri       -> runExceptT $ retryS3 $ case uri ^. the @"uriScheme" of
    "s3:"   -> uploadToS3 envAws (reslashUri uri) lbs
    "http:" -> throwE (GenericAppError "HTTP PUT method not supported")
    scheme  -> throwE (GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme))

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  Local path -> liftIO $ IO.createDirectoryIfMissing True path
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> return ()
    "http:" -> return ()
    _scheme -> return ()

copyS3Uri :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> URI -> URI -> ExceptT AppError m ()
copyS3Uri envAws source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- except $ uriToS3Uri (reslashUri source)
  AWS.S3Uri targetBucket targetObjectKey <- except $ uriToS3Uri (reslashUri target)
  ExceptT $ do
    responseResult <- runResourceT $
      handleErrors $ runAws envAws $ AWS.send (AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey)
    case responseResult of
      Right response -> do
        let responseCode = response ^. AWS.corsResponseStatus
        if 200 <= responseCode && responseCode < 300
          then return (Right ())
          else do
            liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
            return (Left RetriesFailedAppError)
      Left msg -> return (Left msg)

retryWhen :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m a -> ExceptT e m a
retryWhen p n f = catchError f $ \exception -> if n > 0
  then do
    if (p exception)
      then do
        liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
        liftIO $ IO.threadDelay 1000000
        retryWhen p (n - 1) f
      else throwError exception
  else throwError exception

retryUnless :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m a -> ExceptT e m a
retryUnless p = retryWhen (not . p)

retryS3 :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
retryS3 a = do
  retries <- (fromMaybe 3 . join) <$> liftIO (lookupEnv "CABAL_CACHE_RETRY" >>= \s -> return (readMaybe @Int <$> s))
  retryUnless (\appe -> case appErrorStatus appe of
                Just 402 -> False
                Just 408 -> False
                Just 410 -> False
                Just 425 -> False
                Just 429 -> False
                Just i
                  | i >= 500
                  , i < 600 -> False
                Just _   -> True
                Nothing  -> True) retries a

linkOrCopyResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Location -> ExceptT AppError m ()
linkOrCopyResource envAws source target = case source of
  Local sourcePath -> case target of
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> throwError "Can't copy between different file backends"
  Uri sourceUri -> case target of
    Local _targetPath -> throwError "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless ((== Just 301) . appErrorStatus) 3 (copyS3Uri envAws (reslashUri sourceUri) (reslashUri targetUri))
      ("http:", "http:")           -> throwError "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> throwError $ GenericAppError $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

readHttpUri :: (MonadIO m, MonadCatch m) => URI -> ExceptT AppError m LBS.ByteString
readHttpUri httpUri = ExceptT $ handleErrors $ do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: (MonadIO m, MonadCatch m) => URI -> ExceptT AppError m LBS.ByteString
headHttpUri httpUri = ExceptT $ handleErrors $ do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: (MonadIO m, MonadCatch m) => FilePath -> m (Either AppError ())
removePathRecursive pkgStorePath = catch action handler
  where action :: MonadIO m => m (Either AppError ())
        action = Right <$> liftIO (IO.removeDirectoryRecursive pkgStorePath)
        handler :: MonadIO m => IOError -> m (Either AppError ())
        handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: Caught " <> tshow e
          return (Left (GenericAppError (tshow e)))
