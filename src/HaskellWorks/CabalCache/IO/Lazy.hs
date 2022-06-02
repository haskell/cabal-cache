{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}

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
  ) where

import Antiope.Core (ToText(toText), fromText, runAws, runResAws)
import Antiope.S3.Lazy (S3Uri)
import Control.Lens ((%~), (&), (^.))
import Control.Monad.Catch (SomeException, MonadCatch(..), MonadThrow(throwM))
import Control.Monad.Except (void, unless, MonadIO(..))
import Data.Generics.Product.Any (HasAny(the))
import HaskellWorks.CabalCache.AppError (AppError(AwsAppError, HttpAppError, NotFound, RetriesFailedAppError, GenericAppError), appErrorStatus)
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.CabalCache.Show (tshow)
import Network.URI (URI)
import Polysemy (Member, Sem)

import qualified Antiope.S3.Lazy                        as AWS
import qualified Control.Concurrent                     as IO
import qualified Control.Monad.Catch                    as MC
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Text                              as T
import qualified HaskellWorks.CabalCache.IO.Console     as CIO
import qualified HaskellWorks.CabalCache.Polysemy.Error as PY
import qualified Network.AWS                            as AWS
import qualified Network.AWS.S3.CopyObject              as AWS
import qualified Network.AWS.S3.HeadObject              as AWS
import qualified Network.AWS.S3.PutObject               as AWS
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Client.TLS                as HTTPS
import qualified Network.HTTP.Types                     as HTTP
import qualified Polysemy.ConstraintAbsorber.MonadCatch as PY
import qualified Polysemy.Embed                         as PY
import qualified Polysemy.Error                         as PY
import qualified Polysemy.Managed                       as PY
import qualified Polysemy.Resource                      as PY
import qualified System.Directory                       as IO
import qualified System.FilePath.Posix                  as FP
import qualified System.IO                              as IO
import qualified System.IO.Error                        as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsError ::  ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Resource            ) r
  => Sem r a
  -> Sem r a
handleAwsError f = PY.absorbMonadCatch $ MC.catch f $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status 404 _) _ _ _ _)) -> PY.throw $ AwsAppError s
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status 301 _) _ _ _ _)) -> PY.throw $ AwsAppError s
    _                                                                      -> MC.throwM e

handleHttpError :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Resource            ) r
  => Sem r a
  -> Sem r a
handleHttpError f = PY.absorbMonadCatch do
  catch f $ \(e :: HTTP.HttpException) ->
    case e of
      (HTTP.HttpExceptionRequest _ e') -> case e' of
        HTTP.StatusCodeException resp _ -> PY.throw $ HttpAppError (resp & HTTP.responseStatus)
        _                               -> PY.throw $ GenericAppError (tshow e')
      _                                 -> throwM e

getS3Uri :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error AppError     ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Managed            ) r
  => Member (PY.Resource           ) r
  => AWS.Env
  -> URI
  -> Sem r LBS.ByteString
getS3Uri envAws uri = do
  AWS.S3Uri b k <- PY.fromEither $ uriToS3Uri (reslashUri uri)
  handleAwsError $ runAws envAws $ AWS.unsafeDownload b k

uriToS3Uri :: URI -> Either AppError S3Uri
uriToS3Uri uri = case fromText @S3Uri (tshow uri) of
  Right s3Uri -> Right s3Uri
  Left msg    -> Left . GenericAppError $ "Unable to parse URI" <> tshow msg

readResource :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => AWS.Env
  -> Location
  -> Sem r LBS.ByteString
readResource envAws = \case
  Local path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then liftIO $ LBS.readFile path
      else PY.throw NotFound
  Uri uri -> case uri ^. the @"uriScheme" of
    "s3:"     -> getS3Uri envAws (reslashUri uri)
    "http:"   -> readHttpUri (reslashUri uri)
    "https:"  -> readHttpUri (reslashUri uri)
    scheme    -> PY.throw $ GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme)

readFirstAvailableResource :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => AWS.Env
  -> [Location]
  -> Sem r (LBS.ByteString, Location)
readFirstAvailableResource _ [] = PY.throw $ GenericAppError "No resources specified in read"
readFirstAvailableResource envAws (a:as) = do
  (, a) <$> readResource envAws a
    & do PY.onError @AppError \e -> if null as
          then PY.throw e
          else readFirstAvailableResource envAws as

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => AWS.Env
  -> Location
  -> Sem r Bool
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
    "s3:"   -> True <$ PY.managedLocal (headS3Uri envAws (reslashUri uri))
                  & PY.onError @AppError (const (return False))
    "http:" -> True <$ headHttpUri (reslashUri uri)
                  & PY.onError @AppError (const (return False))
    _scheme -> return False

firstExistingResource ::  ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => AWS.Env
  -> [Location]
  -> Sem r (Maybe Location)
firstExistingResource _ [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error AppError     ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Managed            ) r
  => Member (PY.Resource           ) r
  => AWS.Env
  -> URI
  -> Sem r AWS.HeadObjectResponse
headS3Uri envAws uri = do
  AWS.S3Uri b k <- PY.fromEither $ uriToS3Uri (reslashUri uri)
  handleAwsError $ runAws envAws $ AWS.send $ AWS.headObject b k

uploadToS3 :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error AppError     ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Managed            ) r
  => Member (PY.Resource           ) r
  => AWS.Env
  -> URI
  -> LBS.ByteString
  -> Sem r ()
uploadToS3 envAws uri lbs = do
  AWS.S3Uri b k <- PY.fromEither $ uriToS3Uri (reslashUri uri)
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  handleAwsError $ void $ PY.embed $ runResAws envAws $ AWS.send po

reslashUri :: URI -> URI
reslashUri uri = uri & the @"uriPath" %~ fmap reslashChar
  where reslashChar :: Char -> Char
        reslashChar '\\' = '/'
        reslashChar c    = c

writeResource :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error AppError     ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Managed            ) r
  => Member (PY.Resource           ) r
  => AWS.Env
  -> Location
  -> LBS.ByteString
  -> Sem r ()
writeResource envAws loc lbs = case loc of
  Local path -> void $ liftIO (LBS.writeFile path lbs)
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> uploadToS3 envAws (reslashUri uri) lbs
    "http:" -> PY.throw $ GenericAppError "HTTP PUT method not supported"
    scheme  -> PY.throw $ GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme)

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  Local path -> liftIO $ IO.createDirectoryIfMissing True path
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> return ()
    "http:" -> return ()
    _scheme -> return ()

copyS3Uri :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error AppError     ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Managed            ) r
  => Member (PY.Resource           ) r
  => AWS.Env
  -> URI
  -> URI
  -> Sem r ()
copyS3Uri envAws source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- PY.fromEither $ uriToS3Uri (reslashUri source)
  AWS.S3Uri targetBucket targetObjectKey <- PY.fromEither $ uriToS3Uri (reslashUri target)

  response <- PY.managedLocal $
    handleAwsError $ runAws envAws $ AWS.send (AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey)
  let responseCode = response ^. AWS.corsResponseStatus
  if 200 <= responseCode && responseCode < 300
    then return ()
    else do
      CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
      PY.throw RetriesFailedAppError


retry :: forall e r. ()
  => Show e
  => Member (PY.Embed IO) r
  => Member (PY.Error e ) r
  => Member (PY.Resource) r
  => Int
  -> Sem r ()
  -> Sem r ()
retry = retryWhen (const True)

retryWhen :: forall e r. ()
  => Show e
  => Member (PY.Embed IO) r
  => Member (PY.Error e ) r
  => Member (PY.Resource) r
  => (e -> Bool)
  -> Int
  -> Sem r ()
  -> Sem r ()
retryWhen p n f = f
  & PY.onError @e \exception -> if n > 0
      then do
        CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
        PY.embed $ IO.threadDelay 1000000
        if (p exception)
          then retry (n - 1) f
          else PY.throw exception
      else PY.throw exception

retryUnless :: ()
  => Show e
  => Member (PY.Error e ) r
  => Member (PY.Embed IO) r
  => Member (PY.Resource) r
  => (e -> Bool)
  -> Int
  -> Sem r ()
  -> Sem r ()
retryUnless p = retryWhen (not . p)

linkOrCopyResource :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error String        ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => AWS.Env
  -> Location
  -> Location
  -> Sem r ()
linkOrCopyResource envAws source target = case source of
  Local sourcePath -> case target of
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> PY.throw "Can't copy between different file backends"
  Uri sourceUri -> case target of
    Local _targetPath -> PY.throw "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless ((== Just 301) . appErrorStatus) 3 (copyS3Uri envAws (reslashUri sourceUri) (reslashUri targetUri))
      ("http:", "http:")           -> PY.throw "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> PY.throw $ GenericAppError $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

readHttpUri :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Resource            ) r
  => URI
  -> Sem r LBS.ByteString
readHttpUri httpUri = handleHttpError $ do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => URI
  -> Sem r LBS.ByteString
headHttpUri httpUri = handleHttpError $ do
  manager   <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request   <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (reslashUri httpUri)))
  response  <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: ()
  => Member (PY.Embed IO            ) r
  => Member (PY.Error AppError      ) r
  => Member (PY.Error SomeException ) r
  => Member (PY.Managed             ) r
  => Member (PY.Resource            ) r
  => FilePath
  -> Sem r ()
removePathRecursive pkgStorePath = PY.absorbMonadCatch $ catch action handler
  where action :: ()
          => Member (PY.Embed IO            ) r
          => Member (PY.Error AppError      ) r
          => Member (PY.Error SomeException ) r
          => Member (PY.Managed             ) r
          => Member (PY.Resource            ) r
          => Sem r ()
        action = liftIO (IO.removeDirectoryRecursive pkgStorePath)
        handler :: ()
          => Member (PY.Embed IO            ) r
          => Member (PY.Error AppError      ) r
          => Member (PY.Error SomeException ) r
          => Member (PY.Managed             ) r
          => Member (PY.Resource            ) r
          => IOError
          -> Sem r ()
        handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: Caught " <> tshow e
          PY.throw (GenericAppError (tshow e))
