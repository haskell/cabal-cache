{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HaskellWorks.CabalCache.IO.Lazy
  ( readResource,
    readFirstAvailableResource,
    resourceExists,
    writeResource,
    createLocalDirectoryIfMissing,
    linkOrCopyResource,
    readHttpUri,
    removePathRecursive,
    retryOnE,
  ) where

import Control.Monad.Catch              (MonadCatch(..))
import Control.Monad.Except             (MonadError)
import Control.Monad.Trans.Resource     (MonadResource, runResourceT, MonadUnliftIO)
import Data.Generics.Product.Any        (HasAny(the))
import Data.List.NonEmpty               (NonEmpty ((:|)))
import HaskellWorks.CabalCache.AppError (AwsError(..), HttpError(..), statusCodeOf)
import HaskellWorks.CabalCache.Error    (CopyFailed(..), InvalidUrl(..), NotFound(..), NotImplemented(..), UnsupportedUri(..))
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.Prelude
import Lens.Micro
import Network.URI                      (URI)

import qualified Amazonka                             as AWS
import qualified Control.Concurrent                   as IO
import qualified Control.Monad.Oops                   as OO
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.List.NonEmpty                   as NEL
import qualified Data.Text                            as T
import qualified HaskellWorks.CabalCache.AWS.S3       as S3
import qualified HaskellWorks.CabalCache.IO.Console   as CIO
import qualified HaskellWorks.CabalCache.URI          as URI
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Client.TLS              as HTTPS
import qualified System.Directory                     as IO
import qualified System.FilePath.Posix                as FP
import qualified System.IO                            as IO
import qualified System.IO.Error                      as IO


{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleHttpError :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` HttpError
  => e `OO.CouldBe` InvalidUrl
  => MonadIO m
  => Monad m
  => m a
  -> m a
handleHttpError f = catch f $ \(e :: HTTP.HttpException) ->
  case e of
    HTTP.HttpExceptionRequest request content' -> OO.throw $ HttpError request content'
    HTTP.InvalidUrlException url' reason' -> OO.throw $ InvalidUrl (tshow url') (tshow reason')

readResource :: ()
  => MonadResource m
  => MonadCatch m
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` UnsupportedUri
  => e `OO.CouldBe` HttpError
  => e `OO.CouldBe` InvalidUrl
  => e `OO.CouldBe` NotFound
  => AWS.Env
  -> Int
  -> Location
  -> ExceptT (OO.Variant e) m LBS.ByteString
readResource envAws maxRetries = \case
  Local path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then liftIO $ LBS.readFile path
      else OO.throw NotFound
  Uri uri -> retryS3 maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"     -> S3.getS3Uri envAws (URI.reslashUri uri)
    "http:"   -> readHttpUri (URI.reslashUri uri)
    "https:"  -> readHttpUri (URI.reslashUri uri)
    scheme    -> OO.throw $ UnsupportedUri uri $ "Unrecognised uri scheme: " <> T.pack scheme

readFirstAvailableResource :: ()
  => MonadResource m
  => MonadCatch m
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` HttpError
  => e `OO.CouldBe` InvalidUrl
  => e `OO.CouldBe` NotFound
  => e `OO.CouldBe` UnsupportedUri
  => AWS.Env
  -> NonEmpty Location
  -> Int
  -> ExceptT (OO.Variant e) m (LBS.ByteString, Location)
readFirstAvailableResource envAws (a:|as) maxRetries = do
  (, a) <$> readResource envAws maxRetries a
    & do OO.catch @NotFound \e -> do
          case NEL.nonEmpty as of
            Nothing -> OO.throwF (Identity e)
            Just nas -> readFirstAvailableResource envAws nas maxRetries
    & do OO.catch @AwsError \e -> do
          case NEL.nonEmpty as of
            Nothing -> OO.throwF (Identity e)
            Just nas -> readFirstAvailableResource envAws nas maxRetries
    & do OO.catch @HttpError \e -> do
          case NEL.nonEmpty as of
            Nothing -> OO.throwF (Identity e)
            Just nas -> readFirstAvailableResource envAws nas maxRetries

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: ()
  => MonadUnliftIO m
  => MonadCatch m
  => e `OO.CouldBe` InvalidUrl
  => e `OO.CouldBe` UnsupportedUri
  => AWS.Env
  -> Location
  -> ExceptT (OO.Variant e) m Bool
resourceExists envAws = \case
  Local path -> do
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
  Uri uri -> case uri ^. the @"uriScheme" of
    "s3:" -> do
      OO.suspend runResourceT $ (True <$ S3.headS3Uri envAws (URI.reslashUri uri))
        & OO.catch @AwsError (pure . const False)
        & OO.catch @HttpError (pure . const False)
    "http:" -> do
      (True <$ headHttpUri (URI.reslashUri uri))
        & OO.catch @AwsError (pure . const False)
        & OO.catch @HttpError (pure . const False)
    _scheme -> return False

writeResource :: ()
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` HttpError
  => e `OO.CouldBe` NotImplemented
  => e `OO.CouldBe` UnsupportedUri
  => MonadIO m
  => MonadCatch m
  => MonadUnliftIO m
  => AWS.Env
  -> Location
  -> Int
  -> LBS.ByteString
  -> ExceptT (OO.Variant e) m ()
writeResource envAws loc maxRetries lbs = case loc of
  Local path -> liftIO (LBS.writeFile path lbs) >> return ()
  Uri uri' -> retryS3 maxRetries $ case uri' ^. the @"uriScheme" of
    "s3:"   -> S3.putObject envAws (URI.reslashUri uri') lbs
    "http:" -> OO.throw $ NotImplemented "HTTP PUT method not supported"
    scheme  -> OO.throw $ UnsupportedUri uri' $ "Unrecognised uri scheme: " <> T.pack scheme

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  Local path -> liftIO $ IO.createDirectoryIfMissing True path
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> return ()
    "http:" -> return ()
    _scheme -> return ()

retryOnE :: forall e e' m a. ()
  => Monad m
  => Int
  -> ExceptT (OO.Variant e') m a
  -> ExceptT (OO.Variant (e : e')) m a
  -> ExceptT (OO.Variant e') m a
retryOnE n g f = f
  & do OO.catch @e \_ -> if n > 0
        then retryOnE (n - 1) g f
        else g

retryWhen :: ()
  => MonadIO m
  => Show x
  => e `OO.CouldBe` x
  => (x -> Bool)
  -> Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryWhen p n f = f
  & do OO.snatch \exception -> do
        if n > 0
          then do
            if p exception
              then do
                liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
                liftIO $ IO.threadDelay 1000000
                retryWhen p (n - 1) f
              else OO.throw exception
          else OO.throw exception

retryUnless :: forall x e m a. ()
  => MonadIO m
  => Show x
  => e `OO.CouldBe` x
  => (x -> Bool)
  -> Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryUnless p = retryWhen (not . p)

retryS3 :: ()
  => MonadIO m
  => e `OO.CouldBe` AwsError
  => Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryS3 maxRetries a = do
  retryWhen retryPredicate maxRetries a
  where retryPredicate :: AwsError -> Bool
        retryPredicate e = statusCodeOf e `elem` retryableHTTPStatuses

  -- https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ErrorCodeList
  -- https://stackoverflow.com/a/51770411/2976251
  -- another note: linode rate limiting returns 503
retryableHTTPStatuses :: [Int]
retryableHTTPStatuses = [408, 409, 425, 426, 502, 503, 504]

linkOrCopyResource :: ()
  => MonadUnliftIO m
  => e `OO.CouldBe` AwsError
  => e `OO.CouldBe` CopyFailed
  => e `OO.CouldBe` NotImplemented
  => e `OO.CouldBe` UnsupportedUri
  => AWS.Env
  -> Location
  -> Location
  -> ExceptT (OO.Variant e) m ()
linkOrCopyResource envAws source target = case source of
  Local sourcePath -> case target of
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> OO.throw $ NotImplemented "Can't copy between different file backends"
  Uri sourceUri -> case target of
    Local _targetPath -> OO.throw $ NotImplemented "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless @AwsError ((== 301) . statusCodeOf) 3 (S3.copyS3Uri envAws (URI.reslashUri sourceUri) (URI.reslashUri targetUri))
      ("http:", "http:")           -> OO.throw $ NotImplemented "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> OO.throw $ NotImplemented $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

readHttpUri :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` HttpError
  => e `OO.CouldBe` InvalidUrl
  => MonadIO m
  => URI
  -> m LBS.ByteString
readHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (URI.reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` HttpError
  => e `OO.CouldBe` InvalidUrl
  => MonadIO m
  => URI
  -> m LBS.ByteString
headHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (URI.reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: ()
  => MonadCatch m
  => MonadIO m
  => [Char]
  -> ExceptT (OO.Variant e) m ()
removePathRecursive pkgStorePath = liftIO (IO.removeDirectoryRecursive pkgStorePath)
