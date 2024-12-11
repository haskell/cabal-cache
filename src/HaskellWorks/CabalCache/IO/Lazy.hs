module HaskellWorks.CabalCache.IO.Lazy
  ( readResource,
    readFirstAvailableResource,
    resourceExists,
    writeResource,
    createLocalDirectoryIfMissing,
    linkOrCopyResource,
    readHttpUri,
    removePathRecursive,
  ) where

import Data.Generics.Product.Any        (HasAny(the))
import Data.List.NonEmpty               (NonEmpty ((:|)))
import Effectful
import Effectful.Resource
import Effectful.Zoo.Amazonka.Data.AwsEnv
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Amazonka.Data.AwsLogEntry
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Core.Exception
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.Lazy.Dynamic
import HaskellWorks.CabalCache.AppError (AwsStatusError(..), HttpError(..), statusCodeOf)
import HaskellWorks.CabalCache.Error    (CopyFailed(..), InvalidUrl(..), NotFound(..), NotImplemented(..), UnsupportedUri(..))
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.Prelude
import Lens.Micro
import Network.URI                      (URI)

import qualified Control.Concurrent                   as IO
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
  => r <: Error HttpError
  => r <: Error InvalidUrl
  => Eff r a
  -> Eff r a
handleHttpError f = catchIO f $ \(e :: HTTP.HttpException) ->
  case e of
    HTTP.HttpExceptionRequest request content' -> throw $ HttpError request content'
    HTTP.InvalidUrlException url' reason' -> throw $ InvalidUrl (tshow url') (tshow reason')

readResource :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error HttpError
  => r <: Error InvalidUrl
  => r <: Error NotFound
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => Int
  -> Location
  -> Eff r LBS.ByteString
readResource maxRetries = \case
  LocalFile path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then liftIO $ LBS.readFile path
      else throw NotFound
  Uri uri -> retryS3 maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"     -> S3.getS3Uri (URI.reslashUri uri)
    "http:"   -> readHttpUri (URI.reslashUri uri)
    "https:"  -> readHttpUri (URI.reslashUri uri)
    scheme    -> throw $ UnsupportedUri uri $ "Unrecognised uri scheme: " <> T.pack scheme

readFirstAvailableResource :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error HttpError
  => r <: Error InvalidUrl
  => r <: Error NotFound
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => NonEmpty Location
  -> Int
  -> Eff r (LBS.ByteString, Location)
readFirstAvailableResource (a:|as) maxRetries = do
  (, a) <$> readResource maxRetries a
    & do trap @NotFound \e -> do
          case NEL.nonEmpty as of
            Nothing -> throw e
            Just nas -> readFirstAvailableResource nas maxRetries
    & do trap @AwsStatusError \e -> do
          case NEL.nonEmpty as of
            Nothing -> throw e
            Just nas -> readFirstAvailableResource nas maxRetries
    & do trap @HttpError \e -> do
          case NEL.nonEmpty as of
            Nothing -> throw e
            Just nas -> readFirstAvailableResource nas maxRetries

safePathIsSymbolLink :: ()
  => r <: IOE
  => FilePath
  -> Eff r Bool
safePathIsSymbolLink filePath =
  catchIO (liftIO $ IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> Eff r Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error InvalidUrl
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => Location
  -> Eff r Bool
resourceExists = \case
  LocalFile path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then return True
      else do
        symbolicLinkExists <- safePathIsSymbolLink path
        if symbolicLinkExists
          then do
            target <- liftIO $ IO.getSymbolicLinkTarget path
            resourceExists (LocalFile target)
          else return False
  Uri uri -> case uri ^. the @"uriScheme" of
    "s3:" -> do
      (True <$ S3.headS3Uri (URI.reslashUri uri))
        & trap_ @AwsStatusError (pure False)
        -- & trap_ @HttpError (pure False)
    "http:" -> do
      (True <$ headHttpUri (URI.reslashUri uri))
        & trap_ @HttpError (pure False)
        & trap_ @AwsStatusError (pure False)
    _scheme -> return False

writeResource :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error HttpError
  => r <: Error NotImplemented
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => Location
  -> Int
  -> LBS.ByteString
  -> Eff r ()
writeResource loc maxRetries lbs = case loc of
  LocalFile path -> liftIO (LBS.writeFile path lbs)
  Uri uri' -> retryS3 maxRetries $ case uri' ^. the @"uriScheme" of
    "s3:"   -> S3.putObject (URI.reslashUri uri') lbs
    "http:" -> throw $ NotImplemented "HTTP PUT method not supported"
    scheme  -> throw $ UnsupportedUri uri' $ "Unrecognised uri scheme: " <> T.pack scheme

createLocalDirectoryIfMissing :: MonadIO m => Location -> m ()
createLocalDirectoryIfMissing = \case
  LocalFile path -> liftIO $ IO.createDirectoryIfMissing True path
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> return ()
    "http:" -> return ()
    _scheme -> return ()

retryWhen :: ()
  => r <: Error x
  => r <: IOE
  => Show x
  => (x -> Bool)
  -> Int
  -> Eff r a
  -> Eff r a
retryWhen p n f = f
  & do trapIn \exception -> do
        if n > 0
          then do
            if p exception
              then do
                liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
                liftIO $ IO.threadDelay 1000000
                retryWhen p (n - 1) f
              else throw exception
          else throw exception

retryUnless :: forall x r a. ()
  => Show x
  => r <: Error x
  => r <: IOE
  => (x -> Bool)
  -> Int
  -> Eff r a
  -> Eff r a
retryUnless p = retryWhen (not . p)

retryS3 :: ()
  => r <: Error AwsStatusError
  => r <: IOE
  => Int
  -> Eff r a
  -> Eff r a
retryS3 maxRetries a = do
  retryWhen retryPredicate maxRetries a
  where retryPredicate :: AwsStatusError -> Bool
        retryPredicate e = statusCodeOf e `elem` retryableHTTPStatuses

  -- https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ErrorCodeList
  -- https://stackoverflow.com/a/51770411/2976251
  -- another note: linode rate limiting returns 503
retryableHTTPStatuses :: [Int]
retryableHTTPStatuses = [408, 409, 425, 426, 502, 503, 504]

linkOrCopyResource :: ()
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: Error AwsStatusError
  => r <: Error CopyFailed
  => r <: Error NotImplemented
  => r <: Error UnsupportedUri
  => r <: IOE
  => r <: Lazy AwsEnv
  => r <: Resource
  => Location
  -> Location
  -> Eff r ()
linkOrCopyResource source target = case source of
  LocalFile sourcePath -> case target of
    LocalFile targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> throw $ NotImplemented "Can't copy between different file backends"
  Uri sourceUri -> case target of
    LocalFile _targetPath -> throw $ NotImplemented "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless @AwsStatusError ((== 301) . statusCodeOf) 3 (S3.copyS3Uri (URI.reslashUri sourceUri) (URI.reslashUri targetUri))
      ("http:", "http:")           -> throw $ NotImplemented "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> throw $ NotImplemented $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

readHttpUri :: ()
  => r <: Error HttpError
  => r <: Error InvalidUrl
  => r <: IOE
  => URI
  -> Eff r LBS.ByteString
readHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (URI.reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: ()
  => r <: Error HttpError
  => r <: Error InvalidUrl
  => r <: IOE
  => URI
  -> Eff r LBS.ByteString
headHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (URI.reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: ()
  => r <: IOE
  => [Char]
  -> Eff r ()
removePathRecursive pkgStorePath =
  liftIO (IO.removeDirectoryRecursive pkgStorePath)
