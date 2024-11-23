{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Debug.S3.Cp
  ( cmdCp,
  ) where

import App.Commands.Options.Parser      (text)
import App.Commands.Options.Types       (CpOptions (CpOptions))
import Control.Applicative              (Alternative(..), optional)
import Control.Lens                     ((&), (^.), (.~), (%~))
import Control.Monad.IO.Class           (MonadIO(..))
import Data.ByteString                  (ByteString)
import Data.Functor                     ((<&>))
import Data.Generics.Product.Any        (the)
import HaskellWorks.CabalCache.AppError (AwsError(..), displayAwsError)
import HaskellWorks.CabalCache.Error    (CopyFailed(..), ExitFailure(..), UnsupportedUri)
import HaskellWorks.CabalCache.Show     (tshow)
import Network.URI                      (parseURI)

import qualified Amazonka                           as AWS
import qualified Amazonka.Data                      as AWS
import qualified App.Commands.Options.Types         as Z
import qualified Control.Monad.Oops                 as OO
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.AWS.Env    as AWS
import qualified HaskellWorks.CabalCache.AWS.S3     as AWS
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Options.Applicative                as OA
import qualified System.IO                          as IO
import qualified System.IO.Unsafe                   as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

runCp :: Z.CpOptions -> IO ()
runCp opts = OO.runOops $ OO.catchAndExitFailure @ExitFailure do
  let srcUri        = opts ^. the @"srcUri"
  let dstUri        = opts ^. the @"dstUri"
  let mHostEndpoint = opts ^. the @"hostEndpoint"
  let awsLogLevel   = opts ^. the @"awsLogLevel"

  OO.catchAndExitFailure @ExitFailure do
    envAws <-
      liftIO (IO.unsafeInterleaveIO (AWS.mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)))
        <&> case mHostEndpoint of
              Just (host, port, ssl) ->
                \env ->
                  env
                    & the @"overrides" .~ \svc ->
                        svc & the @"endpoint" %~ \mkEndpoint region ->
                          mkEndpoint region
                            & the @"host"   .~ host
                            & the @"port"   .~ port
                            & the @"secure" .~ ssl
              Nothing -> id

    AWS.copyS3Uri envAws srcUri dstUri
      & do OO.catch @AwsError \e -> do
            CIO.hPutStrLn IO.stderr $ "Copy failed: " <> displayAwsError e
      & do OO.catch @CopyFailed \CopyFailed -> do
            CIO.hPutStrLn IO.stderr $ "Copy failed"
      & do OO.catch @UnsupportedUri \e -> do
            CIO.hPutStrLn IO.stderr $ "Unsupported uri: " <> tshow e

optsCp :: OA.Parser CpOptions
optsCp = CpOptions
  <$> OA.option (OA.auto <|> text)
      (  OA.long "region"
      <> OA.metavar "AWS_REGION"
      <> OA.showDefault
      <> OA.value AWS.Oregon
      <> OA.help "The AWS region in which to operate"
      )
  <*> OA.option (OA.maybeReader parseURI)
      (   OA.long "src-uri"
      <>  OA.help "Source URI to copy from"
      <>  OA.metavar "S3_URI"
      )
  <*> OA.option (OA.maybeReader parseURI)
      (   OA.long "dst-uri"
      <>  OA.help "Destination URI to copy to"
      <>  OA.metavar "S3_URI"
      )
  <*> optional
      ( OA.option (OA.eitherReader (AWS.fromText . T.pack))
        (   OA.long "aws-log-level"
        <>  OA.help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
        <>  OA.metavar "AWS_LOG_LEVEL"
        )
      )
  <*> optional parseEndpoint

parseEndpoint :: OA.Parser (ByteString, Int, Bool)
parseEndpoint =
  (,,)
  <$> OA.option (OA.eitherReader (AWS.fromText . T.pack))
        (   OA.long "host-name-override"
        <>  OA.help "Override the host name (default: s3.amazonaws.com)"
        <>  OA.metavar "HOST_NAME"
        )
  <*> OA.option OA.auto
        (   OA.long "host-port-override"
        <>  OA.help "Override the host port"
        <>  OA.metavar "HOST_PORT"
        )
  <*> OA.option OA.auto
        (   OA.long "host-ssl-override"
        <>  OA.help "Override the host SSL"
        <>  OA.metavar "HOST_SSL"
        )

cmdCp :: OA.Mod OA.CommandFields (IO ())
cmdCp = OA.command "cp" $ flip OA.info OA.idm $ runCp <$> optsCp
