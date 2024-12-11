module App.Commands.Debug.S3.Cp
  ( cmdCp,
  ) where

import App.Amazonka
import App.Commands.Options.Parser      (text)
import App.Commands.Options.Types       (CpOptions (CpOptions))
import App.Run
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Lazy.Dynamic
import HaskellWorks.CabalCache.AppError (AwsStatusError(..), displayAwsStatusError)
import HaskellWorks.CabalCache.Error    (CopyFailed(..), ExitFailure(..), UnsupportedUri)
import HaskellWorks.Prelude
import Network.URI                      (parseURI)

import qualified Amazonka                           as AWS
import qualified Amazonka.Data                      as AWS
import qualified App.Commands.Options.Types         as Z
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.AWS.S3     as AWS
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Options.Applicative                as OA
import qualified System.IO                          as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

runCp :: Z.CpOptions -> IO ()
runCp opts = runApp do
  let srcUri        = opts.srcUri
  let dstUri        = opts.dstUri
  let mHostEndpoint = opts.hostEndpoint
  let awsLogLevel   = opts.awsLogLevel

  runLazy (mkAwsEnv opts.region mHostEndpoint awsLogLevel) do
    AWS.copyS3Uri srcUri dstUri
      & do trap @AwsStatusError \e -> do
            CIO.hPutStrLn IO.stderr $ "Copy failed: " <> displayAwsStatusError e
            throw ExitFailure
      & do trap @AwsError \e -> do
            CIO.hPutStrLn IO.stderr $ "Copy failed: " <> tshow e
            throw ExitFailure
      & do trap @CopyFailed \CopyFailed -> do
            CIO.hPutStrLn IO.stderr "Copy failed"
            throw ExitFailure
      & do trap @UnsupportedUri \e -> do
            CIO.hPutStrLn IO.stderr $ "Unsupported uri: " <> tshow e
            throw ExitFailure

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
