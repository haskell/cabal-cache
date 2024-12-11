{-# LANGUAGE ImpredicativeTypes #-}

module App.Run
  ( runApp,
  ) where

import HaskellWorks.CabalCache.Error
import HaskellWorks.CabalCache.Exit
import Effectful
import Effectful.Concurrent
import Effectful.Environment
import Effectful.Resource
import Effectful.Zoo.Amazonka.Api.Run
import Effectful.Zoo.Amazonka.Data.AwsLogEntry
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude

import qualified Data.Text                          as T
import qualified System.IO                          as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

writeLog :: ()
  => r <: IOE
  => CallStack
  -> Severity
  -> Text
  -> Eff r ()
writeLog _ _ t = do
  liftIO $ IO.hPutStrLn IO.stderr $ T.unpack t -- TODO write severity

runApp :: ()
  => Eff
        ( Error ExitFailure
        : DataLog AwsLogEntry
        : DataLog (LogEntry (LogMessage Text))
        : Log Text
        : Environment
        : Concurrent
        : Resource
        : IOE
        : '[]
        ) a
  -> IO a
runApp f =
    f
      & catchAndExitFailure @ExitFailure
      & runDataLogAwsLogEntryToLog
      & runDataLog @(LogEntry (LogMessage Text)) (\_ -> pure ()) -- TODO log these properly
      & runLog (ConcUnlift Persistent Unlimited) writeLog
      & runEnvironment
      & runConcurrent
      & runResource
      & runEff
