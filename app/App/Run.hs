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
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.DataLog.Static
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Log.Static
import HaskellWorks.Prelude
import HaskellWorks.ToText

import Data.Text.IO qualified as T
import System.IO qualified as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

writeLog :: ()
  => r <: IOE
  => Eff r Severity
  -> CallStack
  -> LogMessage Text
  -> Eff r ()
writeLog getLogSeverity _cs (LogMessage messageSeverity message) = do
  logSeverity <- getLogSeverity
  when (messageSeverity >= logSeverity) $
    liftIO $ T.hPutStrLn IO.stderr $ "[" <> toText messageSeverity <> "] " <> message

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
      & runLog (ConcUnlift Persistent Unlimited) (writeLog (pure Info))
      & runEnvironment
      & runConcurrent
      & runResource
      & runEff
