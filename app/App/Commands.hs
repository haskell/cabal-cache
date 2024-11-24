module App.Commands where

import App.Commands.Debug           (cmdDebug)
import App.Commands.Plan            (cmdPlan)
import App.Commands.SyncFromArchive (cmdSyncFromArchive)
import App.Commands.SyncToArchive   (cmdSyncToArchive)
import App.Commands.Version         (cmdVersion)
import HaskellWorks.Prelude
import Options.Applicative          (Parser)

import qualified Options.Applicative as OA

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = OA.subparser $ mempty
  <>  OA.commandGroup "Commands:"
  <>  cmdPlan
  <>  cmdSyncFromArchive
  <>  cmdSyncToArchive
  <>  cmdVersion
  <>  cmdDebug
