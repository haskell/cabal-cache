module App.Commands.Debug
  ( cmdDebug
  ) where

import App.Commands.Debug.S3 (cmdS3)
import HaskellWorks.Prelude

import qualified Options.Applicative as OA

{- HLINT ignore "Monoid law, left identity" -}

commands :: OA.Parser (IO ())
commands = OA.subparser $ mempty
  <>  cmdS3

cmdDebug :: OA.Mod OA.CommandFields (IO ())
cmdDebug = OA.command "debug" $ flip OA.info OA.idm commands
