module App.Commands.Debug.S3
  ( cmdS3
  ) where

import App.Commands.Debug.S3.Cp (cmdCp)
import HaskellWorks.Prelude

import qualified Options.Applicative as OA

{- HLINT ignore "Monoid law, left identity" -}

commands :: OA.Parser (IO ())
commands = OA.subparser $ mempty
  <>  cmdCp

cmdS3 :: OA.Mod OA.CommandFields (IO ())
cmdS3 = OA.command "s3" $ flip OA.info OA.idm commands
