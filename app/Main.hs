module Main where

import App.Commands (commands)
import Control.Monad (join)
import Options.Applicative ((<**>))

import qualified Options.Applicative as OA

main :: IO ()
main = join $ OA.customExecParser
  (OA.prefs $ OA.showHelpOnEmpty <> OA.showHelpOnError)
  (OA.info (commands <**> OA.helper) OA.idm)
