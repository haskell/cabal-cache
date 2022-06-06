module App.Commands.Options.Parser where

import Antiope.Core               (FromText, fromText)
import App.Commands.Options.Types (VersionOptions (..))
import Options.Applicative        (Parser, ReadM)

import qualified Data.Text            as Text
import qualified Options.Applicative  as OA

optsVersion :: Parser VersionOptions
optsVersion = pure VersionOptions

text :: FromText a => ReadM a
text = OA.eitherReader (fromText . Text.pack)
