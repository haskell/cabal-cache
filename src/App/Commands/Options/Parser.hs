{-# LANGUAGE OverloadedStrings #-}
module App.Commands.Options.Parser
where

import Antiope.Core                    (FromText, Region (..), fromText)
import App.Commands.Options.Types      (SyncFromArchiveOptions (..), SyncToArchiveOptions (..))
import App.Static                      (homeDirectory)
import HaskellWorks.Ci.Assist.Location (Location (..), toLocation, (</>))
import Options.Applicative

import qualified Data.Text as Text

optsSyncFromArchive :: Parser SyncFromArchiveOptions
optsSyncFromArchive = SyncFromArchiveOptions
  <$> option (auto <|> text)
      (  long "region"
      <> metavar "AWS_REGION"
      <> showDefault <> value Oregon
      <> help "The AWS region in which to operate"
      )
  <*> option (maybeReader (toLocation . Text.pack))
      (   long "archive-uri"
      <>  help "Archive URI to sync to"
      <>  metavar "S3_URI"
      <>  value (Local $ homeDirectory </> ".cabal" </> "archive")
      )
  <*> strOption
      (   long "store-path"
      <>  help "Path to cabal store"
      <>  metavar "DIRECTORY"
      <>  value (homeDirectory </> ".cabal" </> "store")
      )
  <*> option auto
      (   long "threads"
      <>  help "Number of concurrent threads"
      <>  metavar "NUM_THREADS"
      <>  value 4
      )

xxx :: ReadM Location
xxx = maybeReader (toLocation . Text.pack)

optsSyncToArchive :: Parser SyncToArchiveOptions
optsSyncToArchive = SyncToArchiveOptions
  <$> option (auto <|> text)
      (  long "region"
      <> metavar "AWS_REGION"
      <> showDefault <> value Oregon
      <> help "The AWS region in which to operate"
      )
  <*> option (maybeReader (toLocation . Text.pack))
      (   long "archive-uri"
      <>  help "Archive URI to sync to"
      <>  metavar "S3_URI"
      <>  value (Local $ homeDirectory </> ".cabal" </> "archive")
      )
  <*> strOption
      (   long "store-path"
      <>  help "Path to cabal store"
      <>  metavar "DIRECTORY"
      <>  value (homeDirectory </> ".cabal" </> "store")
      )
  <*> option auto
      (   long "threads"
      <>  help "Number of concurrent threads"
      <>  metavar "NUM_THREADS"
      <>  value 4
      )

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)
