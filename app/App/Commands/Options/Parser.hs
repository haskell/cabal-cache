module App.Commands.Options.Parser
  ( optsVersion,
    optsPackageIds,
    text,
  ) where

import App.Commands.Options.Types (VersionOptions (..))
import Data.Set                   (Set)
import HaskellWorks.Prelude
import Options.Applicative        (Parser, ReadM)

import qualified Amazonka.Data                  as AWS
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Text                      as Text
import qualified HaskellWorks.CabalCache.Types  as Z
import qualified Options.Applicative            as OA

optsVersion :: Parser VersionOptions
optsVersion = pure VersionOptions

text :: AWS.FromText a => ReadM a
text = OA.eitherReader (AWS.fromText . Text.pack)

optsPackageIds :: Parser (Set Z.PackageId)
optsPackageIds =
  S.fromList . join <$> many
  ( OA.option packageIds
    (   OA.long "ignore-packages"
    <>  OA.help "Packages to ignore"
    <>  OA.metavar "PACKAGE_LIST"
    )
  )

packageIds :: ReadM [Text]
packageIds = OA.eitherReader \case
  "" -> pure []
  s -> pure $ T.split (== ',') (T.pack s)
