module App.Commands.Version
  ( cmdVersion,
  ) where

import App.Commands.Options.Parser (optsVersion)
import HaskellWorks.Prelude
import Options.Applicative         (Mod, CommandFields)

import qualified App.Commands.Options.Types         as Z
import qualified Data.Text                          as T
import qualified Data.Version                       as V
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Paths_cabal_cache                  as P
import qualified Options.Applicative                as OA
import qualified Data.List                          as L

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

runVersion :: Z.VersionOptions -> IO ()
runVersion _ = do
  let V.Version {..} = P.version
  let version = L.intercalate "." $ fmap show versionBranch

  CIO.putStrLn $ "cabal-cache " <> T.pack version

cmdVersion :: Mod CommandFields (IO ())
cmdVersion = OA.command "version" $ flip OA.info OA.idm $ runVersion <$> optsVersion
