{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module HaskellWorks.CabalCache.Core
  ( PackageInfo(..)
  , Tagged(..)
  , Presence(..)
  , getPackages
  , relativePaths
  , loadPlan
  , mkCompilerContext
  ) where

import Control.DeepSeq (NFData)
import Control.Lens ((<&>), (&), (^.))
import Control.Monad.Catch (SomeException, MonadCatch(catch))
import Control.Monad.Except (MonadIO(liftIO), forM)
import Data.Aeson (eitherDecode)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Generics.Product.Any (the)
import Data.String (IsString(fromString))
import Data.Text (Text)
import GHC.Generics (Generic)
import HaskellWorks.CabalCache.AppError (AppError)
import HaskellWorks.CabalCache.Error (nothingToError)
import HaskellWorks.CabalCache.Show (tshow)
import Polysemy (Member, Sem)
import System.FilePath ((<.>), (</>))

import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.List                              as L
import qualified Data.Text                              as T
import qualified HaskellWorks.CabalCache.IO.Tar         as IO
import qualified HaskellWorks.CabalCache.Types          as Z
import qualified Polysemy.ConstraintAbsorber.MonadCatch as PY
import qualified Polysemy.Embed                         as PY
import qualified Polysemy.Error                         as PY
import qualified System.Directory                       as IO
import qualified System.Process                         as IO

{- HLINT ignore "Monoid law, left identity" -}

type PackageDir = FilePath
type ConfPath   = FilePath
type Library    = FilePath

data Presence   = Present | Absent deriving (Eq, Show, NFData, Generic)

data Tagged a t = Tagged
  { value :: a
  , tag   :: t
  } deriving (Eq, Show, Generic, NFData)

data PackageInfo = PackageInfo
  { compilerId :: Z.CompilerId
  , packageId  :: Z.PackageId
  , packageDir :: PackageDir
  , confPath   :: Tagged ConfPath Presence
  , libs       :: [Library]
  } deriving (Show, Eq, Generic, NFData)

(<||>) :: ()
  => Member (PY.Error e) r
  => Sem r a
  -> Sem r a
  -> Sem r a
(<||>) f g = f `PY.catch` const g

findExecutable :: ()
  => Member (PY.Error Text) r
  => Member (PY.Embed IO  ) r
  => Text
  -> Sem r Text
findExecutable exe = fmap T.pack $
  liftIO (IO.findExecutable (T.unpack exe)) >>= nothingToError (exe <> " is not in path")

runGhcPkg :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Error Text         ) r
  => Text
  -> [Text]
  -> Sem r Text
runGhcPkg cmdExe args = PY.absorbMonadCatch $
  catch (liftIO $ T.pack <$> IO.readProcess (T.unpack cmdExe) (fmap T.unpack args) "") $
    \(e :: IOError) -> PY.throw $ "Unable to run " <> cmdExe <> " " <> T.unwords args <> ": " <> tshow e

verifyGhcPkgVersion :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error Text         ) r
  => Member (PY.Error SomeException) r
  => Text
  -> Text
  -> Sem r Text
verifyGhcPkgVersion version cmdExe = do
  stdout <- runGhcPkg cmdExe ["--version"]
  if T.isSuffixOf (" " <> version) (mconcat (L.take 1 (T.lines stdout)))
    then return cmdExe
    else PY.throw $ cmdExe <> "has is not of version " <> version

mkCompilerContext :: ()
  => Member (PY.Embed IO           ) r
  => Member (PY.Error SomeException) r
  => Member (PY.Error Text         ) r
  => Z.PlanJson
  -> Sem r Z.CompilerContext
mkCompilerContext plan = do
  compilerVersion <- T.stripPrefix "ghc-" (plan ^. the @"compilerId") & nothingToError "No compiler version available in plan"
  let versionedGhcPkgCmd = "ghc-pkg-" <> compilerVersion
  ghcPkgCmdPath <- (<||>) @Text
    (findExecutable versionedGhcPkgCmd  >>= verifyGhcPkgVersion compilerVersion)
    (findExecutable "ghc-pkg"           >>= verifyGhcPkgVersion compilerVersion)
  return (Z.CompilerContext [T.unpack ghcPkgCmdPath])

relativePaths :: FilePath -> PackageInfo -> [IO.TarGroup]
relativePaths basePath pInfo =
  [ IO.TarGroup basePath $ mempty
      <> (pInfo ^. the @"libs")
      <> [packageDir pInfo]
  , IO.TarGroup basePath $ mempty
      <> ([pInfo ^. the @"confPath"] & filter ((== Present) . (^. the @"tag")) <&> (^. the @"value"))
  ]

getPackages :: FilePath -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packages (mkPackageInfo basePath compilerId')
  where compilerId' :: Text
        compilerId' = planJson ^. the @"compilerId"
        packages :: [Z.Package]
        packages = planJson ^. the @"installPlan"

loadPlan :: FilePath -> IO (Either AppError Z.PlanJson)
loadPlan buildPath = first fromString . eitherDecode <$> LBS.readFile (buildPath </> "cache" </> "plan.json")

-------------------------------------------------------------------------------
mkPackageInfo :: FilePath -> Z.CompilerId -> Z.Package -> IO PackageInfo
mkPackageInfo basePath cid pkg = do
  let pid               = pkg ^. the @"id"
  let compilerPath      = basePath </> T.unpack cid
  let relativeConfPath  = T.unpack cid </> "package.db" </> T.unpack pid <.> ".conf"
  let absoluteConfPath  = basePath </> relativeConfPath
  let libPath           = compilerPath </> "lib"
  let relativeLibPath   = T.unpack cid </> "lib"
  let libPrefix         = "libHS" <> pid
  absoluteConfPathExists <- IO.doesFileExist absoluteConfPath
  libFiles <- getLibFiles relativeLibPath libPath libPrefix
  return PackageInfo
    { compilerId  = cid
    , packageId   = pid
    , packageDir  = T.unpack cid </> T.unpack pid
    , confPath    = Tagged relativeConfPath (bool Absent Present absoluteConfPathExists)
    , libs        = libFiles
    }

getLibFiles :: FilePath -> FilePath -> Text -> IO [Library]
getLibFiles relativeLibPath libPath libPrefix = do
  libExists <- IO.doesDirectoryExist libPath
  if libExists
     then fmap (relativeLibPath </>) . filter (L.isPrefixOf (T.unpack libPrefix)) <$> IO.listDirectory libPath
     else pure []
