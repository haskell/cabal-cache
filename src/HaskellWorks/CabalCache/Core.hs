module HaskellWorks.CabalCache.Core
  ( PackageInfo(..),
    Tagged(..),
    Presence(..),
    getPackages,
    relativePaths,
    loadPlan,
    mkCompilerContext,
  ) where

import Control.DeepSeq                  (NFData)
import Data.Aeson                       (eitherDecode)
import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Core.Exception
import HaskellWorks.CabalCache.Error    (DecodeError(..))
import HaskellWorks.Prelude
import System.FilePath                  ((<.>), (</>))

import qualified Data.ByteString.Lazy           as LBS
import qualified Data.List                      as L
import qualified Data.Text                      as T
import qualified HaskellWorks.CabalCache.IO.Tar as IO
import qualified HaskellWorks.CabalCache.Types  as Z
import qualified System.Directory               as IO
import qualified System.Info                    as I
import qualified System.Process                 as IO

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
  { compilerId  :: Z.CompilerId
  , packageId   :: Z.PackageId
  , packageName :: Z.PackageName
  , packageDir  :: PackageDir
  , confPath    :: Tagged ConfPath Presence
  , libs        :: [Library]
  } deriving (Show, Eq, Generic, NFData)

isPosix :: Bool
isPosix = I.os /= "mingw32"
{-# NOINLINE isPosix #-}

exeExt :: String
exeExt
  | isPosix = ""
  | otherwise = ".exe"

withExeExt :: FilePath -> FilePath
withExeExt = (<.> exeExt)

withExeExt' :: Text -> Text
withExeExt' = T.pack . withExeExt . T.unpack

findExecutable :: ()
  => r <: Error Text
  => r <: IOE
  => Text
  -> Eff r Text
findExecutable exe =
  liftIO (fmap T.pack <$> IO.findExecutable (T.unpack exe))
    & onNothingM (throw (exe <> " is not in path"))

runGhcPkg :: ()
  => r <: Error Text
  => r <: IOE
  => Text
  -> [Text]
  -> Eff r Text
runGhcPkg cmdExe args = catchIO (liftIO $ T.pack <$> IO.readProcess (T.unpack cmdExe) (fmap T.unpack args) "") $
  \(e :: IOError) -> throw $ "Unable to run " <> cmdExe <> " " <> T.unwords args <> ": " <> tshow e

verifyGhcPkgVersion :: ()
  => r <: Error Text
  => r <: IOE
  => Text
  -> Text
  -> Eff r Text
verifyGhcPkgVersion version cmdExe = do
  stdout <- runGhcPkg cmdExe ["--version"]
  if T.isSuffixOf (" " <> version) (mconcat (L.take 1 (T.lines stdout)))
    then return cmdExe
    else throw $ cmdExe <> " is not of version " <> version

mkCompilerContext :: ()
  => r <: Error Text
  => r <: IOE
  => Z.PlanJson
  -> Eff r Z.CompilerContext
mkCompilerContext plan = do
  compilerVersion <- T.stripPrefix "ghc-" plan.compilerId
    & onNothing (throw @Text "No compiler version available in plan")

  let versionedGhcPkgCmd = "ghc-pkg-" <> compilerVersion

  ghcPkgCmdPath <- (findExecutable (withExeExt' versionedGhcPkgCmd)  >>= verifyGhcPkgVersion compilerVersion)
    & trap_ @Text (findExecutable (withExeExt' "ghc-pkg"         )  >>= verifyGhcPkgVersion compilerVersion)

  return (Z.CompilerContext [T.unpack ghcPkgCmdPath])

relativePaths :: FilePath -> PackageInfo -> [IO.TarGroup]
relativePaths basePath pInfo =
  [ IO.TarGroup basePath $ mempty
      <> pInfo.libs
      <> [pInfo.packageDir]
  , IO.TarGroup basePath $ mempty
      <> ([pInfo.confPath] & L.filter (\c -> c.tag == Present) <&> (.value))
  ]

getPackages :: FilePath -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packages (mkPackageInfo basePath compilerId')
  where compilerId' :: Text
        compilerId' = planJson.compilerId
        packages :: [Z.Package]
        packages = planJson.installPlan

loadPlan :: ()
  => r <: Error DecodeError
  => r <: IOE
  => FilePath
  -> Eff r Z.PlanJson
loadPlan resolvedBuildPath = do
  lbs <- liftIO (LBS.readFile (resolvedBuildPath </> "cache" </> "plan.json"))
  a <- eitherDecode lbs
    & onLeft (throw . DecodeError . T.pack)

  pure do a :: Z.PlanJson

-------------------------------------------------------------------------------
mkPackageInfo :: FilePath -> Z.CompilerId -> Z.Package -> IO PackageInfo
mkPackageInfo basePath cid pkg = do
  let pid               = pkg.id
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
    , packageName = pkg.name
    , packageDir  = T.unpack cid </> T.unpack pid
    , confPath    = Tagged relativeConfPath (bool Absent Present absoluteConfPathExists)
    , libs        = libFiles
    }

getLibFiles :: FilePath -> FilePath -> Text -> IO [Library]
getLibFiles relativeLibPath libPath libPrefix = do
  libExists <- IO.doesDirectoryExist libPath
  if libExists
     then fmap (relativeLibPath </>) . L.filter (L.isPrefixOf (T.unpack libPrefix)) <$> IO.listDirectory libPath
     else pure []
