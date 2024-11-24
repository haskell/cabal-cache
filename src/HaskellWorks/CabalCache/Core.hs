{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

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
import Control.Monad.Catch              (MonadCatch(..))
import Control.Monad.Except             (MonadError(..))
import Data.Aeson                       (eitherDecode)
import Data.Generics.Product.Any        (the)
import HaskellWorks.CabalCache.Error    (DecodeError(..))
import HaskellWorks.Prelude
import Lens.Micro
import System.FilePath                  ((<.>), (</>))

import qualified Control.Monad.Oops             as OO
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

(<||>) :: Monad m => ExceptT e m a -> ExceptT e m a -> ExceptT e m a
(<||>) f g = f `catchError` const g

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
  => MonadIO f
  => MonadError (OO.Variant e) f
  => e `OO.CouldBe` Text
  => Text
  -> f Text
findExecutable exe = fmap T.pack $
  liftIO (IO.findExecutable (T.unpack exe)) >>= OO.hoistMaybe (exe <> " is not in path")

runGhcPkg :: ()
  => MonadCatch m
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` Text
  => Text
  -> [Text]
  -> m Text
runGhcPkg cmdExe args = catch (liftIO $ T.pack <$> IO.readProcess (T.unpack cmdExe) (fmap T.unpack args) "") $
  \(e :: IOError) -> OO.throw $ "Unable to run " <> cmdExe <> " " <> T.unwords args <> ": " <> tshow e

verifyGhcPkgVersion :: ()
  => MonadError (OO.Variant e) m
  => MonadIO m
  => MonadCatch m
  => e `OO.CouldBe` Text
  => Text
  -> Text
  -> m Text
verifyGhcPkgVersion version cmdExe = do
  stdout <- runGhcPkg cmdExe ["--version"]
  if T.isSuffixOf (" " <> version) (mconcat (L.take 1 (T.lines stdout)))
    then return cmdExe
    else OO.throw $ cmdExe <> " is not of version " <> version

mkCompilerContext :: ()
  => MonadIO m
  => MonadCatch m
  => e `OO.CouldBe` Text
  => Z.PlanJson
  -> ExceptT (OO.Variant e) m Z.CompilerContext
mkCompilerContext plan = do
  compilerVersion <- T.stripPrefix "ghc-" (plan ^. the @"compilerId")
    & OO.hoistMaybe @Text "No compiler version available in plan"
  let versionedGhcPkgCmd = "ghc-pkg-" <> compilerVersion
  ghcPkgCmdPath <-
          (findExecutable (withExeExt' versionedGhcPkgCmd)  >>= verifyGhcPkgVersion compilerVersion)
    <||>  (findExecutable (withExeExt' "ghc-pkg"         )  >>= verifyGhcPkgVersion compilerVersion)
  return (Z.CompilerContext [T.unpack ghcPkgCmdPath])

relativePaths :: FilePath -> PackageInfo -> [IO.TarGroup]
relativePaths basePath pInfo =
  [ IO.TarGroup basePath $ mempty
      <> (pInfo ^. the @"libs")
      <> [packageDir pInfo]
  , IO.TarGroup basePath $ mempty
      <> ([pInfo ^. the @"confPath"] & L.filter ((== Present) . (^. the @"tag")) <&> (^. the @"value"))
  ]

getPackages :: FilePath -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packages (mkPackageInfo basePath compilerId')
  where compilerId' :: Text
        compilerId' = planJson ^. the @"compilerId"
        packages :: [Z.Package]
        packages = planJson ^. the @"installPlan"

loadPlan :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` DecodeError
  => FilePath
  -> m Z.PlanJson
loadPlan resolvedBuildPath = do
  lbs <- liftIO (LBS.readFile (resolvedBuildPath </> "cache" </> "plan.json"))
  a <- OO.hoistEither $ first (DecodeError . T.pack) (eitherDecode lbs)
  pure do a :: Z.PlanJson

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
    , packageName = pkg ^. the @"name"
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
