{-# LANGUAGE OverloadedStrings #-}
module HaskellWorks.Ci.Assist.PackageConfig
where

import Control.Lens                ((&), (<&>))
import Data.ByteString.Char8       (pack)
import Data.ByteString.Lazy.Search (replace)
import Data.Maybe                  (fromMaybe)
import HaskellWorks.Ci.Assist.Tar
import System.FilePath             (pathSeparator, (</>))

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as List

import Debug.Trace

storePathMacro :: BS.ByteString
storePathMacro = "${STORE_PATH}"

templateConfig :: FilePath -> LBS.ByteString -> LBS.ByteString
templateConfig storePath = replace (pack storePath) storePathMacro

unTemplateConfig :: FilePath -> LBS.ByteString -> LBS.ByteString
unTemplateConfig storePath = replace storePathMacro (pack storePath)

replacePrefix :: FilePath -> FilePath -> FilePath -> FilePath
replacePrefix old new path =
  List.stripPrefix old path
    <&> List.drop 1
    <&> (\x -> new </> x)
    & fromMaybe path
