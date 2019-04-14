module HaskellWorks.Ci.Assist.Tar
where

import Codec.Archive.Tar
import Codec.Archive.Tar.Entry
import Data.Either             (fromRight)

import qualified Data.ByteString.Lazy as LBS

updateEntryWith :: (FilePath -> Bool)
  -> (LBS.ByteString -> LBS.ByteString)
  -> Entry
  -> Entry
updateEntryWith pred transform entry =
  if pred (entryPath entry)
    then case entryContent entry of
        NormalFile bs size ->
          let bs' = transform bs
          in entry { entryContent = NormalFile bs' (LBS.length bs') }
        _ -> entry
    else entry

mapEntriesWith :: (FilePath -> Bool)
  -> (LBS.ByteString -> LBS.ByteString)
  -> Entries e
  -> Entries e
mapEntriesWith pred transform =
  mapEntriesNoFail (updateEntryWith pred transform)

rewritePath :: (FilePath -> FilePath) -> Entry -> Entry
rewritePath transform entry =
  let
    tp = entryTarPath entry
    isDir = entryContent entry == Directory
    fp = fromTarPath tp
    tp' = fromRight tp (toTarPath isDir (transform fp))
  in entry { entryTarPath = tp' }

