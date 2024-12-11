module HaskellWorks.CabalCache.Concurrent.Type
  ( DownloadQueue(..),
    ConsumerId,
    ProviderId,
    PackageId,
  ) where

import GHC.Generics                  (Generic)
import HaskellWorks.CabalCache.Types (PackageId)

import qualified Control.Concurrent.STM as STM
import qualified Data.Relation          as R
import qualified Data.Set               as S

type ConsumerId = PackageId
type ProviderId = PackageId

data DownloadQueue = DownloadQueue
  { tDependencies :: STM.TVar (R.Relation ConsumerId ProviderId)
  , tUploading    :: STM.TVar (S.Set PackageId)
  , tFailures     :: STM.TVar (S.Set PackageId)
  } deriving Generic
