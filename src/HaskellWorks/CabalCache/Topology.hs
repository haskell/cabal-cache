{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module HaskellWorks.CabalCache.Topology
( PlanData(..)
, buildPlanData
, canShare
)
where

import Control.Arrow                 ((&&&))
import Control.Lens                  (each, set, view, (&), (.~), (<&>), (^.), (^..))
import Control.Monad                 (join)
import Data.Either                   (fromRight)
import Data.Generics.Product.Any     (the)
import Data.Map.Strict               (Map)
import Data.Maybe                    (fromMaybe, mapMaybe)
import Data.Set                      (Set)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import HaskellWorks.CabalCache.Types (Package, PackageId, PlanJson)

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Topograph       as TG

data PlanData = PlanData
  { depsMap      :: Map PackageId (Set PackageId)
  , revDepsMap   :: Map PackageId (Set PackageId)
  , nonShareable :: Set PackageId
  } deriving Generic

buildPlanData :: PlanJson   -- ^ The original plan
  -> [PackageId]              -- ^ Packages that are known to be non-shareable
  -> PlanData               -- ^ Updated plan
buildPlanData plan nonShareablePkgs =
  let
    dm = dependenciesMap (plan ^. the @"installPlan")
  in buildPlanData' dm nonShareablePkgs

canShare :: PlanData -> PackageId -> Bool
canShare planData pkgId =
  Set.notMember pkgId (nonShareable planData)

-------------------------------------------------------------------------------

dependenciesMap :: [Package] -> Map PackageId (Set PackageId)
dependenciesMap plan =
  plan <&> (view (the @"id") &&& view (the @"depends"))
       <&> fmap Set.fromList & Map.fromList


buildPlanData' :: Map PackageId (Set PackageId)     -- ^ Dependencies map
  -> [PackageId]                                          -- ^ Packages to exclude
  -> PlanData                              -- ^ All package ids to exclude
buildPlanData' plan knownNonShareable =
  fromRight (error "Could not process dependencies") $
    TG.runG plan $ \g ->
      let
        tg        = TG.transpose g
        nsPaths   = concatMap (fromMaybe [] . paths tg) knownNonShareable
        nsAll     = Set.fromList (join nsPaths)
        dMap      = TG.adjacencyMap (TG.reduction g)
        rdMap     = TG.adjacencyMap (TG.reduction tg)
      in PlanData { depsMap = dMap, revDepsMap = rdMap, nonShareable = nsAll }
  where
    paths g x = (fmap . fmap . fmap) (TG.gFromVertex g) $ TG.dfs g <$> TG.gToVertex g x
