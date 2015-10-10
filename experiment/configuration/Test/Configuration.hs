module Test.Configuration where

import qualified Predicate
import Predicate (Predicate)

import BasePredicate (SystemParams)

import Distribution.Package
         ( Dependency(..) )

import Distribution.PackageDescription
         ( FlagAssignment )

import Data.Monoid
         ( Monoid(..) )
import Data.Foldable
         ( Foldable(fold) )

flatten :: GenericPackageDescription -> PackageDescription
flatten = PackageDescription . flattenConditionTree

finalise :: FlagAssignment -> SystemParams
         -> GenericPackageDescription -> PackageDescription
finalise = error "TODO: finalise"

--apply partial flag assignment
-- :: FlagAssignment
-- -> GenericPackageDescription -> GenericPackageDescription

type DepInfo = ()

search :: SystemParams -> DepInfo
       -> GenericPackageDescription -> Maybe FlagAssignment
search = error "TODO: search"



type GenericPackageDescription
   = ConditionTree Predicate BuildInfo
 

data PackageDescription = PackageDescription {
  buildInfo :: BuildInfo
}

data BuildInfo = BuildInfo  {
  buildDepends      :: [Dependency],
  pkgconfigDepends  :: [Dependency],
  extralibs         :: [String]
}

instance Monoid BuildInfo where
  mempty       = BuildInfo {
    buildDepends      = [],
    pkgconfigDepends  = [],
    extralibs         = []
  }
  mappend a b = BuildInfo {
    buildDepends      = combine buildDepends,
    pkgconfigDepends  = combine pkgconfigDepends,
    extralibs         = combine extralibs
  }
    where combine f = f a `mappend` f b
