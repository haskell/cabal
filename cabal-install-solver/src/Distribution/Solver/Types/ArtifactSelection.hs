{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Provide a type for categorizing artifact requirements.
module Distribution.Solver.Types.ArtifactSelection
    ( ArtifactSelection(..)
    , ArtifactKind(..)
    , allArtifacts
    , dynOutsOnly
    , staticOutsOnly
    , noOuts
    , unArtifactSelection
    , artsSubsetOf
    , artsDifference
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.Set as S

import Distribution.Pretty ( Pretty(pretty) )
import qualified Text.PrettyPrint as PP

-- | A type for specifying which artifacts are available to be required.
newtype ArtifactSelection = ArtifactSelection (S.Set ArtifactKind)
  deriving (Eq, Show, Ord, Generic, Semigroup, Monoid)

instance Pretty ArtifactSelection where
  pretty arts
    | arts == allArtifacts   = PP.text "all artifacts"
    | arts == dynOutsOnly    = PP.text "dynamic artifacts"
    | arts == staticOutsOnly = PP.text "static artifacts"
    | arts == noOuts         = PP.text "no output artifacts"
    | otherwise              = PP.text "unknown artifacts"

instance Binary ArtifactSelection
instance Structured ArtifactSelection

-- | Specific kinds of artifacts.
data ArtifactKind
  = DynOuts     -- ^ Exclude static outputs.
  | StaticOuts  -- ^ Exclude dynamic outputs.
  deriving (Eq, Show, Generic, Ord)

instance Binary ArtifactKind
instance Structured ArtifactKind

-- | ArtifactSelection alias: e.g. dynamic and static interface files.
allArtifacts :: ArtifactSelection
allArtifacts = ArtifactSelection $ S.fromList [DynOuts, StaticOuts]

-- | ArtifactSelection alias: exclude static outputs.
dynOutsOnly :: ArtifactSelection
dynOutsOnly = ArtifactSelection $ S.fromList [DynOuts]

-- | ArtifactSelection alias: exclude static outputs.
staticOutsOnly :: ArtifactSelection
staticOutsOnly = ArtifactSelection $ S.fromList [StaticOuts]

-- | ArtifactSelection alias: exclude all artifacts.
noOuts :: ArtifactSelection
noOuts = ArtifactSelection $ S.fromList []

-- | Obtain the set of artifact kinds included in this artifact selection.
unArtifactSelection :: ArtifactSelection -> S.Set ArtifactKind
unArtifactSelection (ArtifactSelection set) = set

-- | Is a selection a subset of another?
artsSubsetOf :: ArtifactSelection -> ArtifactSelection -> Bool
artsSubsetOf = S.isSubsetOf `on` unArtifactSelection

-- | Return artifacts in the first set not present in the second set.
artsDifference :: ArtifactSelection -> ArtifactSelection -> ArtifactSelection
artsDifference (ArtifactSelection a) (ArtifactSelection b) =
  ArtifactSelection $ a `S.difference` b
