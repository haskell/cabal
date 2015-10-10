module BasePredicate (
  
  BasePredicate(..),
  eval,
  applyOS,
  applyArch,
  applyCompiler,
  applyFlagAssignment,
  
  applySystemParams,

  --TODO: move elsewhere:
  SystemParams(..)
  
  ) where

import PartialValuation

import Distribution.Version
         ( VersionRange(..), withinRange )
import Distribution.System
         ( OS(..), Arch(..) )
import Distribution.Compiler
         ( CompilerFlavor, CompilerId(..) )

import Distribution.PackageDescription
         ( FlagName, FlagAssignment )

import Data.Monoid
         ( Monoid(..) )

data SystemParams = SystemParams OS Arch CompilerId

data BasePredicate
   = OSPredicate       OS
   | ArchPredicate     Arch
   | CompilerPredicate CompilerFlavor VersionRange
   | FlagPredicate     FlagName
  deriving (Eq, Show)

-- | Evaluate a 'BasePredicate'. It will return 'Nothing' if and only if the
-- 'FlagAssignment' is not total.
--
-- You can use this along with 'PredicateExpr.eval'' to give a valuation on a
-- whole 'PredicateExpr':
--
-- > PredicateExpr.eval' (eval sysParms flags)
-- >   :: PredicateExpr BasePredicate -> Maybe Bool
--
eval :: SystemParams
     -> FlagAssignment
     -> PartialValuation BasePredicate
eval (SystemParams os arch (CompilerId comp ver)) flagAssignment =
    PartialValuation peval
  where
    peval (OSPredicate   os')             = Just $! os   == os'
    peval (ArchPredicate arch')           = Just $! arch == arch'
    peval (CompilerPredicate comp' range) = Just $! comp == comp'
                                                 && ver `withinRange` range
    peval (FlagPredicate flag)            = lookup flag flagAssignment

applyOS :: OS -> PartialValuation BasePredicate
applyOS os = PartialValuation peval
  where
    peval (OSPredicate os') = Just $! os == os'
    peval _                 = Nothing

applyArch :: Arch -> PartialValuation BasePredicate
applyArch arch = PartialValuation peval
  where
    peval (ArchPredicate arch') = Just $! arch == arch'
    peval _                     = Nothing

applyCompiler :: CompilerId -> PartialValuation BasePredicate
applyCompiler (CompilerId comp ver) = PartialValuation peval
  where
    peval (CompilerPredicate comp' range) = Just $! comp == comp'
                                                 && ver `withinRange` range
    peval _                               = Nothing

-- | A 'PartialValuation' on a 'BasePredicate' obtained by applying a
-- 'FlagAssignment'. It gives a value in the case that the 'BasePredicate' is a
-- 'FlagPredicate' that is given a value by the 'FlagAssignment'.
--
-- This can be used to simplify a compound 'PredicateExpr' using:
--
-- > PredicateExpr.simplify (applyFlagAssignment flagAssignment)
-- >   :: PredicateExpr BasePredicate -> PredicateExpr BasePredicate
--
applyFlagAssignment :: FlagAssignment -> PartialValuation BasePredicate
applyFlagAssignment flagAssignment = PartialValuation peval
  where
    peval (FlagPredicate flag)  = lookup flag flagAssignment
    peval other                 = Nothing

--applyCompilerFlavor :: CompilerFlavor -> PartialValuation BasePredicate

--applyCompilerIds :: [CompilerId] -> PartialValuation BasePredicate


-- | Partially evaluate a 'BasePredicate' by supplying values for the
-- all the 'SystemParams', leaving only the flags.
--
applySystemParams :: SystemParams
                  -> (BasePredicate -> Either Bool FlagName)
applySystemParams (SystemParams os arch (CompilerId comp ver)) = peval
  where
    peval (OSPredicate   os')             = Left $ os   == os'
    peval (ArchPredicate arch')           = Left $ arch == arch'
    peval (CompilerPredicate comp' range) = Left $ comp == comp'
                                                && ver `withinRange` range
    peval (FlagPredicate flag)            = Right flag
