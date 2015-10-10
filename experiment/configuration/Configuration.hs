module Configuration where

import qualified Predicate
import Predicate (Predicate)

import qualified ConditionTree
import qualified PredicateExpr
import PredicateExpr (PredicateExpr)
import ConditionTree (ConditionTree)
import qualified BasePredicate
import BasePredicate (BasePredicate, SystemParams)
import PartialValuation (PartialValuation, TotalValuation)

import Distribution.Package
         ( Dependency(..), PackageName )
import Distribution.Version
         ( VersionRange )

import Distribution.PackageDescription
         ( FlagName, FlagAssignment )

import Data.Monoid
         ( Monoid(..) )
import Data.Foldable
         ( Foldable(fold) )
import Data.List
         ( partition )
import Data.Maybe
         ( isNothing )
import qualified Data.Map as Map
import Data.Map (Map)
         

flatten :: GenericPackageDescription -> PackageDescription
flatten = PackageDescription . ConditionTree.flatten . pkgCondTree

-- | Finalise a generic package description by resolving all the conditonals.
-- It requires a full flag assignment. Use 'search' if you do not know the
-- flags or only have a partial flag assignment.
--
-- It returns Nothing if and only if the flag assignment is does not to cover
-- all flags used in conditions. (CHECK this! does short-cut evaluation mean
-- some flags may not be needed?)
--
finalise :: SystemParams -> FlagAssignment
         -> GenericPackageDescription -> Maybe PackageDescription
finalise sysParams flagAssignment =
    fmap PackageDescription
  . ConditionTree.resolve' (Predicate.eval sysParams flagAssignment)
  . pkgCondTree

--apply partial flag assignment
-- :: FlagAssignment
-- -> GenericPackageDescription -> GenericPackageDescription
simplify :: PartialValuation BasePredicate
         -> GenericPackageDescription -> GenericPackageDescription
simplify v pkg =
    pkg {
      pkgCondTree = ConditionTree.simplify evalOrSimplify (pkgCondTree pkg)
    }
  where
    evalOrSimplify :: Predicate -> Either Bool Predicate
    evalOrSimplify cond = case PredicateExpr.simplify v cond of
      PredicateExpr.PredExprLit b -> Left b
      other                       -> Right other
    
type ResolveError = ()

resolve :: FlagAssignment
        -> (Constraint -> Bool)
        -> SystemParams
        -> [Constraint]
        -> GenericPackageDescription
        -> Either ResolveError FlagAssignment
resolve initialFlags satisfyDep sysParams extraConstraints pkg =
    search
      satisfiable
      flagDoms
      (simplifyTree (pkgCondTree pkg))

  where
    mkConstraints :: BuildInfo -> Constraints
    mkConstraints = undefined

    satisfiable   :: Constraints -> Maybe ResolveError
    satisfiable   SatisfiableConstraints   = Nothing
    satisfiable   UnsatisfiableConstraints = Just ()

    flagDoms      :: [FlagDomain]   
    flagDoms      = undefined

    --TODO: apply the initial flag assignment, and manual flags
    --      extract the remaining used flags after simplifying
    --      construct all test flag assignments

    --TODO: merge constraints on the same level

    simplifyTree :: ConditionTree (PredicateExpr BasePredicate) BuildInfo
                 -> ConditionTree (PredicateExpr FlagName)      Constraints
    simplifyTree = fmap mkConstraints
                 . ConditionTree.simplify (literal . simplifyExpr)
      where
        literal (PredicateExpr.PredExprLit b) = Left b
        literal e                             = Right e

    simplifyExpr :: PredicateExpr BasePredicate
                  -> PredicateExpr FlagName
    simplifyExpr =
      PredicateExpr.simplify' (BasePredicate.applySystemParams sysParams)


search :: forall constraints error
        . Monoid constraints
       => (constraints -> Maybe error)
       -> [FlagDomain]
       -> ConditionTree (PredicateExpr FlagName) constraints
       -> Either error FlagAssignment
search satisfiable flagDoms tree =
    case satisfiable (mconcat (ConditionTree.unconditional tree)) of
      Just err -> Left err
      Nothing  -> tryAllAssignments Nothing assignments

  where
    -- Find the first assignment that works,
    -- though if all fail then return the first failure
    tryAllAssignments (Just err) []     = Left err
    tryAllAssignments Nothing    []     = error "impossible"
    tryAllAssignments merr       (a:as) =
      case checkAssignment a tree of
        Just err' | isNothing merr -> tryAllAssignments (Just err') as
                  | otherwise      -> tryAllAssignments merr        as
        Nothing   -> Right a

    assignments = allAssignments (reorderFlagDomains flagDoms)

    checkAssignment :: FlagAssignment
                    -> ConditionTree (PredicateExpr FlagName) constraints
                    -> Maybe error
    checkAssignment flagAssignment =
        satisfiable
      . ConditionTree.resolve (PredicateExpr.eval (evalFlags flagAssignment))

    evalFlags :: FlagAssignment -> TotalValuation FlagName
    evalFlags flagAssignment flag =
      case lookup flag flagAssignment of
        Just val -> val
        Nothing  -> error "missing flag"

    -- | Generate all possible flag assignements given the domain of each flag
    allAssignments :: [FlagDomain] -> [FlagAssignment]
    allAssignments []                 = [ [] ]
    allAssignments ((flag, dom):doms) = [ (flag, val):flags
                                        | val   <- dom
                                        , flags <- allAssignments doms ]

    -- | If flags can only take one possible value then put them first
    reorderFlagDomains :: [FlagDomain] -> [FlagDomain]
    reorderFlagDomains doms =
      let (singles, rest) = partition ((==1) . length . snd) doms
       in singles ++ rest


type FlagDomain = (FlagName, [Bool])


data GenericPackageDescription = GenericPackageDescription {
  flags       :: [Flag],
  pkgCondTree :: ConditionTree Predicate BuildInfo
}
 
data Flag = Flag {
    flagName        :: FlagName,
    flagDefault     :: Bool,
    flagManual      :: Bool
  }
  deriving (Show, Eq)

data PackageDescription = PackageDescription {
    buildInfo :: BuildInfo
  }

data BuildInfo = BuildInfo {
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

data Constraint = HaskellLibPackage PackageName VersionRange
                | PkgConfigPackage  PackageName VersionRange
                | CLib String
                | BuildTool String

data Constraints
   = SatisfiableConstraints {
       hsLibVersions     :: Map PackageName VersionRange,
       pkgConfigVersions :: Map PackageName VersionRange
       
     }
   | UnsatisfiableConstraints

instance Monoid Constraints where
