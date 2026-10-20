module Distribution.Solver.Modular.ConfiguredConversion
    ( convCPs
    ) where

import Data.Maybe
import Prelude hiding (pi)
import Data.Either (partitionEithers)
import qualified Data.List as L
import qualified Data.Map.Strict as M

import Distribution.Package (UnitId)
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Utils.Generic (ordNubBy)
import Distribution.Compat.Graph (IsNode(..))

import qualified Distribution.Simple.PackageIndex as SI

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Package

import           Distribution.Solver.Types.ComponentDeps (Component)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.OptionalStanza (OptionalStanzaSet)
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.PackagePath
import           Distribution.Solver.Types.ResolverPackage
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.SolverPackage
import           Distribution.Solver.Types.InstSolverPackage
import           Distribution.Solver.Types.SourcePackage

-- | Converts the solver specific result, one @CP QPN@ per goal, into
-- 'ResolverPackage's, which can then be converted into the install plan.
--
-- Several goals can resolve to the same package version: goals that the
-- solver linked, and goals in different qualified scopes (a build tool, a
-- setup script) that chose the same version on their own. Goals that agree on
-- the package instance, its flags, its stanzas and (recursively) its
-- dependencies are one package and produce a single 'ResolverPackage'. Goals
-- that disagree are different instances of the same version, see
-- Note [Instances of the same package version].
convCPs :: SI.InstalledPackageIndex ->
           CI.PackageIndex (SourcePackage loc) ->
           [CP QPN] -> [ResolverPackage loc]
convCPs iidx sidx cps =
    ordNubBy nodeKey $ map (convCP iidx sidx (instanceQualifiers cps)) cps

{- Note [Instances of the same package version]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A build tool and a setup script are solved in their own qualified scope (see
'Qualifier'), so the solver can choose for them a version that differs from
the one used elsewhere in the plan. It can also choose the same version with
different dependencies: when GHC builds Win32 from source, hsc2hs (a build tool
of Win32) depends on process, whose new version depends on the new Win32, so
the hsc2hs that builds Win32 must be built against the installed process and
Win32 instead. The plan then holds two instances of the same hsc2hs version.

The goals of the solver result are grouped into instances: two goals are the
same instance when they chose the same package instance with the same flags
and stanzas, and their dependencies are the same instances. Goals that the
solver linked always are. In the common case there is one instance per
package version, and its 'SolverId' is the plain @'PlannedId' pkgid 'Nothing'@
it has always been. When there are more, the one chosen for the smallest
package path (the top-level goal, if any) keeps 'Nothing', and each other gets
the smallest package path of its own goals, which no other instance shares.
-}

-- | For each goal of the solver result, how to tell the instance it resolved
-- to apart from other instances of the same package version.
--
-- The 'CP's come in dependency order (see 'toCPs'), so the instances of the
-- dependencies of a goal are known by the time the goal is considered.
instanceQualifiers :: [CP QPN] -> QPN -> Maybe PackagePath
instanceQualifiers cps = \qpn -> M.findWithDefault Nothing qpn qualifiers
  where
    -- The instance each goal resolves to, numbered in order of appearance.
    goalInstance :: M.Map QPN Int
    instancesByKey :: M.Map InstanceKey Int
    (goalInstance, instancesByKey) = L.foldl' addGoal (M.empty, M.empty) cps

    addGoal (goals, instances) (CP (PI qpn i) fa ss deps) =
      case M.lookup key instances of
        Just n  -> (M.insert qpn n goals, instances)
        Nothing -> let n = M.size instances
                   in (M.insert qpn n goals, M.insert key n instances)
      where
        key = InstanceKey (qpnName qpn) i fa ss
                (L.sort [ (comp, L.sort (map depInstance ds))
                        | (comp, ds) <- CD.toList deps ])
        depInstance (PI _ (I _ (Inst uid))) = Left uid
        depInstance (PI dqpn _) =
          Right $ fromMaybe
            (error $ "instanceQualifiers: dependency " ++ showQPN dqpn
                     ++ " of " ++ showQPN qpn ++ " has not been seen yet")
            (M.lookup dqpn goals)

    -- The goals of each instance.
    instanceGoals :: M.Map Int [QPN]
    instanceGoals = M.fromListWith (++) [ (n, [qpn]) | (qpn, n) <- M.toList goalInstance ]

    -- The smallest package path among the goals of each instance.
    instancePath :: M.Map Int PackagePath
    instancePath = M.map (minimum . map qpnPath) instanceGoals

    -- The instances of each package version, smallest package path first.
    instancesOfVersion :: M.Map (PN, I) [(PackagePath, Int)]
    instancesOfVersion =
      M.map L.sort $ M.fromListWith (++)
        [ ((ikName key, ikInstance key), [(instancePath M.! n, n)])
        | (key, n) <- M.toList instancesByKey ]

    -- Only the second and later instances of a package version need telling
    -- apart.
    instanceQualifier :: M.Map Int (Maybe PackagePath)
    instanceQualifier = M.fromList
      [ (n, q)
      | insts <- M.elems instancesOfVersion
      , ((path, n), first) <- zip insts (True : repeat False)
      , let q = if first then Nothing else Just path ]

    qualifiers :: M.Map QPN (Maybe PackagePath)
    qualifiers = M.map (instanceQualifier M.!) goalInstance

    qpnName (Q _ pn) = pn
    qpnPath (Q pp _) = pp

-- | What makes a package instance: the instance the solver chose (a version
-- and a location), its flags and stanzas, and the instances of its
-- dependencies (pre-existing ones by 'UnitId', planned ones by number).
data InstanceKey = InstanceKey
  { ikName      :: PN
  , ikInstance  :: I
  , _ikFlags    :: FlagAssignment
  , _ikStanzas  :: OptionalStanzaSet
  , _ikDeps     :: [(Component, [Either UnitId Int])]
  }
  deriving (Eq, Ord)

convCP :: SI.InstalledPackageIndex ->
          CI.PackageIndex (SourcePackage loc) ->
          (QPN -> Maybe PackagePath) ->
          CP QPN -> ResolverPackage loc
convCP iidx sidx qualifierOf (CP qpi@(PI qpn _) fa es ds) =
  case convPI qpi of
    Left  pi -> PreExisting $
                  InstSolverPackage {
                    instSolverPkgIPI = fromJust $ SI.lookupUnitId iidx pi,
                    instSolverPkgLibDeps = fmap fst ds',
                    instSolverPkgExeDeps = fmap snd ds'
                  }
    Right pi -> Configured $
                  SolverPackage {
                      solverPkgSource = srcpkg,
                      solverPkgFlags = fa,
                      solverPkgStanzas = es,
                      solverPkgLibDeps = fmap fst ds',
                      solverPkgExeDeps = fmap snd ds',
                      solverPkgQualifier = qualifierOf qpn
                    }
      where
        srcpkg = fromMaybe (error "convCP: lookupPackageId failed") $ CI.lookupPackageId sidx pi
  where
    ds' :: CD.ComponentDeps ([SolverId] {- lib -}, [SolverId] {- exe -})
    ds' = fmap (partitionEithers . map (convConfId qualifierOf)) ds

convPI :: PI QPN -> Either UnitId PackageId
convPI (PI _ (I _ (Inst pi))) = Left pi
convPI (PI (Q _ pn) (I v _))  = Right (PackageIdentifier pn v)

convConfId :: (QPN -> Maybe PackagePath) -> PI QPN -> Either SolverId {- is lib -} SolverId {- is exe -}
convConfId qualifierOf (PI qpn@(Q (PackagePath _ q) pn) (I v loc)) =
    case loc of
        Inst pi -> Left (PreExistingId sourceId pi)
        _otherwise
          | QualExe _ pn' <- q
          -- NB: the dependencies of the executable are also
          -- qualified.  So the way to tell if this is an executable
          -- dependency is to make sure the qualifier is pointing
          -- at the actual thing.  Fortunately for us, I was
          -- silly and didn't allow arbitrarily nested build-tools
          -- dependencies, so a shallow check works.
          , pn == pn' -> Right (PlannedId sourceId (qualifierOf qpn))
          | otherwise    -> Left  (PlannedId sourceId (qualifierOf qpn))
  where
    sourceId    = PackageIdentifier pn v
