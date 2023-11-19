{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution.Solver.Modular.Validate (validateTree) where

-- Validation of the tree.
--
-- The task here is to make sure all constraints hold. After validation, any
-- assignment returned by exploration of the tree should be a complete valid
-- assignment, i.e., actually constitute a solution.

import Control.Monad (foldM, mzero, liftM2)
import Control.Monad.Reader (MonadReader, Reader, runReader, local, asks)
import Data.Either (lefts)
import Data.Function (on)

import qualified Data.List as L
import qualified Data.Set as S

import Language.Haskell.Extension (Extension, Language)

import Data.Map.Strict as M
import Distribution.Compiler (CompilerInfo(..))

import Distribution.Solver.Modular.Assignment
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb, pkgConfigPkgIsPresent)
import Distribution.Types.LibraryName
import Distribution.Types.PkgconfigVersionRange

-- In practice, most constraints are implication constraints (IF we have made
-- a number of choices, THEN we also have to ensure that). We call constraints
-- that for which the preconditions are fulfilled ACTIVE. We maintain a set
-- of currently active constraints that we pass down the node.
--
-- We aim at detecting inconsistent states as early as possible.
--
-- Whenever we make a choice, there are two things that need to happen:
--
--   (1) We must check that the choice is consistent with the currently
--       active constraints.
--
--   (2) The choice increases the set of active constraints. For the new
--       active constraints, we must check that they are consistent with
--       the current state.
--
-- We can actually merge (1) and (2) by saying the current choice is
-- a new active constraint, fixing the choice.
--
-- If a test fails, we have detected an inconsistent state. We can
-- disable the current subtree and do not have to traverse it any further.
--
-- We need a good way to represent the current state, i.e., the current
-- set of active constraints. Since the main situation where we have to
-- search in it is (1), it seems best to store the state by package: for
-- every package, we store which versions are still allowed. If for any
-- package, we have inconsistent active constraints, we can also stop.
-- This is a particular way to read task (2):
--
--   (2, weak) We only check if the new constraints are consistent with
--       the choices we've already made, and add them to the active set.
--
--   (2, strong) We check if the new constraints are consistent with the
--       choices we've already made, and the constraints we already have.
--
-- It currently seems as if we're implementing the weak variant. However,
-- when used together with 'preferEasyGoalChoices', we will find an
-- inconsistent state in the very next step.
--
-- What do we do about flags?
--
-- Like for packages, we store the flag choices we have already made.
-- Now, regarding (1), we only have to test whether we've decided the
-- current flag before. Regarding (2), the interesting bit is in discovering
-- the new active constraints. To this end, we look up the constraints for
-- the package the flag belongs to, and traverse its flagged dependencies.
-- Wherever we find the flag in question, we start recording dependencies
-- underneath as new active dependencies. If we encounter other flags, we
-- check if we've chosen them already and either proceed or stop.

-- | The state needed during validation.
data ValidateState = VS {
  supportedExt        :: Extension -> Bool,
  supportedLang       :: Language  -> Bool,
  presentPkgs         :: PkgconfigName -> PkgconfigVersionRange  -> Bool,
  index               :: Index,

  -- Saved, scoped, dependencies. Every time 'validate' makes a package choice,
  -- it qualifies the package's dependencies and saves them in this map. Then
  -- the qualified dependencies are available for subsequent flag and stanza
  -- choices for the same package.
  saved               :: Map QPN (FlaggedDeps QPN),

  pa                  :: PreAssignment,

  -- Map from package name to the components that are provided by the chosen
  -- instance of that package, and whether those components are visible and
  -- buildable.
  availableComponents :: Map QPN (Map ExposedComponent ComponentInfo),

  -- Map from package name to the components that are required from that
  -- package.
  requiredComponents  :: Map QPN ComponentDependencyReasons,

  qualifyOptions      :: QualifyOptions
}

newtype Validate a = Validate (Reader ValidateState a)
  deriving (Functor, Applicative, Monad, MonadReader ValidateState)

runValidate :: Validate a -> ValidateState -> a
runValidate (Validate r) = runReader r

-- | A preassignment comprises knowledge about variables, but not
-- necessarily fixed values.
data PreAssignment = PA PPreAssignment FAssignment SAssignment

-- | A (partial) package preassignment. Qualified package names
-- are associated with MergedPkgDeps.
type PPreAssignment = Map QPN MergedPkgDep

-- | A dependency on a component, including its DependencyReason.
data PkgDep = PkgDep (DependencyReason QPN) (PkgComponent QPN) CI

-- | Map from component name to one of the reasons that the component is
-- required.
type ComponentDependencyReasons = Map ExposedComponent (DependencyReason QPN)

-- | MergedPkgDep records constraints about the instances that can still be
-- chosen, and in the extreme case fixes a concrete instance. Otherwise, it is a
-- list of version ranges paired with the goals / variables that introduced
-- them. It also records whether a package is a build-tool dependency, for each
-- reason that it was introduced.
--
-- It is important to store the component name with the version constraint, for
-- error messages, because whether something is a build-tool dependency affects
-- its qualifier, which affects which constraint is applied.
data MergedPkgDep =
    MergedDepFixed ExposedComponent (DependencyReason QPN) I
  | MergedDepConstrained [VROrigin]

-- | Version ranges paired with origins.
type VROrigin = (VR, ExposedComponent, DependencyReason QPN)

-- | The information needed to create a 'Fail' node.
type Conflict = (ConflictSet, FailReason)

validate :: Tree d c -> Validate (Tree d c)
validate = go
  where
    go :: Tree d c -> Validate (Tree d c)

    go (PChoice qpn rdm gr       ts) = PChoice qpn rdm gr <$> W.traverseWithKey (\k -> goP qpn k . go) ts
    go (FChoice qfn rdm gr b m d ts) =
      do
        -- Flag choices may occur repeatedly (because they can introduce new constraints
        -- in various places). However, subsequent choices must be consistent. We thereby
        -- collapse repeated flag choice nodes.
        PA _ pfa _ <- asks pa -- obtain current flag-preassignment
        case M.lookup qfn pfa of
          Just rb -> -- flag has already been assigned; collapse choice to the correct branch
                     case W.lookup rb ts of
                       Just t  -> goF qfn rb (go t)
                       Nothing -> return $ Fail (varToConflictSet (F qfn)) (MalformedFlagChoice qfn)
          Nothing -> -- flag choice is new, follow both branches
                     FChoice qfn rdm gr b m d <$> W.traverseWithKey (\k -> goF qfn k . go) ts
    go (SChoice qsn rdm gr b   ts) =
      do
        -- Optional stanza choices are very similar to flag choices.
        PA _ _ psa <- asks pa -- obtain current stanza-preassignment
        case M.lookup qsn psa of
          Just rb -> -- stanza choice has already been made; collapse choice to the correct branch
                     case W.lookup rb ts of
                       Just t  -> goS qsn rb (go t)
                       Nothing -> return $ Fail (varToConflictSet (S qsn)) (MalformedStanzaChoice qsn)
          Nothing -> -- stanza choice is new, follow both branches
                     SChoice qsn rdm gr b <$> W.traverseWithKey (\k -> goS qsn k . go) ts

    -- We don't need to do anything for goal choices or failure nodes.
    go (GoalChoice rdm           ts) = GoalChoice rdm <$> traverse go ts
    go (Done       rdm s           ) = pure (Done rdm s)
    go (Fail    c fr               ) = pure (Fail c fr)

    -- What to do for package nodes ...
    goP :: QPN -> POption -> Validate (Tree d c) -> Validate (Tree d c)
    goP qpn@(Q _pp pn) (POption i _) r = do
      PA ppa pfa psa <- asks pa    -- obtain current preassignment
      extSupported   <- asks supportedExt  -- obtain the supported extensions
      langSupported  <- asks supportedLang -- obtain the supported languages
      pkgPresent     <- asks presentPkgs -- obtain the present pkg-config pkgs
      idx            <- asks index -- obtain the index
      svd            <- asks saved -- obtain saved dependencies
      aComps         <- asks availableComponents
      rComps         <- asks requiredComponents
      qo             <- asks qualifyOptions
      -- obtain dependencies and index-dictated exclusions introduced by the choice
      let (PInfo deps comps _ mfr) = idx ! pn ! i
      -- qualify the deps in the current scope
      let qdeps = qualifyDeps qo qpn deps
      -- the new active constraints are given by the instance we have chosen,
      -- plus the dependency information we have for that instance
      let newactives = extractAllDeps pfa psa qdeps
      -- We now try to extend the partial assignment with the new active constraints.
      let mnppa = extend extSupported langSupported pkgPresent newactives
                   =<< extendWithPackageChoice (PI qpn i) ppa
      -- In case we continue, we save the scoped dependencies
      let nsvd = M.insert qpn qdeps svd
      case mfr of
        Just fr -> -- The index marks this as an invalid choice. We can stop.
                   return (Fail (varToConflictSet (P qpn)) fr)
        Nothing ->
          let newDeps :: Either Conflict (PPreAssignment, Map QPN ComponentDependencyReasons)
              newDeps = do
                nppa <- mnppa
                rComps' <- extendRequiredComponents qpn aComps rComps newactives
                checkComponentsInNewPackage (M.findWithDefault M.empty qpn rComps) qpn comps
                return (nppa, rComps')
          in case newDeps of
               Left (c, fr)          -> -- We have an inconsistency. We can stop.
                                        return (Fail c fr)
               Right (nppa, rComps') -> -- We have an updated partial assignment for the recursive validation.
                                        local (\ s -> s { pa = PA nppa pfa psa
                                                        , saved = nsvd
                                                        , availableComponents = M.insert qpn comps aComps
                                                        , requiredComponents = rComps'
                                                        }) r

    -- What to do for flag nodes ...
    goF :: QFN -> Bool -> Validate (Tree d c) -> Validate (Tree d c)
    goF qfn@(FN qpn _f) b r = do
      PA ppa pfa psa <- asks pa -- obtain current preassignment
      extSupported   <- asks supportedExt  -- obtain the supported extensions
      langSupported  <- asks supportedLang -- obtain the supported languages
      pkgPresent     <- asks presentPkgs   -- obtain the present pkg-config pkgs
      svd            <- asks saved         -- obtain saved dependencies
      aComps         <- asks availableComponents
      rComps         <- asks requiredComponents
      -- Note that there should be saved dependencies for the package in question,
      -- because while building, we do not choose flags before we see the packages
      -- that define them.
      let qdeps = svd ! qpn
      -- We take the *saved* dependencies, because these have been qualified in the
      -- correct scope.
      --
      -- Extend the flag assignment
      let npfa = M.insert qfn b pfa
      -- We now try to get the new active dependencies we might learn about because
      -- we have chosen a new flag.
      let newactives = extractNewDeps (F qfn) b npfa psa qdeps
          mNewRequiredComps = extendRequiredComponents qpn aComps rComps newactives
      -- As in the package case, we try to extend the partial assignment.
      let mnppa = extend extSupported langSupported pkgPresent newactives ppa
      case liftM2 (,) mnppa mNewRequiredComps of
        Left (c, fr)         -> return (Fail c fr) -- inconsistency found
        Right (nppa, rComps') ->
            local (\ s -> s { pa = PA nppa npfa psa, requiredComponents = rComps' }) r

    -- What to do for stanza nodes (similar to flag nodes) ...
    goS :: QSN -> Bool -> Validate (Tree d c) -> Validate (Tree d c)
    goS qsn@(SN qpn _f) b r = do
      PA ppa pfa psa <- asks pa -- obtain current preassignment
      extSupported   <- asks supportedExt  -- obtain the supported extensions
      langSupported  <- asks supportedLang -- obtain the supported languages
      pkgPresent     <- asks presentPkgs -- obtain the present pkg-config pkgs
      svd            <- asks saved         -- obtain saved dependencies
      aComps         <- asks availableComponents
      rComps         <- asks requiredComponents
      -- Note that there should be saved dependencies for the package in question,
      -- because while building, we do not choose flags before we see the packages
      -- that define them.
      let qdeps = svd ! qpn
      -- We take the *saved* dependencies, because these have been qualified in the
      -- correct scope.
      --
      -- Extend the flag assignment
      let npsa = M.insert qsn b psa
      -- We now try to get the new active dependencies we might learn about because
      -- we have chosen a new flag.
      let newactives = extractNewDeps (S qsn) b pfa npsa qdeps
          mNewRequiredComps = extendRequiredComponents qpn aComps rComps newactives
      -- As in the package case, we try to extend the partial assignment.
      let mnppa = extend extSupported langSupported pkgPresent newactives ppa
      case liftM2 (,) mnppa mNewRequiredComps of
        Left (c, fr)         -> return (Fail c fr) -- inconsistency found
        Right (nppa, rComps') ->
            local (\ s -> s { pa = PA nppa pfa npsa, requiredComponents = rComps' }) r

-- | Check that a newly chosen package instance contains all components that
-- are required from that package so far. The components must also be visible
-- and buildable.
checkComponentsInNewPackage :: ComponentDependencyReasons
                            -> QPN
                            -> Map ExposedComponent ComponentInfo
                            -> Either Conflict ()
checkComponentsInNewPackage required qpn providedComps =
    case M.toList $ deleteKeys (M.keys providedComps) required of
      (missingComp, dr) : _ ->
          Left $ mkConflict missingComp dr NewPackageIsMissingRequiredComponent
      []                    ->
          let failures = lefts
                  [ case () of
                      _ | compIsVisible compInfo == IsVisible False ->
                          Left $ mkConflict comp dr NewPackageHasPrivateRequiredComponent
                        | compIsBuildable compInfo == IsBuildable False ->
                          Left $ mkConflict comp dr NewPackageHasUnbuildableRequiredComponent
                        | otherwise -> Right ()
                  | let merged = M.intersectionWith (,) required providedComps
                  , (comp, (dr, compInfo)) <- M.toList merged ]
          in case failures of
               failure : _ -> Left failure
               []          -> Right ()
  where
    mkConflict :: ExposedComponent
               -> DependencyReason QPN
               -> (ExposedComponent -> DependencyReason QPN -> FailReason)
               -> Conflict
    mkConflict comp dr mkFailure =
        (CS.insert (P qpn) (dependencyReasonToConflictSet dr), mkFailure comp dr)

    deleteKeys :: Ord k => [k] -> Map k v -> Map k v
    deleteKeys ks m = L.foldr M.delete m ks

-- | We try to extract as many concrete dependencies from the given flagged
-- dependencies as possible. We make use of all the flag knowledge we have
-- already acquired.
extractAllDeps :: FAssignment -> SAssignment -> FlaggedDeps QPN -> [LDep QPN]
extractAllDeps fa sa deps = do
  d <- deps
  case d of
    Simple sd _         -> return sd
    Flagged qfn _ td fd -> case M.lookup qfn fa of
                             Nothing    -> mzero
                             Just True  -> extractAllDeps fa sa td
                             Just False -> extractAllDeps fa sa fd
    Stanza qsn td       -> case M.lookup qsn sa of
                             Nothing    -> mzero
                             Just True  -> extractAllDeps fa sa td
                             Just False -> []

-- | We try to find new dependencies that become available due to the given
-- flag or stanza choice. We therefore look for the choice in question, and then call
-- 'extractAllDeps' for everything underneath.
extractNewDeps :: Var QPN -> Bool -> FAssignment -> SAssignment -> FlaggedDeps QPN -> [LDep QPN]
extractNewDeps v b fa sa = go
  where
    go :: FlaggedDeps QPN -> [LDep QPN]
    go deps = do
      d <- deps
      case d of
        Simple _ _           -> mzero
        Flagged qfn' _ td fd
          | v == F qfn'      -> if b then extractAllDeps fa sa td else extractAllDeps fa sa fd
          | otherwise        -> case M.lookup qfn' fa of
                                  Nothing    -> mzero
                                  Just True  -> go td
                                  Just False -> go fd
        Stanza qsn' td
          | v == S qsn'      -> if b then extractAllDeps fa sa td else []
          | otherwise        -> case M.lookup qsn' sa of
                                  Nothing    -> mzero
                                  Just True  -> go td
                                  Just False -> []

-- | Extend a package preassignment.
--
-- Takes the variable that causes the new constraints, a current preassignment
-- and a set of new dependency constraints.
--
-- We're trying to extend the preassignment with each dependency one by one.
-- Each dependency is for a particular variable. We check if we already have
-- constraints for that variable in the current preassignment. If so, we're
-- trying to merge the constraints.
--
-- Either returns a witness of the conflict that would arise during the merge,
-- or the successfully extended assignment.
extend :: (Extension -> Bool)            -- ^ is a given extension supported
       -> (Language  -> Bool)            -- ^ is a given language supported
       -> (PkgconfigName -> PkgconfigVersionRange -> Bool) -- ^ is a given pkg-config requirement satisfiable
       -> [LDep QPN]
       -> PPreAssignment
       -> Either Conflict PPreAssignment
extend extSupported langSupported pkgPresent newactives ppa = foldM extendSingle ppa newactives
  where

    extendSingle :: PPreAssignment -> LDep QPN -> Either Conflict PPreAssignment
    extendSingle a (LDep dr (Ext  ext ))  =
      if extSupported  ext  then Right a
                            else Left (dependencyReasonToConflictSet dr, UnsupportedExtension ext)
    extendSingle a (LDep dr (Lang lang))  =
      if langSupported lang then Right a
                            else Left (dependencyReasonToConflictSet dr, UnsupportedLanguage lang)
    extendSingle a (LDep dr (Pkg pn vr))  =
      if pkgPresent pn vr then Right a
                          else Left (dependencyReasonToConflictSet dr, MissingPkgconfigPackage pn vr)
    extendSingle a (LDep dr (Dep dep@(PkgComponent qpn _) ci)) =
      let mergedDep = M.findWithDefault (MergedDepConstrained []) qpn a
      in  case (\ x -> M.insert qpn x a) <$> merge mergedDep (PkgDep dr dep ci) of
            Left (c, (d, d')) -> Left (c, ConflictingConstraints d d')
            Right x           -> Right x

-- | Extend a package preassignment with a package choice. For example, when
-- the solver chooses foo-2.0, it tries to add the constraint foo==2.0.
--
-- TODO: The new constraint is implemented as a dependency from foo to foo's
-- main library. That isn't correct, because foo might only be needed as a build
-- tool dependency. The implementation may need to change when we support
-- component-based dependency solving.
extendWithPackageChoice :: PI QPN -> PPreAssignment -> Either Conflict PPreAssignment
extendWithPackageChoice (PI qpn i) ppa =
  let mergedDep = M.findWithDefault (MergedDepConstrained []) qpn ppa
      newChoice = PkgDep (DependencyReason qpn M.empty S.empty)
                         (PkgComponent qpn (ExposedLib LMainLibName))
                         (Fixed i)
  in  case (\ x -> M.insert qpn x ppa) <$> merge mergedDep newChoice of
        Left (c, (d, _d')) -> -- Don't include the package choice in the
                              -- FailReason, because it is redundant.
                              Left (c, NewPackageDoesNotMatchExistingConstraint d)
        Right x            -> Right x

-- | Merge constrained instances. We currently adopt a lazy strategy for
-- merging, i.e., we only perform actual checking if one of the two choices
-- is fixed. If the merge fails, we return a conflict set indicating the
-- variables responsible for the failure, as well as the two conflicting
-- fragments.
--
-- Note that while there may be more than one conflicting pair of version
-- ranges, we only return the first we find.
--
-- The ConflictingDeps are returned in order, i.e., the first describes the
-- conflicting part of the MergedPkgDep, and the second describes the PkgDep.
--
-- TODO: Different pairs might have different conflict sets. We're
-- obviously interested to return a conflict that has a "better" conflict
-- set in the sense the it contains variables that allow us to backjump
-- further. We might apply some heuristics here, such as to change the
-- order in which we check the constraints.
merge :: MergedPkgDep -> PkgDep -> Either (ConflictSet, (ConflictingDep, ConflictingDep)) MergedPkgDep
merge (MergedDepFixed comp1 vs1 i1) (PkgDep vs2 (PkgComponent p comp2) ci@(Fixed i2))
  | i1 == i2  = Right $ MergedDepFixed comp1 vs1 i1
  | otherwise =
      Left ( (CS.union `on` dependencyReasonToConflictSet) vs1 vs2
           , ( ConflictingDep vs1 (PkgComponent p comp1) (Fixed i1)
             , ConflictingDep vs2 (PkgComponent p comp2) ci ) )

merge (MergedDepFixed comp1 vs1 i@(I v _)) (PkgDep vs2 (PkgComponent p comp2) ci@(Constrained vr))
  | checkVR vr v = Right $ MergedDepFixed comp1 vs1 i
  | otherwise    =
      Left ( createConflictSetForVersionConflict p v vs1 vr vs2
           , ( ConflictingDep vs1 (PkgComponent p comp1) (Fixed i)
             , ConflictingDep vs2 (PkgComponent p comp2) ci ) )

merge (MergedDepConstrained vrOrigins) (PkgDep vs2 (PkgComponent p comp2) ci@(Fixed i@(I v _))) =
    go vrOrigins -- I tried "reverse vrOrigins" here, but it seems to slow things down ...
  where
    go :: [VROrigin] -> Either (ConflictSet, (ConflictingDep, ConflictingDep)) MergedPkgDep
    go [] = Right (MergedDepFixed comp2 vs2 i)
    go ((vr, comp1, vs1) : vros)
       | checkVR vr v = go vros
       | otherwise    =
           Left ( createConflictSetForVersionConflict p v vs2 vr vs1
                , ( ConflictingDep vs1 (PkgComponent p comp1) (Constrained vr)
                  , ConflictingDep vs2 (PkgComponent p comp2) ci ) )

merge (MergedDepConstrained vrOrigins) (PkgDep vs2 (PkgComponent _ comp2) (Constrained vr)) =
    Right (MergedDepConstrained $

    -- TODO: This line appends the new version range, to preserve the order used
    -- before a refactoring. Consider prepending the version range, if there is
    -- no negative performance impact.
    vrOrigins ++ [(vr, comp2, vs2)])

-- | Creates a conflict set representing a conflict between a version constraint
-- and the fixed version chosen for a package.
createConflictSetForVersionConflict :: QPN
                                    -> Ver
                                    -> DependencyReason QPN
                                    -> VR
                                    -> DependencyReason QPN
                                    -> ConflictSet
createConflictSetForVersionConflict pkg
                                    conflictingVersion
                                    versionDR@(DependencyReason p1 _ _)
                                    conflictingVersionRange
                                    versionRangeDR@(DependencyReason p2 _ _) =
  let hasFlagsOrStanzas (DependencyReason _ fs ss) = not (M.null fs) || not (S.null ss)
  in
    -- The solver currently only optimizes the case where there is a conflict
    -- between the version chosen for a package and a version constraint that
    -- is not under any flags or stanzas. Here is how we check for this case:
    --
    --   (1) Choosing a specific version for a package foo is implemented as
    --       adding a dependency from foo to that version of foo (See
    --       extendWithPackageChoice), so we check that the DependencyReason
    --       contains the current package and no flag or stanza choices.
    --
    --   (2) We check that the DependencyReason for the version constraint also
    --       contains no flag or stanza choices.
    --
    -- When these criteria are not met, we fall back to calling
    -- dependencyReasonToConflictSet.
    if p1 == pkg && not (hasFlagsOrStanzas versionDR) && not (hasFlagsOrStanzas versionRangeDR)
    then let cs1 = dependencyReasonToConflictSetWithVersionConflict
                   p2
                   (CS.OrderedVersionRange conflictingVersionRange)
                   versionDR
             cs2 = dependencyReasonToConflictSetWithVersionConstraintConflict
                   pkg conflictingVersion versionRangeDR
         in cs1 `CS.union` cs2
    else dependencyReasonToConflictSet versionRangeDR `CS.union` dependencyReasonToConflictSet versionDR

-- | Takes a list of new dependencies and uses it to try to update the map of
-- known component dependencies. It returns a failure when a new dependency
-- requires a component that is missing, private, or unbuildable in a previously
-- chosen package.
extendRequiredComponents :: QPN -- ^ package we extend
                         -> Map QPN (Map ExposedComponent ComponentInfo)
                         -> Map QPN ComponentDependencyReasons
                         -> [LDep QPN]
                         -> Either Conflict (Map QPN ComponentDependencyReasons)
extendRequiredComponents eqpn available = foldM extendSingle
  where
    extendSingle :: Map QPN ComponentDependencyReasons
                 -> LDep QPN
                 -> Either Conflict (Map QPN ComponentDependencyReasons)
    extendSingle required (LDep dr (Dep (PkgComponent qpn comp) _)) =
      let compDeps = M.findWithDefault M.empty qpn required
          success = Right $ M.insertWith M.union qpn (M.insert comp dr compDeps) required
      in -- Only check for the existence of the component if its package has
         -- already been chosen.
         case M.lookup qpn available of
           Just comps ->
               case M.lookup comp comps of
                 Nothing ->
                     Left $ mkConflict qpn comp dr PackageRequiresMissingComponent
                 Just compInfo
                   | compIsVisible compInfo == IsVisible False
                   , eqpn /= qpn -- package components can depend on other components
                   ->
                     Left $ mkConflict qpn comp dr PackageRequiresPrivateComponent
                   | compIsBuildable compInfo == IsBuildable False ->
                     Left $ mkConflict qpn comp dr PackageRequiresUnbuildableComponent
                   | otherwise -> success
           Nothing    -> success
    extendSingle required _                                         = Right required

    mkConflict :: QPN
               -> ExposedComponent
               -> DependencyReason QPN
               -> (QPN -> ExposedComponent -> FailReason)
               -> Conflict
    mkConflict qpn comp dr mkFailure =
      (CS.insert (P qpn) (dependencyReasonToConflictSet dr), mkFailure qpn comp)


-- | Interface.
validateTree :: CompilerInfo -> Index -> PkgConfigDb -> Tree d c -> Tree d c
validateTree cinfo idx pkgConfigDb t = runValidate (validate t) VS {
    supportedExt        = maybe (const True) -- if compiler has no list of extensions, we assume everything is supported
                                (\ es -> let s = S.fromList es in \ x -> S.member x s)
                                (compilerInfoExtensions cinfo)
  , supportedLang       = maybe (const True)
                                (flip L.elem) -- use list lookup because language list is small and no Ord instance
                                (compilerInfoLanguages  cinfo)
  , presentPkgs         = pkgConfigPkgIsPresent pkgConfigDb
  , index               = idx
  , saved               = M.empty
  , pa                  = PA M.empty M.empty M.empty
  , availableComponents = M.empty
  , requiredComponents  = M.empty
  , qualifyOptions      = defaultQualifyOptions idx
  }
