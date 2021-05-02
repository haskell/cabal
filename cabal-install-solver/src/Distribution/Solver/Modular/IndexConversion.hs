module Distribution.Solver.Modular.IndexConversion
    ( convPIs
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Distribution.Compat.NonEmptySet as NonEmptySet
import qualified Data.Set as S

import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Compiler
import Distribution.Package                          -- from Cabal
import Distribution.Simple.BuildToolDepends          -- from Cabal
import Distribution.Types.ExeDependency              -- from Cabal
import Distribution.Types.PkgconfigDependency        -- from Cabal
import Distribution.Types.ComponentName              -- from Cabal
import Distribution.Types.CondTree                   -- from Cabal
import Distribution.Types.MungedPackageId            -- from Cabal
import Distribution.Types.MungedPackageName          -- from Cabal
import Distribution.PackageDescription               -- from Cabal
import Distribution.PackageDescription.Configuration
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.System

import           Distribution.Solver.Types.ComponentDeps
                   ( Component(..), componentNameToComponent )
import           Distribution.Solver.Types.Flag
import           Distribution.Solver.Types.LabeledPackageConstraint
import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SourcePackage

import Distribution.Solver.Modular.Dependency as D
import Distribution.Solver.Modular.Flag as F
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version

-- | Convert both the installed package index and the source package
-- index into one uniform solver index.
--
-- We use 'allPackagesBySourcePackageId' for the installed package index
-- because that returns us several instances of the same package and version
-- in order of preference. This allows us in principle to \"shadow\"
-- packages if there are several installed packages of the same version.
-- There are currently some shortcomings in both GHC and Cabal in
-- resolving these situations. However, the right thing to do is to
-- fix the problem there, so for now, shadowing is only activated if
-- explicitly requested.
convPIs :: OS -> Arch -> CompilerInfo -> Map PN [LabeledPackageConstraint]
        -> ShadowPkgs -> StrongFlags -> SolveExecutables
        -> SI.InstalledPackageIndex -> CI.PackageIndex (SourcePackage loc)
        -> Index
convPIs os arch comp constraints sip strfl solveExes iidx sidx =
  mkIndex $
  convIPI' sip iidx ++ convSPI' os arch comp constraints strfl solveExes sidx

-- | Convert a Cabal installed package index to the simpler,
-- more uniform index format of the solver.
convIPI' :: ShadowPkgs -> SI.InstalledPackageIndex -> [(PN, I, PInfo)]
convIPI' (ShadowPkgs sip) idx =
    -- apply shadowing whenever there are multiple installed packages with
    -- the same version
    [ maybeShadow (convIP idx pkg)
    -- IMPORTANT to get internal libraries. See
    -- Note [Index conversion with internal libraries]
    | (_, pkgs) <- SI.allPackagesBySourcePackageIdAndLibName idx
    , (maybeShadow, pkg) <- zip (id : repeat shadow) pkgs ]
  where

    -- shadowing is recorded in the package info
    shadow (pn, i, PInfo fdeps comps fds _)
      | sip = (pn, i, PInfo fdeps comps fds (Just Shadowed))
    shadow x                                     = x

-- | Extract/recover the package ID from an installed package info, and convert it to a solver's I.
convId :: IPI.InstalledPackageInfo -> (PN, I)
convId ipi = (pn, I ver $ Inst $ IPI.installedUnitId ipi)
  where MungedPackageId mpn ver = mungedId ipi
        -- HACK. See Note [Index conversion with internal libraries]
        pn = encodeCompatPackageName mpn

-- | Convert a single installed package into the solver-specific format.
convIP :: SI.InstalledPackageIndex -> IPI.InstalledPackageInfo -> (PN, I, PInfo)
convIP idx ipi =
  case traverse (convIPId (DependencyReason pn M.empty S.empty) comp idx) (IPI.depends ipi) of
        Left u    -> (pn, i, PInfo [] M.empty M.empty (Just (Broken u)))
        Right fds -> (pn, i, PInfo fds components M.empty Nothing)
 where
  -- TODO: Handle sub-libraries and visibility.
  components =
      M.singleton (ExposedLib LMainLibName)
                  ComponentInfo {
                      compIsVisible = IsVisible True
                    , compIsBuildable = IsBuildable True
                    }

  (pn, i) = convId ipi

  -- 'sourceLibName' is unreliable, but for now we only really use this for
  -- primary libs anyways
  comp = componentNameToComponent $ CLibName $ IPI.sourceLibName ipi
-- TODO: Installed packages should also store their encapsulations!

-- Note [Index conversion with internal libraries]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Something very interesting happens when we have internal libraries
-- in our index.  In this case, we maybe have p-0.1, which itself
-- depends on the internal library p-internal ALSO from p-0.1.
-- Here's the danger:
--
--      - If we treat both of these packages as having PN "p",
--        then the solver will try to pick one or the other,
--        but never both.
--
--      - If we drop the internal packages, now p-0.1 has a
--        dangling dependency on an "installed" package we know
--        nothing about. Oops.
--
-- An expedient hack is to put p-internal into cabal-install's
-- index as a MUNGED package name, so that it doesn't conflict
-- with anyone else (except other instances of itself).  But
-- yet, we ought NOT to say that PNs in the solver are munged
-- package names, because they're not; for source packages,
-- we really will never see munged package names.
--
-- The tension here is that the installed package index is actually
-- per library, but the solver is per package.  We need to smooth
-- it over, and munging the package names is a pretty good way to
-- do it.

-- | Convert dependencies specified by an installed package id into
-- flagged dependencies of the solver.
--
-- May return Nothing if the package can't be found in the index. That
-- indicates that the original package having this dependency is broken
-- and should be ignored.
convIPId :: DependencyReason PN -> Component -> SI.InstalledPackageIndex -> UnitId -> Either UnitId (FlaggedDep PN)
convIPId dr comp idx ipid =
  case SI.lookupUnitId idx ipid of
    Nothing  -> Left ipid
    Just ipi -> let (pn, i) = convId ipi
                    name = ExposedLib LMainLibName  -- TODO: Handle sub-libraries.
                in  Right (D.Simple (LDep dr (Dep (PkgComponent pn name) (Fixed i))) comp)
                -- NB: something we pick up from the
                -- InstalledPackageIndex is NEVER an executable

-- | Convert a cabal-install source package index to the simpler,
-- more uniform index format of the solver.
convSPI' :: OS -> Arch -> CompilerInfo -> Map PN [LabeledPackageConstraint]
         -> StrongFlags -> SolveExecutables
         -> CI.PackageIndex (SourcePackage loc) -> [(PN, I, PInfo)]
convSPI' os arch cinfo constraints strfl solveExes =
    L.map (convSP os arch cinfo constraints strfl solveExes) . CI.allPackages

-- | Convert a single source package into the solver-specific format.
convSP :: OS -> Arch -> CompilerInfo -> Map PN [LabeledPackageConstraint]
       -> StrongFlags -> SolveExecutables -> SourcePackage loc -> (PN, I, PInfo)
convSP os arch cinfo constraints strfl solveExes (SourcePackage (PackageIdentifier pn pv) gpd _ _pl) =
  let i = I pv InRepo
      pkgConstraints = fromMaybe [] $ M.lookup pn constraints
  in  (pn, i, convGPD os arch cinfo pkgConstraints strfl solveExes pn gpd)

-- We do not use 'flattenPackageDescription' or 'finalizePD'
-- from 'Distribution.PackageDescription.Configuration' here, because we
-- want to keep the condition tree, but simplify much of the test.

-- | Convert a generic package description to a solver-specific 'PInfo'.
convGPD :: OS -> Arch -> CompilerInfo -> [LabeledPackageConstraint]
        -> StrongFlags -> SolveExecutables -> PN -> GenericPackageDescription
        -> PInfo
convGPD os arch cinfo constraints strfl solveExes pn
        (GenericPackageDescription pkg scannedVersion flags mlib sub_libs flibs exes tests benchs) =
  let
    fds  = flagInfo strfl flags


    conv :: Monoid a => Component -> (a -> BuildInfo) -> DependencyReason PN ->
            CondTree ConfVar [Dependency] a -> FlaggedDeps PN
    conv comp getInfo dr =
        convCondTree M.empty dr pkg os arch cinfo pn fds comp getInfo solveExes .
        addBuildableCondition getInfo

    initDR = DependencyReason pn M.empty S.empty

    flagged_deps
        = concatMap (\ds ->       conv ComponentLib         libBuildInfo        initDR ds) (maybeToList mlib)
       ++ concatMap (\(nm, ds) -> conv (ComponentSubLib nm) libBuildInfo        initDR ds) sub_libs
       ++ concatMap (\(nm, ds) -> conv (ComponentFLib nm)   foreignLibBuildInfo initDR ds) flibs
       ++ concatMap (\(nm, ds) -> conv (ComponentExe nm)    buildInfo           initDR ds) exes
       ++ prefix (Stanza (SN pn TestStanzas))
            (L.map  (\(nm, ds) -> conv (ComponentTest nm)   testBuildInfo (addStanza TestStanzas initDR) ds)
                    tests)
       ++ prefix (Stanza (SN pn BenchStanzas))
            (L.map  (\(nm, ds) -> conv (ComponentBench nm)  benchmarkBuildInfo (addStanza BenchStanzas initDR) ds)
                    benchs)
       ++ maybe []  (convSetupBuildInfo pn) (setupBuildInfo pkg)

    addStanza :: Stanza -> DependencyReason pn -> DependencyReason pn
    addStanza s (DependencyReason pn' fs ss) = DependencyReason pn' fs (S.insert s ss)

    -- | A too-new specVersion is turned into a global 'FailReason'
    -- which prevents the solver from selecting this release (and if
    -- forced to, emit a meaningful solver error message).
    fr = case scannedVersion of
        Just ver -> Just (UnsupportedSpecVer ver)
        Nothing  -> Nothing

    components :: Map ExposedComponent ComponentInfo
    components = M.fromList $ libComps ++ subLibComps ++ exeComps
      where
        libComps = [ (ExposedLib LMainLibName, libToComponentInfo lib)
                   | lib <- maybeToList mlib ]
        subLibComps = [ (ExposedLib (LSubLibName name), libToComponentInfo lib)
                      | (name, lib) <- sub_libs ]
        exeComps = [ ( ExposedExe name
                     , ComponentInfo {
                           compIsVisible = IsVisible True
                         , compIsBuildable = IsBuildable $ testCondition (buildable . buildInfo) exe /= Just False
                         }
                     )
                   | (name, exe) <- exes ]

        libToComponentInfo lib =
            ComponentInfo {
                compIsVisible = IsVisible $ testCondition (isPrivate . libVisibility) lib /= Just True
              , compIsBuildable = IsBuildable $ testCondition (buildable . libBuildInfo) lib /= Just False
              }

        testCondition = testConditionForComponent os arch cinfo constraints

        isPrivate LibraryVisibilityPrivate = True
        isPrivate LibraryVisibilityPublic  = False

  in PInfo flagged_deps components fds fr

-- | Applies the given predicate (for example, testing buildability or
-- visibility) to the given component and environment. Values are combined with
-- AND. This function returns 'Nothing' when the result cannot be determined
-- before dependency solving. Additionally, this function only considers flags
-- that are set by unqualified flag constraints, and it doesn't check the
-- intra-package dependencies of a component.
testConditionForComponent :: OS
                          -> Arch
                          -> CompilerInfo
                          -> [LabeledPackageConstraint]
                          -> (a -> Bool)
                          -> CondTree ConfVar [Dependency] a
                          -> Maybe Bool
testConditionForComponent os arch cinfo constraints p tree =
    case go $ extractCondition p tree of
      Lit True  -> Just True
      Lit False -> Just False
      _         -> Nothing
  where
    flagAssignment :: [(FlagName, Bool)]
    flagAssignment =
        mconcat [ unFlagAssignment fa
                | PackageConstraint (ScopeAnyQualifier _) (PackagePropertyFlags fa)
                    <- L.map unlabelPackageConstraint constraints]

    -- Simplify the condition, using the current environment. Most of this
    -- function was copied from convBranch and
    -- Distribution.Types.Condition.simplifyCondition.
    go :: Condition ConfVar -> Condition ConfVar
    go (Var (OS os')) = Lit (os == os')
    go (Var (Arch arch')) = Lit (arch == arch')
    go (Var (Impl cf cvr))
        | matchImpl (compilerInfoId cinfo) ||
              -- fixme: Nothing should be treated as unknown, rather than empty
              --        list. This code should eventually be changed to either
              --        support partial resolution of compiler flags or to
              --        complain about incompletely configured compilers.
          any matchImpl (fromMaybe [] $ compilerInfoCompat cinfo) = Lit True
        | otherwise = Lit False
      where
        matchImpl (CompilerId cf' cv) = cf == cf' && checkVR cvr cv
    go (Var (PackageFlag f))
        | Just b <- L.lookup f flagAssignment = Lit b
    go (Var v) = Var v
    go (Lit b) = Lit b
    go (CNot c) =
        case go c of
          Lit True -> Lit False
          Lit False -> Lit True
          c' -> CNot c'
    go (COr c d) =
        case (go c, go d) of
          (Lit False, d') -> d'
          (Lit True, _)   -> Lit True
          (c', Lit False) -> c'
          (_, Lit True)   -> Lit True
          (c', d')        -> COr c' d'
    go (CAnd c d) =
        case (go c, go d) of
          (Lit False, _) -> Lit False
          (Lit True, d') -> d'
          (_, Lit False) -> Lit False
          (c', Lit True) -> c'
          (c', d')       -> CAnd c' d'

-- | Create a flagged dependency tree from a list @fds@ of flagged
-- dependencies, using @f@ to form the tree node (@f@ will be
-- something like @Stanza sn@).
prefix :: (FlaggedDeps qpn -> FlaggedDep qpn)
       -> [FlaggedDeps qpn] -> FlaggedDeps qpn
prefix _ []  = []
prefix f fds = [f (concat fds)]

-- | Convert flag information. Automatic flags are now considered weak
-- unless strong flags have been selected explicitly.
flagInfo :: StrongFlags -> [PackageFlag] -> FlagInfo
flagInfo (StrongFlags strfl) =
    M.fromList . L.map (\ (MkPackageFlag fn _ b m) -> (fn, FInfo b (flagType m) (weak m)))
  where
    weak m = WeakOrTrivial $ not (strfl || m)
    flagType m = if m then Manual else Automatic

-- | Convert condition trees to flagged dependencies.  Mutually
-- recursive with 'convBranch'.  See 'convBranch' for an explanation
-- of all arguments preceding the input 'CondTree'.
convCondTree :: Map FlagName Bool -> DependencyReason PN -> PackageDescription -> OS -> Arch -> CompilerInfo -> PN -> FlagInfo ->
                Component ->
                (a -> BuildInfo) ->
                SolveExecutables ->
                CondTree ConfVar [Dependency] a -> FlaggedDeps PN
convCondTree flags dr pkg os arch cinfo pn fds comp getInfo solveExes@(SolveExecutables solveExes') (CondNode info ds branches) =
             -- Merge all library and build-tool dependencies at every level in
             -- the tree of flagged dependencies. Otherwise 'extractCommon'
             -- could create duplicate dependencies, and the number of
             -- duplicates could grow exponentially from the leaves to the root
             -- of the tree.
             mergeSimpleDeps $
                 [ D.Simple singleDep comp
                 | dep <- ds
                 , singleDep <- convLibDeps dr dep ]  -- unconditional package dependencies

              ++ L.map (\e -> D.Simple (LDep dr (Ext  e)) comp) (allExtensions bi) -- unconditional extension dependencies
              ++ L.map (\l -> D.Simple (LDep dr (Lang l)) comp) (allLanguages  bi) -- unconditional language dependencies
              ++ L.map (\(PkgconfigDependency pkn vr) -> D.Simple (LDep dr (Pkg pkn vr)) comp) (pkgconfigDepends bi) -- unconditional pkg-config dependencies
              ++ concatMap (convBranch flags dr pkg os arch cinfo pn fds comp getInfo solveExes) branches
              -- build-tools dependencies
              -- NB: Only include these dependencies if SolveExecutables
              -- is True.  It might be false in the legacy solver
              -- codepath, in which case there won't be any record of
              -- an executable we need.
              ++ [ D.Simple (convExeDep dr exeDep) comp
                 | solveExes'
                 , exeDep <- getAllToolDependencies pkg bi
                 , not $ isInternal pkg exeDep
                 ]
  where
    bi = getInfo info

data SimpleFlaggedDepKey qpn =
    SimpleFlaggedDepKey (PkgComponent qpn) Component
  deriving (Eq, Ord)

data SimpleFlaggedDepValue qpn = SimpleFlaggedDepValue (DependencyReason qpn) VR

-- | Merge 'Simple' dependencies that apply to the same library or build-tool.
-- This function should be able to merge any two dependencies that can be merged
-- by extractCommon, in order to prevent the exponential growth of dependencies.
--
-- Note that this function can merge dependencies that have different
-- DependencyReasons, which can make the DependencyReasons less precise. This
-- loss of precision only affects performance and log messages, not correctness.
-- However, when 'mergeSimpleDeps' is only called on dependencies at a single
-- location in the dependency tree, the only difference between
-- DependencyReasons should be flags that have value FlagBoth. Adding extra
-- flags with value FlagBoth should not affect performance, since they are not
-- added to the conflict set. The only downside is the possibility of the log
-- incorrectly saying that the flag contributed to excluding a specific version
-- of a dependency. For example, if +/-flagA introduces pkg >=2 and +/-flagB
-- introduces pkg <5, the merged dependency would mean that
-- +/-flagA and +/-flagB introduce pkg >=2 && <5, which would incorrectly imply
-- that +/-flagA excludes pkg-6.
mergeSimpleDeps :: Ord qpn => FlaggedDeps qpn -> FlaggedDeps qpn
mergeSimpleDeps deps = L.map (uncurry toFlaggedDep) (M.toList merged) ++ unmerged
  where
    (merged, unmerged) = L.foldl' f (M.empty, []) deps
      where
        f :: Ord qpn
          => (Map (SimpleFlaggedDepKey qpn) (SimpleFlaggedDepValue qpn), FlaggedDeps qpn)
          -> FlaggedDep qpn
          -> (Map (SimpleFlaggedDepKey qpn) (SimpleFlaggedDepValue qpn), FlaggedDeps qpn)
        f (merged', unmerged') (D.Simple (LDep dr (Dep dep (Constrained vr))) comp) =
            ( M.insertWith mergeValues
                           (SimpleFlaggedDepKey dep comp)
                           (SimpleFlaggedDepValue dr vr)
                           merged'
            , unmerged')
        f (merged', unmerged') unmergeableDep = (merged', unmergeableDep : unmerged')

        mergeValues :: SimpleFlaggedDepValue qpn
                    -> SimpleFlaggedDepValue qpn
                    -> SimpleFlaggedDepValue qpn
        mergeValues (SimpleFlaggedDepValue dr1 vr1) (SimpleFlaggedDepValue dr2 vr2) =
            SimpleFlaggedDepValue (unionDRs dr1 dr2) (vr1 .&&. vr2)

    toFlaggedDep :: SimpleFlaggedDepKey qpn
                 -> SimpleFlaggedDepValue qpn
                 -> FlaggedDep qpn
    toFlaggedDep (SimpleFlaggedDepKey dep comp) (SimpleFlaggedDepValue dr vr) =
        D.Simple (LDep dr (Dep dep (Constrained vr))) comp

-- | Branch interpreter.  Mutually recursive with 'convCondTree'.
--
-- Here, we try to simplify one of Cabal's condition tree branches into the
-- solver's flagged dependency format, which is weaker. Condition trees can
-- contain complex logical expression composed from flag choices and special
-- flags (such as architecture, or compiler flavour). We try to evaluate the
-- special flags and subsequently simplify to a tree that only depends on
-- simple flag choices.
--
-- This function takes a number of arguments:
--
--      1. A map of flag values that have already been chosen. It allows
--         convBranch to avoid creating nested FlaggedDeps that are
--         controlled by the same flag and avoid creating DependencyReasons with
--         conflicting values for the same flag.
--
--      2. The DependencyReason calculated at this point in the tree of
--         conditionals. The flag values in the DependencyReason are similar to
--         the values in the map above, except for the use of FlagBoth.
--
--      3. Some pre dependency-solving known information ('OS', 'Arch',
--         'CompilerInfo') for @os()@, @arch()@ and @impl()@ variables,
--
--      4. The package name @'PN'@ which this condition tree
--         came from, so that we can correctly associate @flag()@
--         variables with the correct package name qualifier,
--
--      5. The flag defaults 'FlagInfo' so that we can populate
--         'Flagged' dependencies with 'FInfo',
--
--      6. The name of the component 'Component' so we can record where
--         the fine-grained information about where the component came
--         from (see 'convCondTree'), and
--
--      7. A selector to extract the 'BuildInfo' from the leaves of
--         the 'CondTree' (which actually contains the needed
--         dependency information.)
--
--      8. The set of package names which should be considered internal
--         dependencies, and thus not handled as dependencies.
convBranch :: Map FlagName Bool
           -> DependencyReason PN
           -> PackageDescription
           -> OS
           -> Arch
           -> CompilerInfo
           -> PN
           -> FlagInfo
           -> Component
           -> (a -> BuildInfo)
           -> SolveExecutables
           -> CondBranch ConfVar [Dependency] a
           -> FlaggedDeps PN
convBranch flags dr pkg os arch cinfo pn fds comp getInfo solveExes (CondBranch c' t' mf') =
    go c'
       (\flags' dr' ->           convCondTree flags' dr' pkg os arch cinfo pn fds comp getInfo solveExes  t')
       (\flags' dr' -> maybe [] (convCondTree flags' dr' pkg os arch cinfo pn fds comp getInfo solveExes) mf')
       flags dr
  where
    go :: Condition ConfVar
       -> (Map FlagName Bool -> DependencyReason PN -> FlaggedDeps PN)
       -> (Map FlagName Bool -> DependencyReason PN -> FlaggedDeps PN)
       ->  Map FlagName Bool -> DependencyReason PN -> FlaggedDeps PN
    go (Lit True)  t _ = t
    go (Lit False) _ f = f
    go (CNot c)    t f = go c f t
    go (CAnd c d)  t f = go c (go d t f) f
    go (COr  c d)  t f = go c t (go d t f)
    go (Var (PackageFlag fn)) t f = \flags' ->
        case M.lookup fn flags' of
          Just True  -> t flags'
          Just False -> f flags'
          Nothing    -> \dr' ->
            -- Add each flag to the DependencyReason for all dependencies below,
            -- including any extracted dependencies. Extracted dependencies are
            -- introduced by both flag values (FlagBoth). Note that we don't
            -- actually need to add the flag to the extracted dependencies for
            -- correct backjumping; the information only improves log messages
            -- by giving the user the full reason for each dependency.
            let addFlagValue v = addFlagToDependencyReason fn v dr'
                addFlag v = M.insert fn v flags'
            in extractCommon (t (addFlag True)  (addFlagValue FlagBoth))
                             (f (addFlag False) (addFlagValue FlagBoth))
                ++ [ Flagged (FN pn fn) (fds M.! fn) (t (addFlag True)  (addFlagValue FlagTrue))
                                                     (f (addFlag False) (addFlagValue FlagFalse)) ]
    go (Var (OS os')) t f
      | os == os'      = t
      | otherwise      = f
    go (Var (Arch arch')) t f
      | arch == arch'  = t
      | otherwise      = f
    go (Var (Impl cf cvr)) t f
      | matchImpl (compilerInfoId cinfo) ||
            -- fixme: Nothing should be treated as unknown, rather than empty
            --        list. This code should eventually be changed to either
            --        support partial resolution of compiler flags or to
            --        complain about incompletely configured compilers.
        any matchImpl (fromMaybe [] $ compilerInfoCompat cinfo) = t
      | otherwise      = f
      where
        matchImpl (CompilerId cf' cv) = cf == cf' && checkVR cvr cv

    addFlagToDependencyReason :: FlagName -> FlagValue -> DependencyReason pn -> DependencyReason pn
    addFlagToDependencyReason fn v (DependencyReason pn' fs ss) =
        DependencyReason pn' (M.insert fn v fs) ss

    -- If both branches contain the same package as a simple dep, we lift it to
    -- the next higher-level, but with the union of version ranges. This
    -- heuristic together with deferring flag choices will then usually first
    -- resolve this package, and try an already installed version before imposing
    -- a default flag choice that might not be what we want.
    --
    -- Note that we make assumptions here on the form of the dependencies that
    -- can occur at this point. In particular, no occurrences of Fixed, as all
    -- dependencies below this point have been generated using 'convLibDep'.
    --
    -- WARNING: This is quadratic!
    extractCommon :: Eq pn => FlaggedDeps pn -> FlaggedDeps pn -> FlaggedDeps pn
    extractCommon ps ps' =
        -- Union the DependencyReasons, because the extracted dependency can be
        -- avoided by removing the dependency from either side of the
        -- conditional.
        [ D.Simple (LDep (unionDRs vs1 vs2) (Dep dep1 (Constrained $ vr1 .||. vr2))) comp
        | D.Simple (LDep vs1                (Dep dep1 (Constrained vr1))) _ <- ps
        , D.Simple (LDep vs2                (Dep dep2 (Constrained vr2))) _ <- ps'
        , dep1 == dep2
        ]

-- | Merge DependencyReasons by unioning their variables.
unionDRs :: DependencyReason pn -> DependencyReason pn -> DependencyReason pn
unionDRs (DependencyReason pn' fs1 ss1) (DependencyReason _ fs2 ss2) =
    DependencyReason pn' (M.union fs1 fs2) (S.union ss1 ss2)

-- | Convert a Cabal dependency on a set of library components (from a single
-- package) to solver-specific dependencies.
convLibDeps :: DependencyReason PN -> Dependency -> [LDep PN]
convLibDeps dr (Dependency pn vr libs) =
    [ LDep dr $ Dep (PkgComponent pn (ExposedLib lib)) (Constrained vr)
    | lib <- NonEmptySet.toList libs ]

-- | Convert a Cabal dependency on an executable (build-tools) to a solver-specific dependency.
convExeDep :: DependencyReason PN -> ExeDependency -> LDep PN
convExeDep dr (ExeDependency pn exe vr) = LDep dr $ Dep (PkgComponent pn (ExposedExe exe)) (Constrained vr)

-- | Convert setup dependencies
convSetupBuildInfo :: PN -> SetupBuildInfo -> FlaggedDeps PN
convSetupBuildInfo pn nfo =
    [ D.Simple singleDep ComponentSetup
    | dep <- setupDepends nfo
    , singleDep <- convLibDeps (DependencyReason pn M.empty S.empty) dep ]
