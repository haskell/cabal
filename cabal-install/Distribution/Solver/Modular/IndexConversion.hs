module Distribution.Solver.Modular.IndexConversion
    ( convPIs
    ) where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Monoid as Mon
import Data.Set as S
import Prelude hiding (pi)

import Distribution.Compiler
import Distribution.InstalledPackageInfo as IPI
import Distribution.Package                          -- from Cabal
import Distribution.Simple.BuildToolDepends          -- from Cabal
import Distribution.Types.Dependency                 -- from Cabal
import Distribution.Types.ExeDependency              -- from Cabal
import Distribution.Types.PkgconfigDependency        -- from Cabal
import Distribution.Types.UnqualComponentName        -- from Cabal
import Distribution.Types.CondTree                   -- from Cabal
import Distribution.PackageDescription as PD         -- from Cabal
import Distribution.PackageDescription.Configuration as PDC
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.System
import Distribution.Types.ForeignLib

import           Distribution.Solver.Types.ComponentDeps (Component(..))
import           Distribution.Solver.Types.OptionalStanza
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
convPIs :: OS -> Arch -> CompilerInfo -> ShadowPkgs -> StrongFlags -> SolveExecutables ->
           SI.InstalledPackageIndex -> CI.PackageIndex (SourcePackage loc) -> Index
convPIs os arch comp sip strfl sexes iidx sidx =
  mkIndex (convIPI' sip iidx ++ convSPI' os arch comp strfl sexes sidx)

-- | Convert a Cabal installed package index to the simpler,
-- more uniform index format of the solver.
convIPI' :: ShadowPkgs -> SI.InstalledPackageIndex -> [(PN, I, PInfo)]
convIPI' (ShadowPkgs sip) idx =
    -- apply shadowing whenever there are multiple installed packages with
    -- the same version
    [ maybeShadow (convIP idx pkg)
    | (_pkgid, pkgs) <- SI.allPackagesBySourcePackageId idx
    , (maybeShadow, pkg) <- zip (id : repeat shadow) pkgs ]
  where

    -- shadowing is recorded in the package info
    shadow (pn, i, PInfo fdeps fds _) | sip = (pn, i, PInfo fdeps fds (Just Shadowed))
    shadow x                                = x

-- | Convert a single installed package into the solver-specific format.
convIP :: SI.InstalledPackageIndex -> InstalledPackageInfo -> (PN, I, PInfo)
convIP idx ipi =
  case mapM (convIPId pn idx) (IPI.depends ipi) of
        Nothing  -> (pn, i, PInfo []            M.empty (Just Broken))
        Just fds -> (pn, i, PInfo (setComp fds) M.empty Nothing)
 where
  -- We assume that all dependencies of installed packages are _library_ deps
  ipid = IPI.installedUnitId ipi
  i = I (pkgVersion (sourcePackageId ipi)) (Inst ipid)
  pn = pkgName (sourcePackageId ipi)
  setComp = setCompFlaggedDeps ComponentLib
-- TODO: Installed packages should also store their encapsulations!

-- | Convert dependencies specified by an installed package id into
-- flagged dependencies of the solver.
--
-- May return Nothing if the package can't be found in the index. That
-- indicates that the original package having this dependency is broken
-- and should be ignored.
convIPId :: PN -> SI.InstalledPackageIndex -> UnitId -> Maybe (FlaggedDep () PN)
convIPId pn' idx ipid =
  case SI.lookupUnitId idx ipid of
    Nothing  -> Nothing
    Just ipi -> let i = I (pkgVersion (sourcePackageId ipi)) (Inst ipid)
                    pn = pkgName (sourcePackageId ipi)
                in  Just (D.Simple (Dep False pn (Fixed i (P pn'))) ())
                -- NB: something we pick up from the
                -- InstalledPackageIndex is NEVER an executable

-- | Convert a cabal-install source package index to the simpler,
-- more uniform index format of the solver.
convSPI' :: OS -> Arch -> CompilerInfo -> StrongFlags -> SolveExecutables ->
            CI.PackageIndex (SourcePackage loc) -> [(PN, I, PInfo)]
convSPI' os arch cinfo strfl sexes = L.map (convSP os arch cinfo strfl sexes) . CI.allPackages

-- | Convert a single source package into the solver-specific format.
convSP :: OS -> Arch -> CompilerInfo -> StrongFlags -> SolveExecutables -> SourcePackage loc -> (PN, I, PInfo)
convSP os arch cinfo strfl sexes (SourcePackage (PackageIdentifier pn pv) gpd _ _pl) =
  let i = I pv InRepo
  in  (pn, i, convGPD os arch cinfo strfl sexes (PI pn i) gpd)

-- We do not use 'flattenPackageDescription' or 'finalizePD'
-- from 'Distribution.PackageDescription.Configuration' here, because we
-- want to keep the condition tree, but simplify much of the test.

-- | Convert a generic package description to a solver-specific 'PInfo'.
convGPD :: OS -> Arch -> CompilerInfo -> StrongFlags -> SolveExecutables ->
           PI PN -> GenericPackageDescription -> PInfo
convGPD os arch cinfo strfl sexes pi
        (GenericPackageDescription pkg flags mlib sub_libs flibs exes tests benchs) =
  let
    fds  = flagInfo strfl flags

    -- | We have to be careful to filter out dependencies on
    -- internal libraries, since they don't refer to real packages
    -- and thus cannot actually be solved over.  We'll do this
    -- by creating a set of package names which are "internal"
    -- and dropping them as we convert.

    ipns = S.fromList $ [ unqualComponentNameToPackageName nm
                        | (nm, _) <- sub_libs ]

    conv :: Mon.Monoid a => Component -> (a -> BuildInfo) ->
            CondTree ConfVar [Dependency] a -> FlaggedDeps Component PN
    conv comp getInfo = convCondTree pkg os arch cinfo pi fds comp getInfo ipns sexes .
                        PDC.addBuildableCondition getInfo

    flagged_deps
        = concatMap (\ds -> conv ComponentLib libBuildInfo ds) (maybeToList mlib)
       ++ concatMap (\(nm, ds) -> conv (ComponentSubLib nm)   libBuildInfo       ds) sub_libs
       ++ concatMap (\(nm, ds) -> conv (ComponentFLib nm)  foreignLibBuildInfo ds) flibs
       ++ concatMap (\(nm, ds) -> conv (ComponentExe nm)   buildInfo          ds) exes
       ++ prefix (Stanza (SN pi TestStanzas))
            (L.map  (\(nm, ds) -> conv (ComponentTest nm)  testBuildInfo      ds) tests)
       ++ prefix (Stanza (SN pi BenchStanzas))
            (L.map  (\(nm, ds) -> conv (ComponentBench nm) benchmarkBuildInfo ds) benchs)
       ++ maybe []    (convSetupBuildInfo pi)    (setupBuildInfo pkg)

  in
    PInfo flagged_deps fds Nothing

-- | Create a flagged dependency tree from a list @fds@ of flagged
-- dependencies, using @f@ to form the tree node (@f@ will be
-- something like @Stanza sn@).
prefix :: (FlaggedDeps comp qpn -> FlaggedDep comp' qpn)
       -> [FlaggedDeps comp qpn] -> FlaggedDeps comp' qpn
prefix _ []  = []
prefix f fds = [f (concat fds)]

-- | Convert flag information. Automatic flags are now considered weak
-- unless strong flags have been selected explicitly.
flagInfo :: StrongFlags -> [PD.Flag] -> FlagInfo
flagInfo (StrongFlags strfl) =
    M.fromList . L.map (\ (MkFlag fn _ b m) -> (fn, FInfo b m (weak m)))
  where
    weak m = WeakOrTrivial $ not (strfl || m)

-- | Internal package names, which should not be interpreted as true
-- dependencies.
type IPNs = Set PN

-- | Convenience function to delete a 'FlaggedDep' if it's
-- for a 'PN' that isn't actually real.
filterIPNs :: IPNs -> Dependency -> FlaggedDep Component PN -> FlaggedDeps Component PN
filterIPNs ipns (Dependency pn _) fd
    | S.notMember pn ipns = [fd]
    | otherwise           = []

-- | Convert condition trees to flagged dependencies.  Mutually
-- recursive with 'convBranch'.  See 'convBranch' for an explanation
-- of all arguments preceeding the input 'CondTree'.
convCondTree :: PackageDescription -> OS -> Arch -> CompilerInfo -> PI PN -> FlagInfo ->
                Component ->
                (a -> BuildInfo) ->
                IPNs ->
                SolveExecutables ->
                CondTree ConfVar [Dependency] a -> FlaggedDeps Component PN
convCondTree pkg os arch cinfo pi@(PI pn _) fds comp getInfo ipns sexes@(SolveExecutables sexes') (CondNode info ds branches) =
                 concatMap
                    (\d -> filterIPNs ipns d (D.Simple (convLibDep pn d) comp))
                    ds  -- unconditional package dependencies
              ++ L.map (\e -> D.Simple (Ext  e) comp) (PD.allExtensions bi) -- unconditional extension dependencies
              ++ L.map (\l -> D.Simple (Lang l) comp) (PD.allLanguages  bi) -- unconditional language dependencies
              ++ L.map (\(PkgconfigDependency pkn vr) -> D.Simple (Pkg pkn vr) comp) (PD.pkgconfigDepends bi) -- unconditional pkg-config dependencies
              ++ concatMap (convBranch pkg os arch cinfo pi fds comp getInfo ipns sexes) branches
              -- build-tools dependencies
              -- NB: Only include these dependencies if SolveExecutables
              -- is True.  It might be false in the legacy solver
              -- codepath, in which case there won't be any record of
              -- an executable we need.
              ++ [ D.Simple (convExeDep pn exeDep) comp
                 | sexes'
                 , exeDep <- getAllToolDependencies pkg bi
                 , not $ isInternal pkg exeDep
                 ]
  where
    bi = getInfo info

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
--      1. Some pre dependency-solving known information ('OS', 'Arch',
--         'CompilerInfo') for @os()@, @arch()@ and @impl()@ variables,
--
--      2. The package instance @'PI' 'PN'@ which this condition tree
--         came from, so that we can correctly associate @flag()@
--         variables with the correct package name qualifier,
--
--      3. The flag defaults 'FlagInfo' so that we can populate
--         'Flagged' dependencies with 'FInfo',
--
--      4. The name of the component 'Component' so we can record where
--         the fine-grained information about where the component came
--         from (see 'convCondTree'), and
--
--      5. A selector to extract the 'BuildInfo' from the leaves of
--         the 'CondTree' (which actually contains the needed
--         dependency information.)
--
--      6. The set of package names which should be considered internal
--         dependencies, and thus not handled as dependencies.
convBranch :: PackageDescription -> OS -> Arch -> CompilerInfo ->
              PI PN -> FlagInfo ->
              Component ->
              (a -> BuildInfo) ->
              IPNs ->
              SolveExecutables ->
              CondBranch ConfVar [Dependency] a ->
              FlaggedDeps Component PN
convBranch pkg os arch cinfo pi@(PI pn _) fds comp getInfo ipns sexes (CondBranch c' t' mf') =
  go c' (          convCondTree pkg os arch cinfo pi fds comp getInfo ipns sexes  t')
        (maybe [] (convCondTree pkg os arch cinfo pi fds comp getInfo ipns sexes) mf')
  where
    go :: Condition ConfVar ->
          FlaggedDeps Component PN -> FlaggedDeps Component PN -> FlaggedDeps Component PN
    go (Lit True)  t _ = t
    go (Lit False) _ f = f
    go (CNot c)    t f = go c f t
    go (CAnd c d)  t f = go c (go d t f) f
    go (COr  c d)  t f = go c t (go d t f)
    go (Var (Flag fn)) t f = extractCommon t f ++ [Flagged (FN pi fn) (fds ! fn) t f]
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

    -- If both branches contain the same package as a simple dep, we lift it to
    -- the next higher-level, but without constraints. This heuristic together
    -- with deferring flag choices will then usually first resolve this package,
    -- and try an already installed version before imposing a default flag choice
    -- that might not be what we want.
    --
    -- Note that we make assumptions here on the form of the dependencies that
    -- can occur at this point. In particular, no occurrences of Fixed, and no
    -- occurrences of multiple version ranges, as all dependencies below this
    -- point have been generated using 'convLibDep'.
    --
    -- WARNING: This is quadratic!
    extractCommon :: FlaggedDeps Component PN -> FlaggedDeps Component PN -> FlaggedDeps Component PN
    extractCommon ps ps' = [ D.Simple (Dep is_exe1 pn1 (Constrained [(vr1 .||. vr2, P pn)])) comp
                           | D.Simple (Dep is_exe1 pn1 (Constrained [(vr1, _)])) _ <- ps
                           , D.Simple (Dep is_exe2 pn2 (Constrained [(vr2, _)])) _ <- ps'
                           , pn1 == pn2
                           , is_exe1 == is_exe2
                           ]

-- | Convert a Cabal dependency on a library to a solver-specific dependency.
convLibDep :: PN -> Dependency -> Dep PN
convLibDep pn' (Dependency pn vr) = Dep False {- not exe -} pn (Constrained [(vr, P pn')])

-- | Convert a Cabal dependency on a executable (build-tools) to a solver-specific dependency.
-- TODO do something about the name of the exe component itself
convExeDep :: PN -> ExeDependency -> Dep PN
convExeDep pn' (ExeDependency pn _ vr) = Dep True pn (Constrained [(vr, P pn')])

-- | Convert setup dependencies
convSetupBuildInfo :: PI PN -> SetupBuildInfo -> FlaggedDeps Component PN
convSetupBuildInfo (PI pn _i) nfo =
    L.map (\d -> D.Simple (convLibDep pn d) ComponentSetup) (PD.setupDepends nfo)
