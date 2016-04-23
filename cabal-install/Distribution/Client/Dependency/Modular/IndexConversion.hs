module Distribution.Client.Dependency.Modular.IndexConversion
    ( convPIs
    ) where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Monoid as Mon
import Prelude hiding (pi)

import qualified Distribution.Client.PackageIndex as CI
import Distribution.Client.Types
import Distribution.Client.ComponentDeps (Component(..))
import Distribution.Compiler
import Distribution.InstalledPackageInfo as IPI
import Distribution.Package                          -- from Cabal
import Distribution.PackageDescription as PD         -- from Cabal
import Distribution.PackageDescription.Configuration as PDC
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.System

import Distribution.Client.Dependency.Modular.Dependency as D
import Distribution.Client.Dependency.Modular.Flag as F
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.Version

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
convPIs :: OS -> Arch -> CompilerInfo -> Bool -> Bool ->
           SI.InstalledPackageIndex -> CI.PackageIndex SourcePackage -> Index
convPIs os arch comp sip strfl iidx sidx =
  mkIndex (convIPI' sip iidx ++ convSPI' os arch comp strfl sidx)

-- | Convert a Cabal installed package index to the simpler,
-- more uniform index format of the solver.
convIPI' :: Bool -> SI.InstalledPackageIndex -> [(PN, I, PInfo)]
convIPI' sip idx =
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
  let ipid = IPI.installedUnitId ipi
      i = I (pkgVersion (sourcePackageId ipi)) (Inst ipid)
      pn = pkgName (sourcePackageId ipi)
  in  case mapM (convIPId pn idx) (IPI.depends ipi) of
        Nothing  -> (pn, i, PInfo []            M.empty (Just Broken))
        Just fds -> (pn, i, PInfo (setComp fds) M.empty Nothing)
 where
  -- We assume that all dependencies of installed packages are _library_ deps
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
                in  Just (D.Simple (Dep pn (Fixed i (P pn'))) ())

-- | Convert a cabal-install source package index to the simpler,
-- more uniform index format of the solver.
convSPI' :: OS -> Arch -> CompilerInfo -> Bool ->
            CI.PackageIndex SourcePackage -> [(PN, I, PInfo)]
convSPI' os arch cinfo strfl = L.map (convSP os arch cinfo strfl) . CI.allPackages

-- | Convert a single source package into the solver-specific format.
convSP :: OS -> Arch -> CompilerInfo -> Bool -> SourcePackage -> (PN, I, PInfo)
convSP os arch cinfo strfl (SourcePackage (PackageIdentifier pn pv) gpd _ _pl) =
  let i = I pv InRepo
  in  (pn, i, convGPD os arch cinfo strfl (PI pn i) gpd)

-- We do not use 'flattenPackageDescription' or 'finalizePackageDescription'
-- from 'Distribution.PackageDescription.Configuration' here, because we
-- want to keep the condition tree, but simplify much of the test.

-- | Convert a generic package description to a solver-specific 'PInfo'.
convGPD :: OS -> Arch -> CompilerInfo -> Bool ->
           PI PN -> GenericPackageDescription -> PInfo
convGPD os arch cinfo strfl pi
        (GenericPackageDescription pkg flags libs exes tests benchs) =
  let
    fds  = flagInfo strfl flags

    conv :: Mon.Monoid a => Component -> (a -> BuildInfo) ->
            CondTree ConfVar [Dependency] a -> FlaggedDeps Component PN
    conv comp getInfo = convCondTree os arch cinfo pi fds comp getInfo .
                        PDC.addBuildableCondition getInfo
  in
    PInfo
      (maybe []    (conv ComponentLib                     libBuildInfo         ) libs    ++
       maybe []    (convSetupBuildInfo pi)    (setupBuildInfo pkg)    ++
       concatMap   (\(nm, ds) -> conv (ComponentExe nm)   buildInfo          ds) exes    ++
      prefix (Stanza (SN pi TestStanzas))
        (L.map     (\(nm, ds) -> conv (ComponentTest nm)  testBuildInfo      ds) tests)  ++
      prefix (Stanza (SN pi BenchStanzas))
        (L.map     (\(nm, ds) -> conv (ComponentBench nm) benchmarkBuildInfo ds) benchs))
      fds
      Nothing

prefix :: (FlaggedDeps comp qpn -> FlaggedDep comp' qpn) -> [FlaggedDeps comp qpn] -> FlaggedDeps comp' qpn
prefix _ []  = []
prefix f fds = [f (concat fds)]

-- | Convert flag information. Automatic flags are now considered weak
-- unless strong flags have been selected explicitly.
flagInfo :: Bool -> [PD.Flag] -> FlagInfo
flagInfo strfl = M.fromList . L.map (\ (MkFlag fn _ b m) -> (fn, FInfo b m (not (strfl || m))))

-- | Convert condition trees to flagged dependencies.
convCondTree :: OS -> Arch -> CompilerInfo -> PI PN -> FlagInfo ->
                Component ->
                (a -> BuildInfo) ->
                CondTree ConfVar [Dependency] a -> FlaggedDeps Component PN
convCondTree os arch cinfo pi@(PI pn _) fds comp getInfo (CondNode info ds branches) =
                 L.map (\d -> D.Simple (convDep pn d) comp) ds  -- unconditional package dependencies
              ++ L.map (\e -> D.Simple (Ext  e) comp) (PD.allExtensions bi) -- unconditional extension dependencies
              ++ L.map (\l -> D.Simple (Lang l) comp) (PD.allLanguages  bi) -- unconditional language dependencies
              ++ L.map (\(Dependency pkn vr) -> D.Simple (Pkg pkn vr) comp) (PD.pkgconfigDepends bi) -- unconditional pkg-config dependencies
              ++ concatMap (convBranch os arch cinfo pi fds comp getInfo) branches
  where
    bi = getInfo info

-- | Branch interpreter.
--
-- Here, we try to simplify one of Cabal's condition tree branches into the
-- solver's flagged dependency format, which is weaker. Condition trees can
-- contain complex logical expression composed from flag choices and special
-- flags (such as architecture, or compiler flavour). We try to evaluate the
-- special flags and subsequently simplify to a tree that only depends on
-- simple flag choices.
convBranch :: OS -> Arch -> CompilerInfo ->
              PI PN -> FlagInfo ->
              Component ->
              (a -> BuildInfo) ->
              (Condition ConfVar,
               CondTree ConfVar [Dependency] a,
               Maybe (CondTree ConfVar [Dependency] a)) -> FlaggedDeps Component PN
convBranch os arch cinfo pi@(PI pn _) fds comp getInfo (c', t', mf') =
  go c' (          convCondTree os arch cinfo pi fds comp getInfo   t')
        (maybe [] (convCondTree os arch cinfo pi fds comp getInfo) mf')
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
    -- point have been generated using 'convDep'.
    extractCommon :: FlaggedDeps Component PN -> FlaggedDeps Component PN -> FlaggedDeps Component PN
    extractCommon ps ps' = [ D.Simple (Dep pn1 (Constrained [(vr1 .||. vr2, P pn)])) comp
                           | D.Simple (Dep pn1 (Constrained [(vr1, _)])) _ <- ps
                           , D.Simple (Dep pn2 (Constrained [(vr2, _)])) _ <- ps'
                           , pn1 == pn2
                           ]

-- | Convert a Cabal dependency to a solver-specific dependency.
convDep :: PN -> Dependency -> Dep PN
convDep pn' (Dependency pn vr) = Dep pn (Constrained [(vr, P pn')])

-- | Convert setup dependencies
convSetupBuildInfo :: PI PN -> SetupBuildInfo -> FlaggedDeps Component PN
convSetupBuildInfo (PI pn _i) nfo =
    L.map (\d -> D.Simple (convDep pn d) ComponentSetup) (PD.setupDepends nfo)
