module Distribution.Client.Dependency.Modular.IndexConversion where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Prelude hiding (pi)

import qualified Distribution.Client.PackageIndex as CI
import Distribution.Client.Types
import Distribution.Compiler
import Distribution.InstalledPackageInfo as IPI
import Distribution.Package                          -- from Cabal
import Distribution.PackageDescription as PD         -- from Cabal
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.System

import Distribution.Client.Dependency.Modular.Dependency as D
import Distribution.Client.Dependency.Modular.Flag as F
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Version

-- | Convert both the installed package index and the source package
-- index into one uniform solver index.
convPIs :: OS -> Arch -> CompilerId ->
           SI.PackageIndex -> CI.PackageIndex SourcePackage -> Index
convPIs os arch cid iidx sidx =
  mkIndex (L.concatMap (convIP iidx)        (SI.allPackages iidx) ++
           L.map       (convSP os arch cid) (CI.allPackages sidx))

-- | Convert a Cabal installed package index to the simpler,
-- more uniform index format of the solver.
convIPI :: SI.PackageIndex -> Index
convIPI idx = mkIndex . L.concatMap (convIP idx) . SI.allPackages $ idx

-- | Convert a single installed package into the solver-specific format.
--
-- May return the empty list if the installed package is broken.
convIP :: SI.PackageIndex -> InstalledPackageInfo -> [(PN, I, PInfo)]
convIP idx ipi =
  let ipid = installedPackageId ipi
      i = I (pkgVersion (sourcePackageId ipi)) (Inst ipid)
      pn = pkgName (sourcePackageId ipi)
  in  maybeToList $ do
        fds <- mapM (convIPId pn idx) (IPI.depends ipi)
        return (pn, i, PInfo fds M.empty [])
-- TODO: Installed packages should also store their encapsulations!

-- | Convert dependencies specified by an installed package id into
-- flagged dependencies of the solver.
--
-- May return Nothing if the package can't be found in the index. That
-- indicates that the original package having this dependency is broken
-- and should be ignored.
convIPId :: PN -> SI.PackageIndex -> InstalledPackageId -> Maybe (FlaggedDep PN)
convIPId pn' idx ipid =
  case SI.lookupInstalledPackageId idx ipid of
    Nothing  -> Nothing
    Just ipi -> let i = I (pkgVersion (sourcePackageId ipi)) (Inst ipid)
                    pn = pkgName (sourcePackageId ipi)
                in  Just (D.Simple (Dep pn (Fixed i (Goal (P pn') []))))

-- | Convert a cabal-install source package index to the simpler,
-- more uniform index format of the solver.
convSPI :: OS -> Arch -> CompilerId ->
           CI.PackageIndex SourcePackage -> Index
convSPI os arch cid = mkIndex . L.map (convSP os arch cid) . CI.allPackages

-- | Convert a single source package into the solver-specific format.
convSP :: OS -> Arch -> CompilerId -> SourcePackage -> (PN, I, PInfo)
convSP os arch cid (SourcePackage (PackageIdentifier pn pv) gpd _pl) =
  let i = I pv InRepo
  in  (pn, i, convGPD os arch cid (PI pn i) gpd)

-- We do not use 'flattenPackageDescription' or 'finalizePackageDescription'
-- from 'Distribution.PackageDescription.Configuration' here, because we
-- want to keep the condition tree, but simplify much of the test.

-- | Convert a generic package description to a solver-specific 'PInfo'.
--
-- TODO: We currently just take all dependencies from all specified library,
-- executable and test components. This does not quite seem fair.
convGPD :: OS -> Arch -> CompilerId ->
           PI PN -> GenericPackageDescription -> PInfo
convGPD os arch cid
        pi@(PI _pn _i)
        (GenericPackageDescription _ flags libs exes tests benchs) =
  let
    fds = flagDefaults flags
  in
    PInfo
      (maybe []  (convCondTree os arch cid pi fds (const True)          ) libs   ++
       concatMap (convCondTree os arch cid pi fds (const True)     . snd) exes   ++
       concatMap (convCondTree os arch cid pi fds testEnabled      . snd) tests  ++
       concatMap (convCondTree os arch cid pi fds benchmarkEnabled . snd) benchs)
      fds
      [] -- TODO: add encaps

-- | Convert flag information.
flagDefaults :: [PD.Flag] -> FlagDefaults
flagDefaults = M.fromList . L.map (\ (MkFlag fn _ b _) -> (fn, b))

-- | Convert condition trees to flagged dependencies.
convCondTree :: OS -> Arch -> CompilerId -> PI PN -> FlagDefaults ->
                (a -> Bool) -> -- how to detect if a branch is active
                CondTree ConfVar [Dependency] a -> FlaggedDeps PN
convCondTree os arch cid pi@(PI pn _) fds p (CondNode info ds branches)
  | p info    = L.map (D.Simple . convDep pn) ds  -- unconditional dependencies
              ++ concatMap (convBranch os arch cid pi fds p) branches
  | otherwise = []

-- | Branch interpreter.
--
-- Here, we try to simplify one of Cabal's condition tree branches into the
-- solver's flagged dependency format, which is weaker. Condition trees can
-- contain complex logical expression composed from flag choices and special
-- flags (such as architecture, or compiler flavour). We try to evaluate the
-- special flags and subsequently simplify to a tree that only depends on
-- simple flag choices.
convBranch :: OS -> Arch -> CompilerId ->
              PI PN -> FlagDefaults ->
              (a -> Bool) -> -- how to detect if a branch is active
              (Condition ConfVar,
               CondTree ConfVar [Dependency] a,
               Maybe (CondTree ConfVar [Dependency] a)) -> FlaggedDeps PN
convBranch os arch cid@(CompilerId cf cv) pi fds p (c', t', mf') =
  go c' (          convCondTree os arch cid pi fds p   t')
        (maybe [] (convCondTree os arch cid pi fds p) mf')
  where
    go :: Condition ConfVar ->
          FlaggedDeps PN -> FlaggedDeps PN -> FlaggedDeps PN
    go (Lit True)  t _ = t
    go (Lit False) _ f = f
    go (CNot c)    t f = go c f t
    go (CAnd c d)  t f = go c (go d t f) f
    go (COr  c d)  t f = go c t (go d t f)
    go (Var (Flag fn)) t f = [Flagged (FN pi fn) (fds ! fn) t f]
    go (Var (OS os')) t f
      | os == os'      = t
      | otherwise      = f
    go (Var (Arch arch')) t f
      | arch == arch'  = t
      | otherwise      = f
    go (Var (Impl cf' cvr')) t f
      | cf == cf' && checkVR cvr' cv = t
      | otherwise      = f

-- | Convert a Cabal dependency to a solver-specific dependency.
convDep :: PN -> Dependency -> Dep PN
convDep pn' (Dependency pn vr) = Dep pn (Constrained [(vr, Goal (P pn') [])])

-- | Convert a Cabal package identifier to a solver-specific dependency.
convPI :: PN -> PackageIdentifier -> Dep PN
convPI pn' (PackageIdentifier pn v) = Dep pn (Constrained [(eqVR v, Goal (P pn') [])])
