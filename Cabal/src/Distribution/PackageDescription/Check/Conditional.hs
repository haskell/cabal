{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Distribution.PackageDescription.Check.Conditional
-- Copyright   :  Lennart Kolmodin 2008, Francesco Ariis 2023
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Checks on conditional targets (libraries, executables, etc. that are
-- still inside a CondTree and related checks that can only be performed
-- here (variables, duplicated modules).
module Distribution.PackageDescription.Check.Conditional
  ( checkCondTarget
  , checkDuplicateModules
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check.Monad
import Distribution.System

import qualified Data.Map as Map

import Control.Monad

-- As a prerequisite to some checks, we transform a target CondTree into
-- a CondTree of “target + useful context”.
-- This is slightly clearer, is easier to walk without resorting to
-- list comprehensions, allows us in the future to apply some sensible
-- “optimisations” to checks (exclusive branches, etc.).

-- | @nf@ function is needed to appropriately name some targets which need
-- to be spoonfed (otherwise name appears as "").
initTargetAnnotation
  :: Monoid a
  => (UnqualComponentName -> a -> a) -- Naming function for targets.
  -> UnqualComponentName
  -> TargetAnnotation a
initTargetAnnotation nf n = TargetAnnotation (nf n mempty) False

-- | We “build up” target from various slices.
updateTargetAnnotation
  :: Monoid a
  => a -- A target (lib, exe, test, …)
  -> TargetAnnotation a
  -> TargetAnnotation a
updateTargetAnnotation t ta = ta{taTarget = taTarget ta <> t}

-- | Before walking a target 'CondTree', we need to annotate it with
-- information relevant to the checks (read 'TaraAnn' and 'checkCondTarget'
-- doc for more info).
annotateCondTree
  :: forall a
   . (Eq a, Monoid a)
  => [PackageFlag] -- User flags.
  -> TargetAnnotation a
  -> CondTree ConfVar [Dependency] a
  -> CondTree ConfVar [Dependency] (TargetAnnotation a)
annotateCondTree fs ta (CondNode a c bs) =
  let ta' = updateTargetAnnotation a ta
      bs' = map (annotateBranch ta') bs
      bs'' = crossAnnotateBranches defTrueFlags bs'
   in CondNode ta' c bs''
  where
    annotateBranch
      :: TargetAnnotation a
      -> CondBranch ConfVar [Dependency] a
      -> CondBranch
          ConfVar
          [Dependency]
          (TargetAnnotation a)
    annotateBranch wta (CondBranch k t mf) =
      let uf = isPkgFlagCond k
          wta' = wta{taPackageFlag = taPackageFlag wta || uf}
          atf = annotateCondTree fs
       in CondBranch
            k
            (atf wta' t)
            (atf wta <$> mf)
    -- Note how we are passing the *old* wta
    -- in the `else` branch, since we are not
    -- under that flag.

    -- We only want to pick up variables that are flags and that are
    -- \*off* by default.
    isPkgFlagCond :: Condition ConfVar -> Bool
    isPkgFlagCond (Lit _) = False
    isPkgFlagCond (Var (PackageFlag f)) = elem f defOffFlags
    isPkgFlagCond (Var _) = False
    isPkgFlagCond (CNot cn) = not (isPkgFlagCond cn)
    isPkgFlagCond (CAnd ca cb) = isPkgFlagCond ca || isPkgFlagCond cb
    isPkgFlagCond (COr ca cb) = isPkgFlagCond ca && isPkgFlagCond cb

    -- Package flags that are off by default *and* that are manual.
    defOffFlags =
      map flagName $
        filter
          ( \f ->
              not (flagDefault f)
                && flagManual f
          )
          fs

    defTrueFlags :: [PackageFlag]
    defTrueFlags = filter flagDefault fs

-- Propagate contextual information in CondTree branches. This is
-- needed as CondTree is a rosetree and not a binary tree.
crossAnnotateBranches
  :: forall a
   . (Eq a, Monoid a)
  => [PackageFlag] -- `default: true` flags.
  -> [CondBranch ConfVar [Dependency] (TargetAnnotation a)]
  -> [CondBranch ConfVar [Dependency] (TargetAnnotation a)]
crossAnnotateBranches fs bs = map crossAnnBranch bs
  where
    crossAnnBranch
      :: CondBranch ConfVar [Dependency] (TargetAnnotation a)
      -> CondBranch ConfVar [Dependency] (TargetAnnotation a)
    crossAnnBranch wr =
      let
        rs = filter (/= wr) bs
        ts = mapMaybe realiseBranch rs
       in
        updateTargetAnnBranch (mconcat ts) wr

    realiseBranch :: CondBranch ConfVar [Dependency] (TargetAnnotation a) -> Maybe a
    realiseBranch b =
      let
        -- We are only interested in True by default package flags.
        realiseBranchFunction :: ConfVar -> Either ConfVar Bool
        realiseBranchFunction (PackageFlag n) | elem n (map flagName fs) = Right True
        realiseBranchFunction _ = Right False
        ms = simplifyCondBranch realiseBranchFunction (fmap taTarget b)
       in
        fmap snd ms

    updateTargetAnnBranch
      :: a
      -> CondBranch ConfVar [Dependency] (TargetAnnotation a)
      -> CondBranch ConfVar [Dependency] (TargetAnnotation a)
    updateTargetAnnBranch a (CondBranch k t mt) =
      let updateTargetAnnTree (CondNode ka c wbs) =
            (CondNode (updateTargetAnnotation a ka) c wbs)
       in CondBranch k (updateTargetAnnTree t) (updateTargetAnnTree <$> mt)

-- | A conditional target is a library, exe, benchmark etc., destructured
-- in a CondTree. Traversing method: we render the branches, pass a
-- relevant context, collect checks.
checkCondTarget
  :: forall m a
   . (Monad m, Eq a, Monoid a)
  => [PackageFlag] -- User flags.
  -> (a -> CheckM m ()) -- Check function (a = target).
  -> (UnqualComponentName -> a -> a)
  -- Naming function (some targets
  -- need to have their name
  -- spoonfed to them.
  -> (UnqualComponentName, CondTree ConfVar [Dependency] a)
  -- Target name/condtree.
  -> CheckM m ()
checkCondTarget fs cf nf (unqualName, ct) =
  wTree $ annotateCondTree fs (initTargetAnnotation nf unqualName) ct
  where
    -- Walking the tree. Remember that CondTree is not a binary
    -- tree but a /rose/tree.
    wTree
      :: CondTree ConfVar [Dependency] (TargetAnnotation a)
      -> CheckM m ()
    wTree (CondNode ta _ bs)
      -- There are no branches ([] == True) *or* every branch
      -- is “simple” (i.e. missing a 'condBranchIfFalse' part).
      -- This is convenient but not necessarily correct in all
      -- cases; a more precise way would be to check incompatibility
      -- among simple branches conditions (or introduce a principled
      -- `cond` construct in `.cabal` files.
      | all isSimple bs = do
          localCM (initCheckCtx ta) (cf $ taTarget ta)
          mapM_ wBranch bs
      -- If there are T/F conditions, there is no need to check
      -- the intermediate 'TargetAnnotation' too.
      | otherwise = do
          mapM_ wBranch bs

    isSimple
      :: CondBranch ConfVar [Dependency] (TargetAnnotation a)
      -> Bool
    isSimple (CondBranch _ _ Nothing) = True
    isSimple (CondBranch _ _ (Just _)) = False

    wBranch
      :: CondBranch ConfVar [Dependency] (TargetAnnotation a)
      -> CheckM m ()
    wBranch (CondBranch k t mf) = do
      checkCondVars k
      wTree t
      maybe (return ()) wTree mf

-- | Condvar checking (misspelled OS in if conditions, etc).
checkCondVars :: Monad m => Condition ConfVar -> CheckM m ()
checkCondVars cond =
  let (_, vs) = simplifyCondition cond (\v -> Left v)
   in -- Using simplifyCondition is convenient and correct,
      -- if checks become more complex we can always walk
      -- 'Condition'.
      mapM_ vcheck vs
  where
    vcheck :: Monad m => ConfVar -> CheckM m ()
    vcheck (OS (OtherOS os)) =
      tellP (PackageDistInexcusable $ UnknownOS [os])
    vcheck (Arch (OtherArch arch)) =
      tellP (PackageDistInexcusable $ UnknownArch [arch])
    vcheck (Impl (OtherCompiler os) _) =
      tellP (PackageDistInexcusable $ UnknownCompiler [os])
    vcheck _ = return ()

-- Checking duplicated modules cannot unfortunately be done in the
-- “tree checking”. This is because of the monoidal instance in some targets,
-- where e.g. merged dependencies are `nub`’d, hence losing information for
-- this particular check.
checkDuplicateModules :: GenericPackageDescription -> [PackageCheck]
checkDuplicateModules pkg =
  concatMap checkLib (maybe id (:) (condLibrary pkg) . map snd $ condSubLibraries pkg)
    ++ concatMap checkExe (map snd $ condExecutables pkg)
    ++ concatMap checkTest (map snd $ condTestSuites pkg)
    ++ concatMap checkBench (map snd $ condBenchmarks pkg)
  where
    -- the duplicate modules check is has not been thoroughly vetted for backpack
    checkLib = checkDups "library" (\l -> explicitLibModules l ++ map moduleReexportName (reexportedModules l))
    checkExe = checkDups "executable" exeModules
    checkTest = checkDups "test suite" testModules
    checkBench = checkDups "benchmark" benchmarkModules
    checkDups :: String -> (a -> [ModuleName]) -> CondTree v c a -> [PackageCheck]
    checkDups s getModules t =
      let sumPair (x, x') (y, y') = (x + x' :: Int, y + y' :: Int)
          mergePair (x, x') (y, y') = (x + x', max y y')
          maxPair (x, x') (y, y') = (max x x', max y y')
          libMap =
            foldCondTree
              Map.empty
              (\(_, v) -> Map.fromListWith sumPair . map (\x -> (x, (1, 1))) $ getModules v)
              (Map.unionWith mergePair) -- if a module may occur in nonexclusive branches count it twice strictly and once loosely.
              (Map.unionWith maxPair) -- a module occurs the max of times it might appear in exclusive branches
              t
          dupLibsStrict = Map.keys $ Map.filter ((> 1) . fst) libMap
          dupLibsLax = Map.keys $ Map.filter ((> 1) . snd) libMap
       in if not (null dupLibsLax)
            then
              [ PackageBuildImpossible
                  (DuplicateModule s dupLibsLax)
              ]
            else
              if not (null dupLibsStrict)
                then
                  [ PackageDistSuspicious
                      (PotentialDupModule s dupLibsStrict)
                  ]
                else []
