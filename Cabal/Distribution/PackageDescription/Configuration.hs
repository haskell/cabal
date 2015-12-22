-- -fno-warn-deprecations for use of Map.foldWithKey
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Configuration
-- Copyright   :  Thomas Schilling, 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is about the cabal configurations feature. It exports
-- 'finalizePackageDescription' and 'flattenPackageDescription' which are
-- functions for converting 'GenericPackageDescription's down to
-- 'PackageDescription's. It has code for working with the tree of conditions
-- and resolving or flattening conditions.

module Distribution.PackageDescription.Configuration (
    finalizePackageDescription,
    flattenPackageDescription,

    -- Utils
    parseCondition,
    freeVars,
    mapCondTree,
    mapTreeData,
    mapTreeConds,
    mapTreeConstrs,
  ) where

import Distribution.Package
         ( PackageName, Dependency(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..), PackageDescription(..)
         , Library(..), Executable(..), BuildInfo(..)
         , Flag(..), FlagName(..), FlagAssignment
         , Benchmark(..), CondTree(..), ConfVar(..), Condition(..)
         , TestSuite(..) )
import Distribution.PackageDescription.Utils
         ( cabalBug, userBug )
import Distribution.Version
         ( VersionRange, anyVersion, intersectVersionRanges, withinRange )
import Distribution.Compiler
         ( CompilerId(CompilerId) )
import Distribution.System
         ( Platform(..), OS, Arch )
import Distribution.Simple.Utils
         ( currentDir, lowercase )
import Distribution.Simple.Compiler
         ( CompilerInfo(..) )

import Distribution.Text
         ( Text(parse) )
import Distribution.Compat.ReadP as ReadP hiding ( char )
import Control.Arrow (first)
import qualified Distribution.Compat.ReadP as ReadP ( char )
import Distribution.Compat.Semigroup as Semi

import Data.Char ( isAlphaNum )
import Data.Maybe ( mapMaybe, maybeToList )
import Data.Map ( Map, fromListWith, toList )
import qualified Data.Map as Map

------------------------------------------------------------------------------

-- | Simplify the condition and return its free variables.
simplifyCondition :: Condition c
                  -> (c -> Either d Bool)   -- ^ (partial) variable assignment
                  -> (Condition d, [d])
simplifyCondition cond i = fv . walk $ cond
  where
    walk cnd = case cnd of
      Var v   -> either Var Lit (i v)
      Lit b   -> Lit b
      CNot c  -> case walk c of
                   Lit True -> Lit False
                   Lit False -> Lit True
                   c' -> CNot c'
      COr c d -> case (walk c, walk d) of
                   (Lit False, d') -> d'
                   (Lit True, _)   -> Lit True
                   (c', Lit False) -> c'
                   (_, Lit True)   -> Lit True
                   (c',d')         -> COr c' d'
      CAnd c d -> case (walk c, walk d) of
                    (Lit False, _) -> Lit False
                    (Lit True, d') -> d'
                    (_, Lit False) -> Lit False
                    (c', Lit True) -> c'
                    (c',d')        -> CAnd c' d'
    -- gather free vars
    fv c = (c, fv' c)
    fv' c = case c of
      Var v     -> [v]
      Lit _      -> []
      CNot c'    -> fv' c'
      COr c1 c2  -> fv' c1 ++ fv' c2
      CAnd c1 c2 -> fv' c1 ++ fv' c2

-- | Simplify a configuration condition using the OS and arch names.  Returns
--   the names of all the flags occurring in the condition.
simplifyWithSysParams :: OS -> Arch -> CompilerInfo -> Condition ConfVar
                      -> (Condition FlagName, [FlagName])
simplifyWithSysParams os arch cinfo cond = (cond', flags)
  where
    (cond', flags) = simplifyCondition cond interp
    interp (OS os')    = Right $ os' == os
    interp (Arch arch') = Right $ arch' == arch
    interp (Impl comp vr)
      | matchImpl (compilerInfoId cinfo) = Right True
      | otherwise = case compilerInfoCompat cinfo of
          -- fixme: treat Nothing as unknown, rather than empty list once we
          --        support partial resolution of system parameters
          Nothing     -> Right False
          Just compat -> Right (any matchImpl compat)
          where
            matchImpl (CompilerId c v) = comp == c && v `withinRange` vr
    interp (Flag f) = Left f

-- TODO: Add instances and check
--
-- prop_sC_idempotent cond a o = cond' == cond''
--   where
--     cond'  = simplifyCondition cond a o
--     cond'' = simplifyCondition cond' a o
--
-- prop_sC_noLits cond a o = isLit res || not (hasLits res)
--   where
--     res = simplifyCondition cond a o
--     hasLits (Lit _) = True
--     hasLits (CNot c) = hasLits c
--     hasLits (COr l r) = hasLits l || hasLits r
--     hasLits (CAnd l r) = hasLits l || hasLits r
--     hasLits _ = False
--

-- | Parse a configuration condition from a string.
parseCondition :: ReadP r (Condition ConfVar)
parseCondition = condOr
  where
    condOr   = sepBy1 condAnd (oper "||") >>= return . foldl1 COr
    condAnd  = sepBy1 cond (oper "&&")>>= return . foldl1 CAnd
    cond     = sp >> (boolLiteral +++ inparens condOr +++ notCond +++ osCond
                      +++ archCond +++ flagCond +++ implCond )
    inparens   = between (ReadP.char '(' >> sp) (sp >> ReadP.char ')' >> sp)
    notCond  = ReadP.char '!' >> sp >> cond >>= return . CNot
    osCond   = string "os" >> sp >> inparens osIdent >>= return . Var
    archCond = string "arch" >> sp >> inparens archIdent >>= return . Var
    flagCond = string "flag" >> sp >> inparens flagIdent >>= return . Var
    implCond = string "impl" >> sp >> inparens implIdent >>= return . Var
    boolLiteral   = fmap Lit  parse
    archIdent     = fmap Arch parse
    osIdent       = fmap OS   parse
    flagIdent     = fmap (Flag . FlagName . lowercase) (munch1 isIdentChar)
    isIdentChar c = isAlphaNum c || c == '_' || c == '-'
    oper s        = sp >> string s >> sp
    sp            = skipSpaces
    implIdent     = do i <- parse
                       vr <- sp >> option anyVersion parse
                       return $ Impl i vr

------------------------------------------------------------------------------

mapCondTree :: (a -> b) -> (c -> d) -> (Condition v -> Condition w)
            -> CondTree v c a -> CondTree w d b
mapCondTree fa fc fcnd (CondNode a c ifs) =
    CondNode (fa a) (fc c) (map g ifs)
  where
    g (cnd, t, me) = (fcnd cnd, mapCondTree fa fc fcnd t,
                           fmap (mapCondTree fa fc fcnd) me)

mapTreeConstrs :: (c -> d) -> CondTree v c a -> CondTree v d a
mapTreeConstrs f = mapCondTree id f id

mapTreeConds :: (Condition v -> Condition w) -> CondTree v c a -> CondTree w c a
mapTreeConds f = mapCondTree id id f

mapTreeData :: (a -> b) -> CondTree v c a -> CondTree v c b
mapTreeData f = mapCondTree f id id

-- | Result of dependency test. Isomorphic to @Maybe d@ but renamed for
--   clarity.
data DepTestRslt d = DepOk | MissingDeps d

instance Semigroup d => Monoid (DepTestRslt d) where
    mempty = DepOk
    mappend = (Semi.<>)

instance Semigroup d => Semigroup (DepTestRslt d) where
    DepOk <> x     = x
    x     <> DepOk = x
    (MissingDeps d) <> (MissingDeps d') = MissingDeps (d <> d')

data BT a = BTN a | BTB (BT a) (BT a)  -- very simple binary tree


-- | Try to find a flag assignment that satisfies the constraints of all trees.
--
-- Returns either the missing dependencies, or a tuple containing the
-- resulting data, the associated dependencies, and the chosen flag
-- assignments.
--
-- In case of failure, the _smallest_ number of of missing dependencies is
-- returned. [TODO: Could also be specified with a function argument.]
--
-- TODO: The current algorithm is rather naive.  A better approach would be to:
--
-- * Rule out possible paths, by taking a look at the associated dependencies.
--
-- * Infer the required values for the conditions of these paths, and
--   calculate the required domains for the variables used in these
--   conditions.  Then picking a flag assignment would be linear (I guess).
--
-- This would require some sort of SAT solving, though, thus it's not
-- implemented unless we really need it.
--
resolveWithFlags ::
     [(FlagName,[Bool])]
        -- ^ Domain for each flag name, will be tested in order.
  -> OS      -- ^ OS as returned by Distribution.System.buildOS
  -> Arch    -- ^ Arch as returned by Distribution.System.buildArch
  -> CompilerInfo  -- ^ Compiler information
  -> [Dependency]  -- ^ Additional constraints
  -> [CondTree ConfVar [Dependency] PDTagged]
  -> ([Dependency] -> DepTestRslt [Dependency])  -- ^ Dependency test function.
  -> Either [Dependency] (TargetSet PDTagged, FlagAssignment)
       -- ^ Either the missing dependencies (error case), or a pair of
       -- (set of build targets with dependencies, chosen flag assignments)
resolveWithFlags dom os arch impl constrs trees checkDeps =
    case try dom [] of
      Right r -> Right r
      Left dbt -> Left $ findShortest dbt
  where
    extraConstrs = toDepMap constrs

    -- simplify trees by (partially) evaluating all conditions and converting
    -- dependencies to dependency maps.
    simplifiedTrees = map ( mapTreeConstrs toDepMap  -- convert to maps
                          . mapTreeConds (fst . simplifyWithSysParams os arch impl))
                          trees

    -- @try@ recursively tries all possible flag assignments in the domain and
    -- either succeeds or returns a binary tree with the missing dependencies
    -- encountered in each run.  Since the tree is constructed lazily, we
    -- avoid some computation overhead in the successful case.
    try [] flags =
        let targetSet = TargetSet $ flip map simplifiedTrees $
                -- apply additional constraints to all dependencies
                first (`constrainBy` extraConstrs) .
                simplifyCondTree (env flags)
            deps = overallDependencies targetSet
        in case checkDeps (fromDepMap deps) of
             DepOk           -> Right (targetSet, flags)
             MissingDeps mds -> Left (BTN mds)

    try ((n, vals):rest) flags =
        tryAll $ map (\v -> try rest ((n, v):flags)) vals

    tryAll = foldr mp mz

    -- special version of `mplus' for our local purposes
    mp (Left xs)   (Left ys)   = (Left (BTB xs ys))
    mp (Left _)    m@(Right _) = m
    mp m@(Right _) _           = m

    -- `mzero'
    mz = Left (BTN [])

    env flags flag = (maybe (Left flag) Right . lookup flag) flags

    -- for the error case we inspect our lazy tree of missing dependencies and
    -- pick the shortest list of missing dependencies
    findShortest (BTN x) = x
    findShortest (BTB lt rt) =
        let l = findShortest lt
            r = findShortest rt
        in case (l,r) of
             ([], xs) -> xs  -- [] is too short
             (xs, []) -> xs
             ([x], _) -> [x] -- single elem is optimum
             (_, [x]) -> [x]
             (xs, ys) -> if lazyLengthCmp xs ys
                         then xs else ys
    -- lazy variant of @\xs ys -> length xs <= length ys@
    lazyLengthCmp [] _ = True
    lazyLengthCmp _ [] = False
    lazyLengthCmp (_:xs) (_:ys) = lazyLengthCmp xs ys

-- | A map of dependencies.  Newtyped since the default monoid instance is not
--   appropriate.  The monoid instance uses 'intersectVersionRanges'.
newtype DependencyMap = DependencyMap { unDependencyMap :: Map PackageName VersionRange }
  deriving (Show, Read)

instance Monoid DependencyMap where
    mempty = DependencyMap Map.empty
    mappend = (Semi.<>)

instance Semigroup DependencyMap where
    (DependencyMap a) <> (DependencyMap b) =
        DependencyMap (Map.unionWith intersectVersionRanges a b)

toDepMap :: [Dependency] -> DependencyMap
toDepMap ds =
  DependencyMap $ fromListWith intersectVersionRanges [ (p,vr) | Dependency p vr <- ds ]

fromDepMap :: DependencyMap -> [Dependency]
fromDepMap m = [ Dependency p vr | (p,vr) <- toList (unDependencyMap m) ]

simplifyCondTree :: (Monoid a, Monoid d) =>
                    (v -> Either v Bool)
                 -> CondTree v d a
                 -> (d, a)
simplifyCondTree env (CondNode a d ifs) =
    mconcat $ (d, a) : mapMaybe simplifyIf ifs
  where
    simplifyIf (cnd, t, me) =
        case simplifyCondition cnd env of
          (Lit True, _) -> Just $ simplifyCondTree env t
          (Lit False, _) -> fmap (simplifyCondTree env) me
          _ -> error $ "Environment not defined for all free vars"

-- | Flatten a CondTree.  This will resolve the CondTree by taking all
--  possible paths into account.  Note that since branches represent exclusive
--  choices this may not result in a \"sane\" result.
ignoreConditions :: (Monoid a, Monoid c) => CondTree v c a -> (a, c)
ignoreConditions (CondNode a c ifs) = (a, c) `mappend` mconcat (concatMap f ifs)
  where f (_, t, me) = ignoreConditions t
                       : maybeToList (fmap ignoreConditions me)

freeVars :: CondTree ConfVar c a  -> [FlagName]
freeVars t = [ f | Flag f <- freeVars' t ]
  where
    freeVars' (CondNode _ _ ifs) = concatMap compfv ifs
    compfv (c, ct, mct) = condfv c ++ freeVars' ct ++ maybe [] freeVars' mct
    condfv c = case c of
      Var v      -> [v]
      Lit _      -> []
      CNot c'    -> condfv c'
      COr c1 c2  -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2


------------------------------------------------------------------------------

-- | A set of targets with their package dependencies
newtype TargetSet a = TargetSet [(DependencyMap, a)]

-- | Combine the target-specific dependencies in a TargetSet to give the
-- dependencies for the package as a whole.
overallDependencies :: TargetSet PDTagged -> DependencyMap
overallDependencies (TargetSet targets) = mconcat depss
  where
    (depss, _) = unzip $ filter (removeDisabledSections . snd) targets
    removeDisabledSections :: PDTagged -> Bool
    removeDisabledSections (Lib _) = True
    removeDisabledSections (Exe _ _) = True
    removeDisabledSections (Test _ t) = testEnabled t
    removeDisabledSections (Bench _ b) = benchmarkEnabled b
    removeDisabledSections PDNull = True

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy :: DependencyMap  -- ^ Input map
            -> DependencyMap  -- ^ Extra constraints
            -> DependencyMap
constrainBy left extra =
    DependencyMap $
      Map.foldWithKey tightenConstraint (unDependencyMap left)
                                        (unDependencyMap extra)
  where tightenConstraint n c l =
            case Map.lookup n l of
              Nothing -> l
              Just vr -> Map.insert n (intersectVersionRanges vr c) l

-- | Collect up the targets in a TargetSet of tagged targets, storing the
-- dependencies as we go.
flattenTaggedTargets :: TargetSet PDTagged ->
        (Maybe Library, [(String, Executable)], [(String, TestSuite)]
        , [(String, Benchmark)])
flattenTaggedTargets (TargetSet targets) = foldr untag (Nothing, [], [], []) targets
  where
    untag (_, Lib _) (Just _, _, _, _) = userBug "Only one library expected"
    untag (deps, Lib l) (Nothing, exes, tests, bms) =
        (Just l', exes, tests, bms)
      where
        l' = l {
                libBuildInfo = (libBuildInfo l) { targetBuildDepends = fromDepMap deps }
            }
    untag (deps, Exe n e) (mlib, exes, tests, bms)
        | any ((== n) . fst) exes =
          userBug $ "There exist several exes with the same name: '" ++ n ++ "'"
        | any ((== n) . fst) tests =
          userBug $ "There exists a test with the same name as an exe: '" ++ n ++ "'"
        | any ((== n) . fst) bms =
          userBug $ "There exists a benchmark with the same name as an exe: '" ++ n ++ "'"
        | otherwise = (mlib, (n, e'):exes, tests, bms)
      where
        e' = e {
                buildInfo = (buildInfo e) { targetBuildDepends = fromDepMap deps }
            }
    untag (deps, Test n t) (mlib, exes, tests, bms)
        | any ((== n) . fst) tests =
          userBug $ "There exist several tests with the same name: '" ++ n ++ "'"
        | any ((== n) . fst) exes =
          userBug $ "There exists an exe with the same name as the test: '" ++ n ++ "'"
        | any ((== n) . fst) bms =
          userBug $ "There exists a benchmark with the same name as the test: '" ++ n ++ "'"
        | otherwise = (mlib, exes, (n, t'):tests, bms)
      where
        t' = t {
            testBuildInfo = (testBuildInfo t)
                { targetBuildDepends = fromDepMap deps }
            }
    untag (deps, Bench n b) (mlib, exes, tests, bms)
        | any ((== n) . fst) bms =
          userBug $ "There exist several benchmarks with the same name: '" ++ n ++ "'"
        | any ((== n) . fst) exes =
          userBug $ "There exists an exe with the same name as the benchmark: '" ++ n ++ "'"
        | any ((== n) . fst) tests =
          userBug $ "There exists a test with the same name as the benchmark: '" ++ n ++ "'"
        | otherwise = (mlib, exes, tests, (n, b'):bms)
      where
        b' = b {
            benchmarkBuildInfo = (benchmarkBuildInfo b)
                { targetBuildDepends = fromDepMap deps }
            }
    untag (_, PDNull) x = x  -- actually this should not happen, but let's be liberal


------------------------------------------------------------------------------
-- Convert GenericPackageDescription to PackageDescription
--

data PDTagged = Lib Library
              | Exe String Executable
              | Test String TestSuite
              | Bench String Benchmark
              | PDNull
              deriving Show

instance Monoid PDTagged where
    mempty = PDNull
    mappend = (Semi.<>)

instance Semigroup PDTagged where
    PDNull    <> x      = x
    x         <> PDNull = x
    Lib l     <> Lib l' = Lib (l <> l')
    Exe n e   <> Exe   n' e' | n == n' = Exe n (e <> e')
    Test n t  <> Test  n' t' | n == n' = Test n (t <> t')
    Bench n b <> Bench n' b' | n == n' = Bench n (b <> b')
    _         <> _  = cabalBug "Cannot combine incompatible tags"

-- | Create a package description with all configurations resolved.
--
-- This function takes a `GenericPackageDescription` and several environment
-- parameters and tries to generate `PackageDescription` by finding a flag
-- assignment that result in satisfiable dependencies.
--
-- It takes as inputs a not necessarily complete specifications of flags
-- assignments, an optional package index as well as platform parameters.  If
-- some flags are not assigned explicitly, this function will try to pick an
-- assignment that causes this function to succeed.  The package index is
-- optional since on some platforms we cannot determine which packages have
-- been installed before.  When no package index is supplied, every dependency
-- is assumed to be satisfiable, therefore all not explicitly assigned flags
-- will get their default values.
--
-- This function will fail if it cannot find a flag assignment that leads to
-- satisfiable dependencies.  (It will not try alternative assignments for
-- explicitly specified flags.)  In case of failure it will return a /minimum/
-- number of dependencies that could not be satisfied.  On success, it will
-- return the package description and the full flag assignment chosen.
--
finalizePackageDescription ::
     FlagAssignment  -- ^ Explicitly specified flag assignments
  -> (Dependency -> Bool) -- ^ Is a given dependency satisfiable from the set of
                          -- available packages?  If this is unknown then use
                          -- True.
  -> Platform      -- ^ The 'Arch' and 'OS'
  -> CompilerInfo  -- ^ Compiler information
  -> [Dependency]  -- ^ Additional constraints
  -> GenericPackageDescription
  -> Either [Dependency]
            (PackageDescription, FlagAssignment)
             -- ^ Either missing dependencies or the resolved package
             -- description along with the flag assignments chosen.
finalizePackageDescription userflags satisfyDep
        (Platform arch os) impl constraints
        (GenericPackageDescription pkg flags mlib0 exes0 tests0 bms0) =
    case resolveFlags of
      Right ((mlib, exes', tests', bms'), targetSet, flagVals) ->
        Right ( pkg { library = mlib
                    , executables = exes'
                    , testSuites = tests'
                    , benchmarks = bms'
                    , buildDepends = fromDepMap (overallDependencies targetSet)
                      --TODO: we need to find a way to avoid pulling in deps
                      -- for non-buildable components. However cannot simply
                      -- filter at this stage, since if the package were not
                      -- available we would have failed already.
                    }
              , flagVals )

      Left missing -> Left missing
  where
    -- Combine lib, exes, and tests into one list of @CondTree@s with tagged data
    condTrees = maybeToList (fmap (mapTreeData Lib) mlib0 )
                ++ map (\(name,tree) -> mapTreeData (Exe name) tree) exes0
                ++ map (\(name,tree) -> mapTreeData (Test name) tree) tests0
                ++ map (\(name,tree) -> mapTreeData (Bench name) tree) bms0

    resolveFlags =
        case resolveWithFlags flagChoices os arch impl constraints condTrees check of
          Right (targetSet, fs) ->
              let (mlib, exes, tests, bms) = flattenTaggedTargets targetSet in
              Right ( (fmap libFillInDefaults mlib,
                       map (\(n,e) -> (exeFillInDefaults e) { exeName = n }) exes,
                       map (\(n,t) -> (testFillInDefaults t) { testName = n }) tests,
                       map (\(n,b) -> (benchFillInDefaults b) { benchmarkName = n }) bms),
                     targetSet, fs)
          Left missing      -> Left missing

    flagChoices    = map (\(MkFlag n _ d manual) -> (n, d2c manual n d)) flags
    d2c manual n b = case lookup n userflags of
                     Just val -> [val]
                     Nothing
                      | manual -> [b]
                      | otherwise -> [b, not b]
    --flagDefaults = map (\(n,x:_) -> (n,x)) flagChoices
    check ds     = let missingDeps = filter (not . satisfyDep) ds
                   in if null missingDeps
                      then DepOk
                      else MissingDeps missingDeps

{-
let tst_p = (CondNode [1::Int] [Distribution.Package.Dependency "a" AnyVersion] [])
let tst_p2 = (CondNode [1::Int] [Distribution.Package.Dependency "a" (EarlierVersion (Version [1,0] [])), Distribution.Package.Dependency "a" (LaterVersion (Version [2,0] []))] [])

let p_index = Distribution.Simple.PackageIndex.fromList [Distribution.Package.PackageIdentifier "a" (Version [0,5] []), Distribution.Package.PackageIdentifier "a" (Version [2,5] [])]
let look = not . null . Distribution.Simple.PackageIndex.lookupDependency p_index
let looks ds = mconcat $ map (\d -> if look d then DepOk else MissingDeps [d]) ds
resolveWithFlags [] Distribution.System.Linux Distribution.System.I386 (Distribution.Compiler.GHC,Version [6,8,2] []) [tst_p] looks   ===>  Right ...
resolveWithFlags [] Distribution.System.Linux Distribution.System.I386 (Distribution.Compiler.GHC,Version [6,8,2] []) [tst_p2] looks  ===>  Left ...
-}

-- | Flatten a generic package description by ignoring all conditions and just
-- join the field descriptors into on package description.  Note, however,
-- that this may lead to inconsistent field values, since all values are
-- joined into one field, which may not be possible in the original package
-- description, due to the use of exclusive choices (if ... else ...).
--
-- TODO: One particularly tricky case is defaulting.  In the original package
-- description, e.g., the source directory might either be the default or a
-- certain, explicitly set path.  Since defaults are filled in only after the
-- package has been resolved and when no explicit value has been set, the
-- default path will be missing from the package description returned by this
-- function.
flattenPackageDescription :: GenericPackageDescription -> PackageDescription
flattenPackageDescription (GenericPackageDescription pkg _ mlib0 exes0 tests0 bms0) =
    pkg { library = mlib
        , executables = reverse exes
        , testSuites = reverse tests
        , benchmarks = reverse bms
        , buildDepends = ldeps ++ reverse edeps ++ reverse tdeps ++ reverse bdeps
        }
  where
    (mlib, ldeps) = case mlib0 of
        Just lib -> let (l,ds) = ignoreConditions lib in
                    (Just (libFillInDefaults l), ds)
        Nothing -> (Nothing, [])
    (exes, edeps) = foldr flattenExe ([],[]) exes0
    (tests, tdeps) = foldr flattenTst ([],[]) tests0
    (bms, bdeps) = foldr flattenBm ([],[]) bms0
    flattenExe (n, t) (es, ds) =
        let (e, ds') = ignoreConditions t in
        ( (exeFillInDefaults $ e { exeName = n }) : es, ds' ++ ds )
    flattenTst (n, t) (es, ds) =
        let (e, ds') = ignoreConditions t in
        ( (testFillInDefaults $ e { testName = n }) : es, ds' ++ ds )
    flattenBm (n, t) (es, ds) =
        let (e, ds') = ignoreConditions t in
        ( (benchFillInDefaults $ e { benchmarkName = n }) : es, ds' ++ ds )

-- This is in fact rather a hack.  The original version just overrode the
-- default values, however, when adding conditions we had to switch to a
-- modifier-based approach.  There, nothing is ever overwritten, but only
-- joined together.
--
-- This is the cleanest way i could think of, that doesn't require
-- changing all field parsing functions to return modifiers instead.
libFillInDefaults :: Library -> Library
libFillInDefaults lib@(Library { libBuildInfo = bi }) =
    lib { libBuildInfo = biFillInDefaults bi }

exeFillInDefaults :: Executable -> Executable
exeFillInDefaults exe@(Executable { buildInfo = bi }) =
    exe { buildInfo = biFillInDefaults bi }

testFillInDefaults :: TestSuite -> TestSuite
testFillInDefaults tst@(TestSuite { testBuildInfo = bi }) =
    tst { testBuildInfo = biFillInDefaults bi }

benchFillInDefaults :: Benchmark -> Benchmark
benchFillInDefaults bm@(Benchmark { benchmarkBuildInfo = bi }) =
    bm { benchmarkBuildInfo = biFillInDefaults bi }

biFillInDefaults :: BuildInfo -> BuildInfo
biFillInDefaults bi =
    if null (hsSourceDirs bi)
    then bi { hsSourceDirs = [currentDir] }
    else bi
