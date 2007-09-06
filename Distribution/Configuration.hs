{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Configuration
-- Copyright   :  Thomas Schilling, 2007
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Configurations

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Configuration (
    Flag(..),
    ConfVar(..),
    Condition(..), parseCondition, simplifyCondition,
    CondTree(..), ppCondTree, mapTreeData,
    --satisfyFlags, 
    resolveWithFlags, ignoreConditions,
    DepTestRslt(..)
  ) where

import Distribution.Compat.ReadP as ReadP hiding ( char )
import qualified Distribution.Compat.ReadP as ReadP ( char )
import Distribution.Version 
    ( Version(..), VersionRange(..), withinRange
    , showVersionRange, parseVersionRange )

import Text.PrettyPrint.HughesPJ

import Data.Char ( isAlphaNum, toLower )
import Data.Maybe ( catMaybes, maybeToList )
import Data.Monoid

#ifdef DEBUG
import Data.List ( (\\) )
import Distribution.ParseUtils
#endif

------------------------------------------------------------------------------

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data Flag = MkFlag
    { flagName        :: String
    , flagDescription :: String
    , flagDefault     :: Bool
    }

instance Show Flag where show (MkFlag n _ _) = n

-- | A @ConfFlag@ represents an user-defined flag
data ConfFlag = ConfFlag String
    deriving Eq

-- | A @ConfVar@ represents the variable type used. 
data ConfVar = OS String 
             | Arch String 
             | Flag ConfFlag
             | Impl String VersionRange
               deriving Eq

instance Show ConfVar where
    show (OS n) = "os(" ++ n ++ ")"
    show (Arch n) = "arch(" ++ n ++ ")"
    show (Flag (ConfFlag f)) = "flag(" ++ f ++ ")"
    show (Impl c v) = "impl(" ++ c ++ " " ++ showVersionRange v ++ ")"

-- | A boolean expression parameterized over the variable type used.
data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
                   
instance Show c => Show (Condition c) where
    show c = render $ ppCond c

-- | Pretty print a @Condition@.
ppCond :: Show c => Condition c -> Doc
ppCond (Var x) = text (show x)
ppCond (Lit b) = text (show b)
ppCond (CNot c) = char '!' <> parens (ppCond c)
ppCond (COr c1 c2) = parens $ sep [ppCond c1, text "||" <+> ppCond c2]
ppCond (CAnd c1 c2) = parens $ sep [ppCond c1, text "&&" <+> ppCond c2]


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

-- | Simplify a configuration condition using the os and arch names.  Returns
--   the names of all the flags occurring in the condition.
simplifyWithSysParams :: String -> String -> (String, Version) -> Condition ConfVar ->  
                         (Condition ConfFlag, [String])
simplifyWithSysParams os arch (impl, implVer) cond = (cond', flags)
  where
    (cond', fvs) = simplifyCondition cond interp 
    interp (OS name)   = Right $ name == os
    interp (Arch name) = Right $ name == arch
    interp (Impl i vr) = Right $ impl == i && implVer `withinRange` vr
    interp (Flag  f)   = Left f
    flags = [ fname | ConfFlag fname <- fvs ]

-- XXX: Add instances and check
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
    cond     = sp >> (lit +++ inparens condOr +++ notCond +++ osCond 
                      +++ archCond +++ flagCond +++ implCond )
    inparens   = between (ReadP.char '(' >> sp) (sp >> ReadP.char ')' >> sp)
    notCond  = ReadP.char '!' >> sp >> cond >>= return . CNot
    osCond   = string "os" >> sp >> inparens osIdent >>= return . Var. OS 
    archCond = string "arch" >> sp >> inparens archIdent >>= return . Var . Arch 
    flagCond = string "flag" >> sp >> inparens flagIdent >>= return . Var . Flag . ConfFlag
    implCond = string "impl" >> sp >> inparens implIdent >>= return . Var
    ident    = munch1 isIdentChar >>= return . map toLower
    lit      = ((string "true" <++ string "True") >> return (Lit True)) <++ 
               ((string "false" <++ string "False") >> return (Lit False))
    archIdent     = ident >>= return 
    osIdent       = ident >>= return 
    flagIdent     = ident
    isIdentChar c = isAlphaNum c || (c `elem` "_-")
    oper s        = sp >> string s >> sp
    sp            = skipSpaces
    implIdent     = do i <- ident
                       vr <- sp >> option AnyVersion parseVersionRange
                       return $ Impl i vr

------------------------------------------------------------------------------

data CondTree v c a = CondNode 
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [( Condition v
                              , CondTree v c a
                              , Maybe (CondTree v c a))]
    }

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

instance (Show v, Show c) => Show (CondTree v c a) where
    show t = render $ ppCondTree t (text . show)

ppCondTree :: Show v => CondTree v c a -> (c -> Doc) -> Doc
ppCondTree (CondNode _dat cs ifs) ppD =
    (text "depends: " <+>
      ppD cs)
    $+$
    (vcat $ map ppIf ifs)
  where 
    ppIf (c,thenTree,mElseTree) = 
        ((text "if" <+> ppCond c <> colon) $$
          nest 2 (ppCondTree thenTree ppD))
        $+$ (maybe empty (\t -> text "else: " $$ nest 2 (ppCondTree t ppD))
                   mElseTree)

 
-- | Result of dependency test. Isomorphic to @Maybe d@ but renamed for
--   clarity.
data DepTestRslt d = DepOk | MissingDeps d 

instance Monoid d => Monoid (DepTestRslt d) where
    mempty = DepOk
    mappend DepOk x = x
    mappend x DepOk = x
    mappend (MissingDeps d) (MissingDeps d') = MissingDeps (d `mappend` d')


data BT a = BTN a | BTB (BT a) (BT a)  -- very simple binary tree


-- | Try to find a flag assignment that satisfies the constaints of all trees.
--
-- Returns either the missing dependencies, or a tuple containing the
-- resulting data, the associated dependencies, and the chosen flag
-- assignments.
--
-- In case of failure, the _smallest_ number of of missing dependencies is
-- returned. [XXX: Could also be specified with a function argument.]
--
-- XXX: The current algorithm is rather naive.  A better approach would be to:
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
resolveWithFlags :: Monoid a =>
     [(String,[Bool])] 
        -- ^ Domain for each flag name, will be tested in order.
  -> String  -- ^ OS name, as returned by System.Info.os
  -> String  -- ^ arch name, as returned by System.Info.arch
  -> (String, Version) -- ^ Compiler name + version
  -> [CondTree ConfVar [d] a]    
  -> ([d] -> DepTestRslt [d])  -- ^ Dependency test function.
  -> (Either [d] -- missing dependencies
       ([a], [d], [(String, Bool)]))
resolveWithFlags dom os arch impl trees checkDeps =
    case try dom [] of
      Right r -> Right r
      Left dbt -> Left $ findShortest dbt
  where 
    -- Check dependencies only once; might avoid some duplicate efforts.
    preCheckedTrees = map ( mapTreeConstrs (\d -> (checkDeps d,d))
                          . mapTreeConds (fst . simplifyWithSysParams os arch impl) )
                        trees

    -- @try@ recursively tries all possible flag assignments in the domain and
    -- either succeeds or returns a binary tree with the missing dependencies
    -- encountered in each run.  Since the tree is constructed lazily, we
    -- avoid some computation overhead in the successful case.
    try [] flags = 
        let (depss, as) = unzip 
                         . map (simplifyCondTree (env flags)) 
                         $ preCheckedTrees
        in case mconcat depss of
             (DepOk, ds) -> Right (as, ds, flags)
             (MissingDeps mds, _) -> Left (BTN mds)
    try ((n, vals):rest) flags = 
        tryAll $ map (\v -> try rest ((n, v):flags)) vals

    tryAll = foldr mp mz

    -- special version of `mplus' for our local purposes
    mp (Left xs)   (Left ys)   = (Left (BTB xs ys))
    mp (Left _)    m@(Right _) = m
    mp m@(Right _) _           = m

    -- `mzero'
    mz = Left (BTN [])

    env flags flag@(ConfFlag n) = maybe (Left flag) Right . lookup n $ flags 

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



simplifyCondTree :: (Monoid a, Monoid d) =>
                    (v -> Either v Bool) 
                 -> CondTree v d a 
                 -> (d, a)
simplifyCondTree env (CondNode a d ifs) =
    foldr mappend (d, a) $ catMaybes $ map simplifyIf ifs
  where
    simplifyIf (cnd, t, me) = 
        case simplifyCondition cnd env of
          (Lit True, _) -> Just $ simplifyCondTree env t
          (Lit False, _) -> fmap (simplifyCondTree env) me
          _ -> error $ "Environment not defined for all free vars" 

-- | Flatten a CondTree.  This will resolve the CondTree by taking all
--  possible paths into account.  Note that since branches represent exclusive
--  choices this may not result in a "sane" result.
ignoreConditions :: (Monoid a, Monoid c) => CondTree v c a -> (a, c)
ignoreConditions (CondNode a c ifs) = (a, c) `mappend` mconcat (concatMap f ifs)
  where f (_, t, me) = ignoreConditions t 
                       : maybeToList (fmap ignoreConditions me)

------------------------------------------------------------------------------
-- compatibility

#if (__GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ <= 602)
-- This instance wasn't present in Data.Monoid in GHC 6.2
instance (Monoid a, Monoid b) => Monoid (a,b) where
	mempty = (mempty, mempty)
	(a1,b1) `mappend` (a2,b2) =
		(a1 `mappend` a2, b1 `mappend` b2)
#endif

------------------------------------------------------------------------------
-- Testing

#ifdef DEBUG

tstTree :: CondTree ConfVar [Int] String
tstTree = CondNode "A" [0] 
              [ (CNot (Var (Flag (ConfFlag "a"))), 
                 CondNode "B" [1] [],
                 Nothing)
              , (CAnd (Var (Flag (ConfFlag "b"))) (Var (Flag (ConfFlag "c"))),
                CondNode "C" [2] [],
                Just $ CondNode "D" [3] 
                         [ (Lit True,
                           CondNode "E" [4] [],
                           Just $ CondNode "F" [5] []) ])
                ]


test_simplify = simplifyWithSysParams i386 darwin ("ghc",Version [6,6] []) tstCond 
  where 
    tstCond = COr (CAnd (Var (Arch ppc)) (Var (OS darwin)))
                  (CAnd (Var (Flag (ConfFlag "debug"))) (Var (OS darwin)))
    [ppc,i386] = ["ppc","i386"]
    [darwin,windows] = ["darwin","windows"]



test_parseCondition = map (runP 1 "test" parseCondition) testConditions
  where
    testConditions = [ "os(darwin)"
                     , "arch(i386)"
                     , "!os(linux)"
                     , "! arch(ppc)"
                     , "os(windows) && arch(i386)"
                     , "os(windows) && arch(i386) && flag(debug)"
                     , "true && false || false && true"  -- should be same 
                     , "(true && false) || (false && true)"  -- as this
                     , "(os(darwin))"
                     , " ( os ( darwin ) ) "
                     , "true && !(false || os(plan9))"
                     , "flag( foo_bar )"
                     , "flag( foo_O_-_O_bar )"
                     , "impl ( ghc )"
                     , "impl( ghc >= 6.6.1 )"
                     ]

test_ppCondTree = render $ ppCondTree tstTree (text . show)
  

test_simpCondTree = simplifyCondTree env tstTree
  where
    env x = maybe (Left x) Right (lookup x flags)
    flags = [(mkFlag "a",False), (mkFlag "b",False), (mkFlag "c", True)] 
    mkFlag = Flag . ConfFlag

test_resolveWithFlags = resolveWithFlags dom "os" "arch" ("ghc",Version [6,6] []) [tstTree] check
  where
    dom = [("a", [False,True]), ("b", [True,False]), ("c", [True,False])]
    check ds = let missing = ds \\ avail in
               case missing of
                 [] -> DepOk
                 _ -> MissingDeps missing
    avail = [0,1,3,4]

test_ignoreConditions = ignoreConditions tstTree

#endif
