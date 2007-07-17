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
    CondTree(..), ppCondTree, evalCond,
    satisfyFlags, DepTestRslt(..)
  ) where

import Distribution.Compat.ReadP as ReadP hiding ( char )
import qualified Distribution.Compat.ReadP as ReadP ( char )

import Text.PrettyPrint.HughesPJ

import Data.Char ( isAlphaNum, toLower )

#ifdef DEBUG
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

-- | A @ConfVar@ represents the variable type used. 
data ConfVar = OS String 
             | Arch String 
             | Flag String
             
instance Show ConfVar where
    show (OS n) = "os(" ++ n ++ ")"
    show (Arch n) = "arch(" ++ n ++ ")"
    show (Flag f) = "flag(" ++ f ++ ")"

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
                  -> (c -> Maybe Bool)   -- ^ (partial) variable assignment
                  -> (Condition c, [c])
simplifyCondition cond i = fv . walk $ cond
  where
    walk cnd = case cnd of
      Var v   -> maybe (Var v) Lit (i v)
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
simplifyWithSysParams :: Condition ConfVar -> String -> String -> 
                         (Condition ConfVar, [String])
simplifyWithSysParams cond arch os = (cond', flags)
  where
    (cond', fvs) = simplifyCondition cond interp 
    interp (OS name)   = Just $ name == os
    interp (Arch name) = Just $ name == arch
    interp _           = Nothing
    flags = [ fname | Flag fname <- fvs ]

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
                      +++ archCond +++ flagCond)
    inparens   = between (ReadP.char '(' >> sp) (sp >> ReadP.char ')' >> sp)
    notCond  = ReadP.char '!' >> sp >> cond >>= return . CNot
    osCond   = string "os" >> sp >> inparens osIdent >>= return . Var. OS 
    archCond = string "arch" >> sp >> inparens archIdent >>= return . Var . Arch 
    flagCond = string "flag" >> sp >> inparens flagIdent >>= return . Var . Flag 
    ident    = munch1 isIdentChar >>= return . map toLower
    lit      = ((string "true" <++ string "True") >> return (Lit True)) <++ 
               ((string "false" <++ string "False") >> return (Lit False))
    archIdent     = ident >>= return 
    osIdent       = ident >>= return 
    flagIdent     = ident
    isIdentChar c = isAlphaNum c || (c `elem` "_-")
    oper s        = sp >> string s >> sp
    sp            = skipSpaces


------------------------------------------------------------------------------

data CondTree' v c a = CondNode 
    { condTreeData        :: a
    , condTreeConstraints :: [c]
    , condTreeComponents  :: [( Condition v
                              , CondTree' v c a
                              , Maybe (CondTree' v c a))]
    }



ppCondTree' :: Show v => CondTree' v c a -> (c -> Doc) -> Doc
ppCondTree' (CondNode dat cs ifs) ppD =
    (text "depends: " <+>
      (fsep $ punctuate (char ',') $ map ppD cs))
    $+$
    (vcat $ map ppIf ifs)
  where 
    ppIf (c,thenTree,mElseTree) = 
        ((text "if" <+> ppCond c <> colon) $$
          nest 2 (ppCondTree' thenTree ppD))
        $+$ (maybe empty (\t -> text "else: " $$ nest 2 (ppCondTree' t ppD))
                   mElseTree)


-- | A CondTree is the internal (normalized) representation of a specification
-- with (optional) conditional statements in it.  To get to the final value,
-- a sequence of conditions has to be evaluated completely, which then specifies
-- which path to take, until we reach a leaf.  
-- 
-- Each path is annotated with a number of constraints, which must be
-- fulfilled and a modifier, describing which information to add to the final
-- value.  We record constraints and data separately, to allow possible
-- optimizations later on.
--
-- Although the a CondTree is a tree, it can be constructed in a way to
-- preserve maximum sharing.  For example, in the following, the specification
-- on the left can be represented using the graph depicted on the right.
--
-- > if C1 {                                   C1
-- >   constraints: A,B                   A,B /  \
-- >   if C2 {                               /    \
-- >     constraints: D                     C2     \
-- >     data: 42                        D /  \ E   \
-- >   } else {                       42  /    | 3   |
-- >     constraints: E                  |     |     |
-- >     data: 3                          \    |    /
-- >   }                                   \   |   /
-- > }                                      \  |  /
-- > constraint: F                           v v v
-- > data: 1                                 F ; 1
-- 
data CondTree v c a = CondLeaf [c] (a -> a)
                    | Cond (Condition v) 
                           ([c], a -> a, CondTree v c a)
                           ([c], a -> a, CondTree v c a)
                    --deriving Show
instance (Show c, Show v) => Show (CondTree v c a) where
    show c = render $ ppCondTree c (text . show) []
      
-- Pretty print a cond tree.  The printed tree will be normalized to have all
-- it's data in the nodes, which results in a fair amount of duplication.  So,
-- ya be warned.
ppCondTree :: (Show v) => CondTree v c a -> (c -> Doc) -> [c] -> Doc
ppCondTree (CondLeaf ds _) ppD ds' = 
    text "build-depends:" <+> 
      (fsep $ punctuate (char ',') $ map ppD (ds' ++ ds))
ppCondTree (Cond c (d1s, _, ct1) (d2s, _, ct2)) ppD ds' =
            ((text "if" <+> ppCond c <> colon) $$ 
             nest 2 (ppCondTree ct1 ppD (d1s ++ ds')))
            $+$
            (text "else:" $$ nest 2 (ppCondTree ct2 ppD (d2s ++ ds')))
             
-- | Completely evaluate a CondTree with the given variable assignment.  The
--   environment must be defined for all free variables in all the conditions.
evalCond :: (v -> Maybe Bool) -> CondTree v d a -> ([d], a -> a)
evalCond _ (CondLeaf ds fs) = (ds, fs)
evalCond f (Cond cnd y n) = 
    case simplifyCondition cnd f of
      (Lit b, _) -> let (ds', fs', cnd') = if b then y else n 
                        (ds, fs) = evalCond f cnd' 
                    in (ds' ++ ds, fs' . fs)
      _ -> error $ "Environment not defined for all free vars" 

data DepTestRslt d = DepOk | MissingDeps [d] -- result of dependency test
data BT a = BTN a | BTB (BT a) (BT a)  -- very simple binary tree

-- | Search Condition graph for a combination that satisfies certain
-- dependencies.  Returns either the missing dependencies, or a tuple
-- containing the resulting data, the associated dependencies, and the chosen
-- flag assignments.
--
-- In case of failure, the _smallest_ number of of missing dependencies is
-- returned.
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
satisfyFlags :: [(String,[Bool])] 
                -- ^ Domain for each flag name, will be tested in order.
             -> String  -- ^ OS name, as returned System.Info.os
             -> String  -- ^ arch name, as returned System.Info.arch
             -> CondTree ConfVar d a    
             -> ([d] -> DepTestRslt d)  -- ^ Dependency test function.
             -> a                       -- ^ Empty result value.
             -> (Either [d] -- missing dependencies
                 (a, [d], [(String, Bool)]))
satisfyFlags dom os arch tree depsOk ini = 
    case try dom [] of
      Right r -> Right r
      Left dbt -> Left $ findShortest dbt
  where 
    -- @try@ recursively tries all possible flag assignments in the domain and
    -- either succeeds or returns a binary tree with the missing dependencies
    -- encountered in each run.  Since the tree is constructed lazily, we
    -- avoid some computation overhead in the successful case.
    try [] env = let (deps, dmod) = evalCond (f env) tree in
                 case depsOk deps of
                   DepOk -> Right (dmod ini, deps, env)
                   MissingDeps ds -> Left (BTN ds)
    try ((n, vals):rest) env = 
        tryAll $ map (\v -> try rest ((n, v):env)) vals

    tryAll = foldr mp mz

    -- special kind of `mplus' for our local purposes
    mp (Left xs)   (Left ys)   = (Left (BTB xs ys))
    mp (Left _)    m@(Right _) = m
    mp m@(Right _) _           = m

    -- `mzero'
    mz = Left (BTN [])

    f _ (OS o)     = Just $ o == os
    f _ (Arch a)   = Just $ a == arch
    f env (Flag n) = lookup n env

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

------------------------------------------------------------------------------
-- Testing

#ifdef DEBUG
test_satisfyFlags = satisfyFlags dom os arch tstTree check []
  where 
    dom = [("a",[True,False]),("b",[False,True]),("c",[True,False])]
    os = "house"
    arch = "i386"
    
    check xs = if all (`elem` avail) xs 
               then DepOk 
               else (MissingDeps (filter (not . (`elem` avail)) xs))
    avail = [1,5,4,0,42]

tstTree = Cond (CNot (Var (Flag "a"))) ([1], (1:), t1) ([2], (2:), t2)
  where
    t1 = Cond (CAnd (Var (Flag "b")) (Var (Flag "c"))) 
              ([3], (3:), t2) ([4], (4:), t2)
    t2 = CondLeaf [0,7,42] (0:)
{-
-- | A @Conditional@ guards a value of type @a@ with a condition.  The idea is
--   that the value is only accessible if the condition can be satisfied.
data Conditional c a = MkCond 
    { condCondition  :: Condition c
    , condValue      :: a
    } deriving Show

getValue :: Conditional c a
         -> (c -> Bool) -- ^ Truth assignment for all the values.  (Must be
                        --   defined for all free variables.)
         -> Maybe a
getValue c i = case eval c of
                 (Lit True, _) -> Just $ condValue c
                 _             -> Nothing
  where
    eval c = simplifyCondition (condCondition c) (Just . i)
-}

------------------------------------------------------------------------------
-- Testing

test_simplify = simplifyWithSysParams tstCond i386 darwin
  where 
    tstCond = COr (CAnd (Var (Arch ppc)) (Var (OS darwin)))
                  (CAnd (Var (Flag "debug")) (Var (OS darwin)))
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
                     ]

test_ppCondTree' = render $ ppCondTree' tstTree (text . show)
  where
    tstTree :: CondTree' ConfVar Int String
    tstTree = CondNode "A" [0] 
              [ (CNot (Var (Flag "a")), 
                 CondNode "B" [1] [],
                 Nothing)
              , (CAnd (Var (Flag "b")) (Var (Flag "c")),
                CondNode "C" [2] [],
                Just $ CondNode "D" [3] 
                         [ (Lit True,
                           CondNode "E" [4] [],
                           Just $ CondNode "F" [5] []) ])
                ]


#endif
