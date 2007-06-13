-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Configuration
-- Copyright   :  Thomas Schilling 2007
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

module Distribution.Configuration where

import Distribution.ParseUtils
import Distribution.Compat.ReadP as ReadP 
import Distribution.Version

import Data.Char ( isAlphaNum, toLower )

data FlagValue = FlUnknown | FlTrue | FlFalse
               deriving (Eq, Show)

data Flag = MkFlag
    { flagName        :: String
    , flagDescription :: String
    , flagDefault     :: FlagValue
    } deriving Show

data ConfVar = OS OSName
             | Arch ArchName
             | Flag String
             -- | Depend [Dependency]
               deriving Show

data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
                   deriving Show

-- | The name of an operating system.  Abstract, so we can implement dedicated
--   operations on them if needed.
data OSName = MkOSName String 
              deriving (Show, Eq)

mkOSName :: String -> OSName
mkOSName = MkOSName

-- | The name of an architecture.  Abstract.
data ArchName = MkArchName String
                deriving (Show, Eq)

mkArchName :: String -> ArchName
mkArchName = MkArchName

-- | Simplify the condition and return its free variables.
simplifyCondition :: Condition c
                  -> (c -> Maybe Bool)   -- ^ (partial) variable assignment
                  -> (Condition c, [c])
simplifyCondition cond i = fv . walk $ cond
  where
    walk c = case c of
      Var v   -> maybe (Var v) Lit (i v)
      Lit b   -> Lit b
      CNot c  -> case walk c of
                   Lit True -> Lit False
                   Lit False -> Lit True
                   c' -> CNot c'
      COr c d -> case (walk c, walk d) of
                   (Lit False, d') -> d'
                   (Lit True, d')  -> Lit True
                   (c', Lit False) -> c'
                   (c', Lit True)  -> Lit True
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
      Lit b      -> []
      CNot c'    -> fv' c'
      COr c1 c2  -> fv' c1 ++ fv' c2
      CAnd c1 c2 -> fv' c1 ++ fv' c2

simplifyWithSysParams :: Condition ConfVar -> ArchName -> OSName -> 
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

parseCondition :: ReadP r (Condition ConfVar)
parseCondition = condOr
  where
    condOr   = sepBy1 condAnd (oper "||") >>= return . foldl1 COr
    condAnd  = sepBy1 cond (oper "&&")>>= return . foldl1 CAnd
    cond     = sp >> (lit +++ parens condOr +++ notCond +++ osCond 
                      +++ archCond +++ flagCond)
    parens   = between (char '(' >> sp) (sp >> char ')' >> sp)
    notCond  = char '!' >> sp >> cond >>= return . CNot
    osCond   = string "os" >> sp >> parens osIdent >>= return . Var. OS 
    archCond = string "arch" >> sp >> parens archIdent >>= return . Var . Arch 
    flagCond = string "flag" >> sp >> parens flagIdent >>= return . Var . Flag 
    ident    = munch1 isIdentChar >>= return . map toLower
    lit      = ((string "true" <++ string "True") >> return (Lit True)) <++ 
               ((string "false" <++ string "False") >> return (Lit False))
    archIdent     = ident >>= return . mkArchName
    osIdent       = ident >>= return . mkOSName
    flagIdent     = ident
    isIdentChar c = isAlphaNum c || (c `elem` "_-")
    oper s        = sp >> string s >> sp
    sp            = skipSpaces


data CondTree v c a = CondLeaf [c] [a]
                    | Cond (Condition v) 
                           ([c], [a], CondTree v c a)
                           ([c], [a], CondTree v c a)
                    deriving Show

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
    [ppc,i386] = map (mkArchName) ["ppc","i386"]
    [darwin,windows] = map (mkOSName) ["darwin","windows"]



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