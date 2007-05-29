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

import Data.Char ( isAlphaNum, toLower )

data FlagValue = FLUnknown | FlTrue | FlFalse
               deriving (Eq, Show)

data Flag = MkFlag
    { flagName        :: String
    , flagDescription :: String
    , flagDefault     :: FlagValue
    }

data Condition = OS String
               | Arch String
               | Flag String
               | Lit Bool
               | CNot Condition
               | COr Condition Condition
               | CAnd Condition Condition
               deriving Show

-- | Simplify the condition and return its free flag-variables.
simplifyCondition :: Condition 
                  -> String -- ^ Arch identifier
                  -> String -- ^ OS identifier
                  -> (Condition, [String])
simplifyCondition cond arch os = fv . walk $ cond
  where
    -- reduce arch(..) and os(..) to boolean and simplify tree
    walk c = case c of
      OS name -> if os == name then Lit True else Lit False
      Arch a  -> if a == arch then Lit True else Lit False
      Flag f  -> Flag f
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
    -- gather free flags
    fv c = (c, fv' c)
    fv' c = case c of
      Flag f     -> [f]
      Lit b      -> []
      CNot c'    -> fv' c'
      COr c1 c2  -> fv' c1 ++ fv' c2
      CAnd c1 c2 -> fv' c1 ++ fv' c2



parseCondition :: ReadP r Condition
parseCondition = condOr
  where
    condOr   = sepBy1 condAnd (oper "||") >>= return . foldl1 COr
    condAnd  = sepBy1 cond (oper "&&")>>= return . foldl1 CAnd
    cond     = sp >> (lit +++ parens condOr +++ notCond +++ osCond 
                      +++ archCond +++ flagCond)
    parens   = between (char '(' >> sp) (sp >> char ')' >> sp)
    notCond  = char '!' >> sp >> cond >>= return . CNot
    osCond   = string "os" >> sp >> parens osIdent >>= return . OS
    archCond = string "arch" >> sp >> parens archIdent >>= return . Arch 
    flagCond = string "flag" >> sp >> parens flagIdent >>= return . Flag 
    ident    = munch1 isIdentChar >>= return . map toLower
    lit      = ((string "true" <++ string "True") >> return (Lit True)) <++ 
               ((string "false" <++ string "False") >> return (Lit False))
    archIdent     = ident
    osIdent       = ident
    flagIdent     = ident
    isIdentChar c = isAlphaNum c || (c `elem` "_-")
    oper s        = sp >> string s >> sp
    sp            = skipSpaces

------------------------------------------------------------------------------
-- Testing

test_simplifyCondition = simplifyCondition tstCond "ia32" "darwin"
  where 
    tstCond = COr (CAnd (Arch "ppc") (OS "darwin"))
                  (CAnd (Flag "debug") (OS "darwin"))

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