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

module UnitTest.Distribution.PackageDescription.Configuration where

import Distribution.PackageDescription.Configuration

import Distribution.Package (Package)
import Distribution.PackageDescription
         ( GenericPackageDescription(..), PackageDescription(..)
         , Library(..), Executable(..), BuildInfo(..)
         , Flag(..), CondTree(..), ConfVar(..), ConfFlag(..), Condition(..) )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version
    ( Version(..), Dependency(..), VersionRange(..)
    , withinRange, parseVersionRange )
import Distribution.Compiler (CompilerFlavor, parseCompilerFlavor)
import Distribution.System
         ( OS, readOS, Arch, readArch )
import Distribution.Simple.Utils (currentDir)

import Distribution.Compat.ReadP as ReadP hiding ( char )
import qualified Distribution.Compat.ReadP as ReadP ( char )

import Data.Char ( isAlphaNum, toLower )
import Data.Maybe ( catMaybes, maybeToList )
import Data.List  ( nub )
import Data.Monoid

import Data.List ( (\\) )
import Distribution.ParseUtils

------------------------------------------------------------------------------
-- Testing

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
