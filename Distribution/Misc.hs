{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Misc
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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

module Distribution.Misc(License(..), Dependency(..), Extension(..), Opt
                         ,extensionsToNHCFlag, extensionsToGHCFlag
#ifdef DEBUG        
        ,hunitTests
#endif
                        )
    where

import Distribution.Version(VersionRange)
import Data.List(nub)

#ifdef DEBUG
import HUnit (Test)
#endif

-- ------------------------------------------------------------
-- * Misc
-- ------------------------------------------------------------

data License = GPL | LGPL | BSD3 | BSD4 | PublicDomain | AllRightsReserved
             | {- ... | -} OtherLicense FilePath
               deriving (Read, Show, Eq)

-- |Maybe move to Distribution.Version?
data Dependency = Dependency String VersionRange
                  deriving (Read, Show, Eq)

-- |This represents non-standard compiler extensions which each
-- package might employ.

data Extension = 
	       OverlappingInstances
               | RecursiveDo
               | ParallelListComp
               | MultiParamTypeClasses
               | NoMonomorphismRestriction
               | FunctionalDependencies
               | RankTwoTypes
               | PolymorphicComponents
               | ExistentialQuantification
               | PatternTypeAnnotations
               | ImplicitParams
               | FlexibleContexts
               | FlexibleInstances

	       | TypeSynonymInstances
	       | TemplateHaskell
               | ForeignFunctionInterface
               | AllowOverlappingInstances
               | AllowUndecidableInstances
               | AllowIncoherentInstances
               | InlinePhase
               | ContextStack
               | Arrows
               | Generics
               | NoImplicitPrelude

               | ExtensibleRecords
               | RestrictedTypeSynonyms
               | HereDocuments
               | HoodDebugging
               | UnsafeOverlappingInstances
	       deriving (Show, Read, Eq)

-- |GHC: Return the unsupported extensions, and the flags for the supported extensions
extensionsToGHCFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToGHCFlag l
    = splitEither $ nub $ map extensionToGHCFlag l
    where
    extensionToGHCFlag :: Extension -> Either Extension String
    extensionToGHCFlag OverlappingInstances         = Right "-fallow-overlapping-instances"
    extensionToGHCFlag TypeSynonymInstances         = Right "-fglasgow-exts"
    extensionToGHCFlag TemplateHaskell              = Right "-fth"
    extensionToGHCFlag ForeignFunctionInterface     = Right "-ffi"
    extensionToGHCFlag NoMonomorphismRestriction    = Right "-fno-monomorphism-restriction"
    extensionToGHCFlag AllowOverlappingInstances    = Right "-fallow-overlapping-instances"
    extensionToGHCFlag AllowUndecidableInstances    = Right "-fallow-undecidable-instances"
    extensionToGHCFlag AllowIncoherentInstances     = Right "-fallow- incoherent-instances"
    extensionToGHCFlag InlinePhase                  = Right "-finline-phase"
    extensionToGHCFlag ContextStack                 = Right "-fcontext-stack"
    extensionToGHCFlag Arrows                       = Right "-farrows"
    extensionToGHCFlag Generics                     = Right "-fgenerics"
    extensionToGHCFlag NoImplicitPrelude            = Right "-fno-implicit-prelude"
    extensionToGHCFlag ImplicitParams               = Right "-fimplicit-params"

    extensionToGHCFlag RecursiveDo                  = Right "-fglasgow-exts"
    extensionToGHCFlag ParallelListComp             = Right "-fglasgow-exts"
    extensionToGHCFlag MultiParamTypeClasses        = Right "-fglasgow-exts"
    extensionToGHCFlag FunctionalDependencies       = Right "-fglasgow-exts"
    extensionToGHCFlag RankTwoTypes                 = Right "-fglasgow-exts"
    extensionToGHCFlag PolymorphicComponents        = Right "-fglasgow-exts"
    extensionToGHCFlag ExistentialQuantification    = Right "-fglasgow-exts"
    extensionToGHCFlag PatternTypeAnnotations       = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleContexts             = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleInstances            = Right "-fglasgow-exts"

    extensionToGHCFlag e@ExtensibleRecords          = Left e
    extensionToGHCFlag e@RestrictedTypeSynonyms     = Left e
    extensionToGHCFlag e@HereDocuments              = Left e
    extensionToGHCFlag e@HoodDebugging              = Left e
    extensionToGHCFlag e@UnsafeOverlappingInstances = Left e

-- |NHC: Return the unsupported extensions, and the flags for the supported extensions
extensionsToNHCFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToNHCFlag l
    = splitEither $ nub $ map extensionToNHCFlag l
      where
      extensionToNHCFlag NoMonomorphismRestriction = Right "" -- not implemented in NHC
      extensionToNHCFlag ForeignFunctionInterface  = Right ""
      extensionToNHCFlag HoodDebugging             = Right ""
      extensionToNHCFlag e                         = Left e

-- |Hugs: Return the unsupported extensions, and the flags for the supported extensions
extensionsToHugsFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToHugsFlag l
    = splitEither $ nub $ map extensionToHugsFlag l
      where
      extensionToHugsFlag OverlappingInstances       = Right "+o"
      extensionToHugsFlag UnsafeOverlappingInstances = Right "+O"
      extensionToHugsFlag HereDocuments              = Right "+H"
      extensionToHugsFlag RecursiveDo                = Right "-98"
      extensionToHugsFlag ParallelListComp           = Right "-98"
      extensionToHugsFlag MultiParamTypeClasses      = Right "-98"
      extensionToHugsFlag FunctionalDependencies     = Right "-98"
      extensionToHugsFlag RankTwoTypes               = Right "-98"
      extensionToHugsFlag PolymorphicComponents      = Right "-98"
      extensionToHugsFlag ExistentialQuantification  = Right "-98"
      extensionToHugsFlag PatternTypeAnnotations     = Right "-98"
      extensionToHugsFlag ImplicitParams             = Right "-98"
      extensionToHugsFlag ExtensibleRecords          = Right "-98"
      extensionToHugsFlag RestrictedTypeSynonyms     = Right "-98"
      extensionToHugsFlag HoodDebugging              = Right "-98"
      extensionToHugsFlag FlexibleContexts           = Right "-98"
      extensionToHugsFlag FlexibleInstances          = Right "-98"
      extensionToHugsFlag e                          = Left e

splitEither :: [Either a b] -> ([a], [b])
splitEither l = ([a | Left a <- l], [b | Right b <- l])

type Opt = String

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
