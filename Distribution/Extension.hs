-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Extension
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Haskell language extensions

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

module Distribution.Extension (
	Extension(..), Opt,
	extensionsToNHCFlag, extensionsToGHCFlag, extensionsToHugsFlag,
#ifdef DEBUG        
        hunitTests
#endif
  ) where

import Data.List(nub)

#ifdef DEBUG
import HUnit (Test)
#endif

-- ------------------------------------------------------------
-- * Extension
-- ------------------------------------------------------------

-- |This represents non-standard compiler extensions which each
-- package might employ.

data Extension = 
	       OverlappingInstances
               | RecursiveDo
               | ParallelListComp
               | MultiParamTypeClasses
               | NoMonomorphismRestriction
               | FunctionalDependencies
               | RankNTypes
               | PolymorphicComponents
               | ExistentialQuantification
               | ScopedTypeVariables
               | ImplicitParams
               | FlexibleContexts
               | FlexibleInstances
               | EmptyDataDecls
               | CPP

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
               | NamedFieldPuns

               | ExtensibleRecords
               | RestrictedTypeSynonyms
               | HereDocuments
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
    extensionToGHCFlag AllowIncoherentInstances     = Right "-fallow-incoherent-instances"
    extensionToGHCFlag InlinePhase                  = Right "-finline-phase"
    extensionToGHCFlag ContextStack                 = Right "-fcontext-stack"
    extensionToGHCFlag Arrows                       = Right "-farrows"
    extensionToGHCFlag Generics                     = Right "-fgenerics"
    extensionToGHCFlag NoImplicitPrelude            = Right "-fno-implicit-prelude"
    extensionToGHCFlag ImplicitParams               = Right "-fimplicit-params"
    extensionToGHCFlag CPP                          = Right "-cpp"

    extensionToGHCFlag RecursiveDo                  = Right "-fglasgow-exts"
    extensionToGHCFlag ParallelListComp             = Right "-fglasgow-exts"
    extensionToGHCFlag MultiParamTypeClasses        = Right "-fglasgow-exts"
    extensionToGHCFlag FunctionalDependencies       = Right "-fglasgow-exts"
    extensionToGHCFlag RankNTypes                   = Right "-fglasgow-exts"
    extensionToGHCFlag PolymorphicComponents        = Right "-fglasgow-exts"
    extensionToGHCFlag ExistentialQuantification    = Right "-fglasgow-exts"
    extensionToGHCFlag ScopedTypeVariables          = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleContexts             = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleInstances            = Right "-fglasgow-exts"
    extensionToGHCFlag EmptyDataDecls               = Right "-fglasgow-exts"

    extensionToGHCFlag e@ExtensibleRecords          = Left e
    extensionToGHCFlag e@RestrictedTypeSynonyms     = Left e
    extensionToGHCFlag e@HereDocuments              = Left e
    extensionToGHCFlag e@UnsafeOverlappingInstances = Left e
    extensionToGHCFlag e@NamedFieldPuns             = Left e

-- |NHC: Return the unsupported extensions, and the flags for the supported extensions
extensionsToNHCFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToNHCFlag l
    = splitEither $ nub $ map extensionToNHCFlag l
      where
      -- NHC doesn't enforce the monomorphism restriction at all.
      extensionToNHCFlag NoMonomorphismRestriction = Right ""
      extensionToNHCFlag ForeignFunctionInterface  = Right ""
      extensionToNHCFlag ExistentialQuantification = Right ""
      extensionToNHCFlag EmptyDataDecls            = Right ""
      extensionToNHCFlag NamedFieldPuns            = Right "-puns"
      extensionToNHCFlag CPP                       = Right "-cpp"
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
      extensionToHugsFlag RankNTypes                 = Right "-98"
      extensionToHugsFlag PolymorphicComponents      = Right "-98"
      extensionToHugsFlag ExistentialQuantification  = Right "-98"
      extensionToHugsFlag ScopedTypeVariables        = Right "-98"
      extensionToHugsFlag ImplicitParams             = Right "-98"
      extensionToHugsFlag ExtensibleRecords          = Right "-98"
      extensionToHugsFlag RestrictedTypeSynonyms     = Right "-98"
      extensionToHugsFlag FlexibleContexts           = Right "-98"
      extensionToHugsFlag FlexibleInstances          = Right "-98"
      extensionToHugsFlag ForeignFunctionInterface   = Right ""
      extensionToHugsFlag EmptyDataDecls             = Right ""
      extensionToHugsFlag CPP                        = Right ""
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
