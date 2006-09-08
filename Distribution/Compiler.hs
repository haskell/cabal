{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compiler
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Haskell implementations.

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

module Distribution.Compiler (
        -- * Haskell implementations
	CompilerFlavor(..), Compiler(..), showCompilerId,
	compilerBinaryName,
        -- * Support for language extensions
        Opt,
        extensionsToFlags,
        extensionsToGHCFlag, extensionsToHugsFlag,
        extensionsToNHCFlag, extensionsToJHCFlag,
#ifdef DEBUG
        hunitTests
#endif
  ) where

import Distribution.Version (Version(..), showVersion)
import Language.Haskell.Extension (Extension(..))

import Data.List (nub)

#ifdef DEBUG
import HUnit (Test)
#endif

-- ------------------------------------------------------------
-- * Command Line Types and Exports
-- ------------------------------------------------------------

data CompilerFlavor
  = GHC | NHC | Hugs | HBC | Helium | JHC | OtherCompiler String
              deriving (Show, Read, Eq)

data Compiler = Compiler {compilerFlavor:: CompilerFlavor,
			  compilerVersion :: Version,
                          compilerPath  :: FilePath,
                          compilerPkgTool :: FilePath}
                deriving (Show, Read, Eq)

showCompilerId :: Compiler -> String
showCompilerId (Compiler f (Version [] _) _ _) = compilerBinaryName f
showCompilerId (Compiler f v _ _) = compilerBinaryName f ++ '-': showVersion v

compilerBinaryName :: CompilerFlavor -> String
compilerBinaryName GHC  = "ghc"
compilerBinaryName NHC  = "hmake" -- FIX: uses hmake for now
compilerBinaryName Hugs = "ffihugs"
compilerBinaryName JHC  = "jhc"
compilerBinaryName cmp  = error $ "Unsupported compiler: " ++ (show cmp)

-- ------------------------------------------------------------
-- * Extensions
-- ------------------------------------------------------------

-- |For the given compiler, return the unsupported extensions, and the
-- flags for the supported extensions.
extensionsToFlags :: CompilerFlavor -> [ Extension ] -> ([Extension], [Opt])
extensionsToFlags GHC exts = extensionsToGHCFlag exts
extensionsToFlags Hugs exts = extensionsToHugsFlag exts
extensionsToFlags NHC exts = extensionsToNHCFlag exts
extensionsToFlags JHC exts = extensionsToJHCFlag exts
extensionsToFlags _ exts = (exts, [])

-- |GHC: Return the unsupported extensions, and the flags for the supported extensions
extensionsToGHCFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToGHCFlag l
    = splitEither $ nub $ map extensionToGHCFlag l
    where
    extensionToGHCFlag :: Extension -> Either Extension String
    extensionToGHCFlag OverlappingInstances         = Right "-fallow-overlapping-instances"
    extensionToGHCFlag TypeSynonymInstances         = Right "-fglasgow-exts"
    extensionToGHCFlag TemplateHaskell              = Right "-fth"
    extensionToGHCFlag ForeignFunctionInterface     = Right "-fffi"
    extensionToGHCFlag NoMonomorphismRestriction    = Right "-fno-monomorphism-restriction"
    extensionToGHCFlag UndecidableInstances         = Right "-fallow-undecidable-instances"
    extensionToGHCFlag IncoherentInstances          = Right "-fallow-incoherent-instances"
    extensionToGHCFlag InlinePhase                  = Right "-finline-phase"
    extensionToGHCFlag ContextStack                 = Right "-fcontext-stack"
    extensionToGHCFlag Arrows                       = Right "-farrows"
    extensionToGHCFlag Generics                     = Right "-fgenerics"
    extensionToGHCFlag NoImplicitPrelude            = Right "-fno-implicit-prelude"
    extensionToGHCFlag ImplicitParams               = Right "-fimplicit-params"
    extensionToGHCFlag CPP                          = Right "-cpp"

    extensionToGHCFlag BangPatterns                 = Right "-fbang-patterns"
    extensionToGHCFlag RecursiveDo                  = Right "-fglasgow-exts"
    extensionToGHCFlag ParallelListComp             = Right "-fglasgow-exts"
    extensionToGHCFlag MultiParamTypeClasses        = Right "-fglasgow-exts"
    extensionToGHCFlag FunctionalDependencies       = Right "-fglasgow-exts"
    extensionToGHCFlag Rank2Types                   = Right "-fglasgow-exts"
    extensionToGHCFlag RankNTypes                   = Right "-fglasgow-exts"
    extensionToGHCFlag PolymorphicComponents        = Right "-fglasgow-exts"
    extensionToGHCFlag ExistentialQuantification    = Right "-fglasgow-exts"
    extensionToGHCFlag ScopedTypeVariables          = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleContexts             = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleInstances            = Right "-fglasgow-exts"
    extensionToGHCFlag EmptyDataDecls               = Right "-fglasgow-exts"
    extensionToGHCFlag PatternGuards                = Right "-fglasgow-exts"
    extensionToGHCFlag GeneralizedNewtypeDeriving   = Right "-fglasgow-exts"

    extensionToGHCFlag e@ExtensibleRecords          = Left e
    extensionToGHCFlag e@RestrictedTypeSynonyms     = Left e
    extensionToGHCFlag e@HereDocuments              = Left e
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

-- |JHC: Return the unsupported extensions, and the flags for the supported extensions
extensionsToJHCFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToJHCFlag l = (es, filter (not . null) rs)
      where
      (es,rs) = splitEither $ nub $ map extensionToJHCFlag l
      extensionToJHCFlag TypeSynonymInstances       = Right ""
      extensionToJHCFlag ForeignFunctionInterface   = Right ""
      extensionToJHCFlag NoImplicitPrelude          = Right "--noprelude"
      extensionToJHCFlag CPP                        = Right "-fcpp"
      extensionToJHCFlag e                          = Left e

-- |Hugs: Return the unsupported extensions, and the flags for the supported extensions
extensionsToHugsFlag :: [ Extension ] -> ([Extension], [Opt])
extensionsToHugsFlag l
    = splitEither $ nub $ map extensionToHugsFlag l
      where
      extensionToHugsFlag OverlappingInstances       = Right "+o"
      extensionToHugsFlag IncoherentInstances        = Right "+oO"
      extensionToHugsFlag HereDocuments              = Right "+H"
      extensionToHugsFlag TypeSynonymInstances       = Right "-98"
      extensionToHugsFlag RecursiveDo                = Right "-98"
      extensionToHugsFlag ParallelListComp           = Right "-98"
      extensionToHugsFlag MultiParamTypeClasses      = Right "-98"
      extensionToHugsFlag FunctionalDependencies     = Right "-98"
      extensionToHugsFlag Rank2Types                 = Right "-98"
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
