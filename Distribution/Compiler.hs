{-# OPTIONS -cpp #-}
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
	CompilerFlavor(..), Compiler(..),
        showCompilerId, compilerVersion,
	compilerPath, compilerPkgToolPath,
        -- * Support for language extensions
        Opt,
        extensionsToFlags,
        extensionsToGHCFlag, extensionsToHugsFlag,
        extensionsToNHCFlag, extensionsToJHCFlag,
#ifdef DEBUG
        hunitTests
#endif
  ) where

import Distribution.Version (Version(..))
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Language.Haskell.Extension (Extension(..))
import Distribution.Program

import Data.List (nub)

#ifdef DEBUG
import Test.HUnit (Test)
#endif

-- ------------------------------------------------------------
-- * Command Line Types and Exports
-- ------------------------------------------------------------

data CompilerFlavor
  = GHC | NHC | Hugs | HBC | Helium | JHC | OtherCompiler String
              deriving (Show, Read, Eq, Ord)

data Compiler = Compiler {
        compilerFlavor  :: CompilerFlavor,
        compilerId      :: PackageIdentifier,
        compilerProg    :: Program,
        compilerPkgTool :: Program
    }
    deriving (Show, Read)

showCompilerId :: Compiler -> String
showCompilerId = showPackageId . compilerId

compilerVersion :: Compiler -> Version
compilerVersion = pkgVersion . compilerId

compilerPath :: Compiler -> FilePath
compilerPath = programPath . compilerProg

compilerPkgToolPath :: Compiler -> FilePath
compilerPkgToolPath = programPath . compilerPkgTool

-- ------------------------------------------------------------
-- * Extensions
-- ------------------------------------------------------------

-- |For the given compiler, return the unsupported extensions, and the
-- flags for the supported extensions.
extensionsToFlags :: CompilerFlavor -> Version -> [ Extension ]
                  -> ([Extension], [Opt])
extensionsToFlags GHC  v exts = extensionsToGHCFlag  v exts
extensionsToFlags Hugs _ exts = extensionsToHugsFlag   exts
extensionsToFlags NHC  _ exts = extensionsToNHCFlag    exts
extensionsToFlags JHC  _ exts = extensionsToJHCFlag    exts
extensionsToFlags _    _ exts = (exts, [])

-- |GHC: Return the unsupported extensions, and the flags for the supported extensions
extensionsToGHCFlag :: Version -> [ Extension ] -> ([Extension], [Opt])
extensionsToGHCFlag v l
    = splitEither $ nub $ map extensionToGHCFlag l
    where
    v6_7 = Version { versionBranch = [6, 7], versionTags = [] }
    extensionToGHCFlag :: Extension -> Either Extension String
    extensionToGHCFlag OverlappingInstances
     | v >= v6_7 = Right "-XOverlappingInstances"
     | otherwise = Right "-fallow-overlapping-instances"
    extensionToGHCFlag TypeSynonymInstances
     | v >= v6_7 = Right "-XTypeSynonymInstances"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag TemplateHaskell
     | v >= v6_7 = Right "-XTemplateHaskell"
     | otherwise = Right "-fth"
    extensionToGHCFlag ForeignFunctionInterface
     | v >= v6_7 = Right "-XForeignFunctionInterface"
     | otherwise = Right "-fffi"
    extensionToGHCFlag NoMonomorphismRestriction
     | v >= v6_7 = Right "-XNoMonomorphismRestriction"
     | otherwise = Right "-fno-monomorphism-restriction"
    extensionToGHCFlag UndecidableInstances
     | v >= v6_7 = Right "-XUndecidableInstances"
     | otherwise = Right "-fallow-undecidable-instances"
    extensionToGHCFlag IncoherentInstances
     | v >= v6_7 = Right "-XIncoherentInstances"
     | otherwise = Right "-fallow-incoherent-instances"
    extensionToGHCFlag Arrows
     | v >= v6_7 = Right "-XArrows"
     | otherwise = Right "-farrows"
    extensionToGHCFlag Generics
     | v >= v6_7 = Right "-XGenerics"
     | otherwise = Right "-fgenerics"
    extensionToGHCFlag NoImplicitPrelude
     | v >= v6_7 = Right "-XNoImplicitPrelude"
     | otherwise = Right "-fno-implicit-prelude"
    extensionToGHCFlag ImplicitParams
     | v >= v6_7 = Right "-XImplicitParams"
     | otherwise = Right "-fimplicit-params"
    extensionToGHCFlag CPP
     | v >= v6_7 = Right "-XCPP"
     | otherwise = Right "-cpp"

    extensionToGHCFlag BangPatterns
     | v >= v6_7 = Right "-XBangPatterns"
     | otherwise = Right "-fbang-patterns"
    extensionToGHCFlag KindSignatures
     | v >= v6_7 = Right "-XKindSignatures"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag RecursiveDo
     | v >= v6_7 = Right "-XRecursiveDo"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag ParallelListComp
     | v >= v6_7 = Right "-XParallelListComp"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag MultiParamTypeClasses
     | v >= v6_7 = Right "-XMultiParamTypeClasses"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag FunctionalDependencies
     | v >= v6_7 = Right "-XFunctionalDependencies"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag Rank2Types
     | v >= v6_7 = Right "-XRank2Types"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag RankNTypes
     | v >= v6_7 = Right "-XRankNTypes"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag PolymorphicComponents
     | v >= v6_7 = Right "-XPolymorphicComponents"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag ExistentialQuantification
     | v >= v6_7 = Right "-XExistentialQuantification"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag ScopedTypeVariables
     | v >= v6_7 = Right "-XScopedTypeVariables"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleContexts
     | v >= v6_7 = Right "-XFlexibleContexts"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag FlexibleInstances
     | v >= v6_7 = Right "-XFlexibleInstances"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag EmptyDataDecls
     | v >= v6_7 = Right "-XEmptyDataDecls"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag PatternGuards
     | v >= v6_7 = Right "-XPatternGuards"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag GeneralizedNewtypeDeriving
     | v >= v6_7 = Right "-XGeneralizedNewtypeDeriving"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag MagicHash
     | v >= v6_7 = Right "-XMagicHash"
     | otherwise = Right "-fglasgow-exts"
    extensionToGHCFlag e@TypeFamilies
     | v >= v6_7 = Right "-XTypeFamilies"
     | otherwise = Left e

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
