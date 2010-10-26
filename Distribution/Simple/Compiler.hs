-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Compiler
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This should be a much more sophisticated abstraction than it is. Currently
-- it's just a bit of data about the compiler, like it's flavour and name and
-- version. The reason it's just data is because currently it has to be in
-- 'Read' and 'Show' so it can be saved along with the 'LocalBuildInfo'. The
-- only interesting bit of info it contains is a mapping between language
-- extensions and compiler command line flags. This module also defines a
-- 'PackageDB' type which is used to refer to package databases. Most compilers
-- only know about a single global package collection but GHC has a global and
-- per-user one and it lets you create arbitrary other package databases. We do
-- not yet fully support this latter feature.

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

module Distribution.Simple.Compiler (
        -- * Haskell implementations
        module Distribution.Compiler,
        Compiler(..),
        showCompilerId, compilerFlavor, compilerVersion,

        -- * Support for package databases
        PackageDB(..),
        PackageDBStack,
        registrationPackageDB,

        -- * Support for optimisation levels
        OptimisationLevel(..),
        flagToOptimisationLevel,

        -- * Support for language extensions
        Flag,
        languageToFlags,
        unsupportedLanguages,
        extensionsToFlags,
        unsupportedExtensions
  ) where

import Distribution.Compiler
import Distribution.Version (Version(..))
import Distribution.Text (display)
import Language.Haskell.Extension (Language(Haskell98), Extension)

import Data.List (nub)
import Data.Maybe (catMaybes, isNothing)

data Compiler = Compiler {
        compilerId              :: CompilerId,
        compilerLanguages       :: [(Language, Flag)],
        compilerExtensions      :: [(Extension, Flag)]
    }
    deriving (Show, Read)

showCompilerId :: Compiler -> String
showCompilerId = display . compilerId

compilerFlavor ::  Compiler -> CompilerFlavor
compilerFlavor = (\(CompilerId f _) -> f) . compilerId

compilerVersion :: Compiler -> Version
compilerVersion = (\(CompilerId _ v) -> v) . compilerId

-- ------------------------------------------------------------
-- * Package databases
-- ------------------------------------------------------------

-- |Some compilers have a notion of a database of available packages.
-- For some there is just one global db of packages, other compilers
-- support a per-user or an arbitrary db specified at some location in
-- the file system. This can be used to build isloated environments of
-- packages, for example to build a collection of related packages
-- without installing them globally.
--
data PackageDB = GlobalPackageDB
               | UserPackageDB
               | SpecificPackageDB FilePath
    deriving (Eq, Ord, Show, Read)

-- | We typically get packages from several databases, and stack them
-- together. This type lets us be explicit about that stacking. For example
-- typical stacks include:
--
-- > [GlobalPackageDB]
-- > [GlobalPackageDB, UserPackageDB]
-- > [GlobalPackageDB, SpecificPackageDB "package.conf.inplace"]
--
-- Note that the 'GlobalPackageDB' is invariably at the bottom since it
-- contains the rts, base and other special compiler-specific packages.
--
-- We are not restricted to using just the above combinations. In particular
-- we can use several custom package dbs and the user package db together.
--
-- When it comes to writing, the top most (last) package is used.
--
type PackageDBStack = [PackageDB]

-- | Return the package that we should register into. This is the package db at
-- the top of the stack.
--
registrationPackageDB :: PackageDBStack -> PackageDB
registrationPackageDB []  = error "internal error: empty package db set"
registrationPackageDB dbs = last dbs

-- ------------------------------------------------------------
-- * Optimisation levels
-- ------------------------------------------------------------

-- | Some compilers support optimising. Some have different levels.
-- For compliers that do not the level is just capped to the level
-- they do support.
--
data OptimisationLevel = NoOptimisation
                       | NormalOptimisation
                       | MaximumOptimisation
    deriving (Eq, Show, Read, Enum, Bounded)

flagToOptimisationLevel :: Maybe String -> OptimisationLevel
flagToOptimisationLevel Nothing  = NormalOptimisation
flagToOptimisationLevel (Just s) = case reads s of
  [(i, "")]
    | i >= fromEnum (minBound :: OptimisationLevel)
   && i <= fromEnum (maxBound :: OptimisationLevel)
                -> toEnum i
    | otherwise -> error $ "Bad optimisation level: " ++ show i
                        ++ ". Valid values are 0..2"
  _             -> error $ "Can't parse optimisation level " ++ s

-- ------------------------------------------------------------
-- * Languages and Extensions
-- ------------------------------------------------------------

unsupportedLanguages :: Compiler -> [Language] -> [Language]
unsupportedLanguages comp langs =
  [ lang | lang <- langs
         , isNothing (languageToFlag comp lang) ]

languageToFlags :: Compiler -> Maybe Language -> [Flag]
languageToFlags comp = filter (not . null)
                     . catMaybes . map (languageToFlag comp)
                     . maybe [Haskell98] (\x->[x])

languageToFlag :: Compiler -> Language -> Maybe Flag
languageToFlag comp ext = lookup ext (compilerLanguages comp)


-- |For the given compiler, return the extensions it does not support.
unsupportedExtensions :: Compiler -> [Extension] -> [Extension]
unsupportedExtensions comp exts =
  [ ext | ext <- exts
        , isNothing (extensionToFlag comp ext) ]

type Flag = String

-- |For the given compiler, return the flags for the supported extensions.
extensionsToFlags :: Compiler -> [Extension] -> [Flag]
extensionsToFlags comp = nub . filter (not . null)
                       . catMaybes . map (extensionToFlag comp)

extensionToFlag :: Compiler -> Extension -> Maybe Flag
extensionToFlag comp ext = lookup ext (compilerExtensions comp)
