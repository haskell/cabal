-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Check
-- Copyright   :  Lennart Kolmodin 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This has code for checking for various problems in packages. There is one
-- set of checks that just looks at a 'PackageDescription' in isolation and
-- another set of checks that also looks at files in the package. Some of the
-- checks are basic sanity checks, others are portability standards that we'd
-- like to encourage. There is a 'PackageCheck' type that distinguishes the
-- different kinds of check so we can see which ones are appropriate to report
-- in different situations. This code gets uses when configuring a package when
-- we consider only basic problems. The higher standard is uses when when
-- preparing a source tarball and by hackage when uploading new packages. The
-- reason for this is that we want to hold packages that are expected to be
-- distributed to a higher standard than packages that are only ever expected
-- to be used on the author's own environment.

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

module Distribution.PackageDescription.Check (
        -- * Package Checking
        PackageCheck(..),
        checkPackage,
        checkConfiguredPackage,

        -- ** Checking package contents
        checkPackageFiles,
        checkPackageContent,
        CheckPackageContentOps(..),
        checkPackageFileNames,
  ) where

import Data.Maybe (isNothing, catMaybes, fromMaybe)
import Data.List  (sort, group, isPrefixOf)
import Control.Monad
         ( filterM, liftM )
import qualified System.Directory as System
         ( doesFileExist, doesDirectoryExist )

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Compiler
         ( CompilerFlavor(..) )
import Distribution.System
         ( OS(..), Arch(..) )
import Distribution.License
         ( License(..) )
import Distribution.Simple.Utils
         ( cabalVersion, intercalate )

import Distribution.Version
         ( Version(..), withinRange )
import Distribution.Package
         ( PackageName(PackageName), packageName, packageVersion )
import Distribution.Text
         ( display, simpleParse )
import Language.Haskell.Extension (Extension(..))
import System.FilePath
         ( (</>), takeExtension, isRelative, isAbsolute
         , splitDirectories,  splitPath )
import System.FilePath.Windows as FilePath.Windows
         ( isValid )

-- | Results of some kind of failed package check.
--
-- There are a range of severities, from merely dubious to totally insane.
-- All of them come with a human readable explanation. In future we may augment
-- them with more machine readable explanations, for example to help an IDE
-- suggest automatic corrections.
--
data PackageCheck =

       -- | This package description is no good. There's no way it's going to
       -- build sensibly. This should give an error at configure time.
       PackageBuildImpossible { explanation :: String }

       -- | A problem that is likely to affect building the package, or an
       -- issue that we'd like every package author to be aware of, even if
       -- the package is never distributed.
     | PackageBuildWarning { explanation :: String }

       -- | An issue that might not be a problem for the package author but
       -- might be annoying or determental when the package is distributed to
       -- users. We should encourage distributed packages to be free from these
       -- issues, but occasionally there are justifiable reasons so we cannot
       -- ban them entirely.
     | PackageDistSuspicious { explanation :: String }

       -- | An issue that is ok in the author's environment but is almost
       -- certain to be a portability problem for other environments. We can
       -- quite legitimately refuse to publicly distribute packages with these
       -- problems.
     | PackageDistInexcusable { explanation :: String }

instance Show PackageCheck where
    show notice = explanation notice

check :: Bool -> PackageCheck -> Maybe PackageCheck
check False _  = Nothing
check True  pc = Just pc

-- ------------------------------------------------------------
-- * Standard checks
-- ------------------------------------------------------------

-- TODO:
--
--  * check for unknown 'OS's and 'Arch's. This requires checking the
--    'GenericPackageDescription' which we do not currently get passed.

-- | Check for common mistakes and problems in package descriptions.
--
-- This is the standard collection of checks covering all apsects except
-- for checks that require looking at files within the package. For those
-- see 'checkPackageFiles'.
--
-- It requires the 'GenericPackageDescription' and optionally a particular
-- configuration of that package. If you pass 'Nothing' then we just check
-- a version of the generic description using 'flattenPackageDescription'.
--
checkPackage :: GenericPackageDescription
             -> Maybe PackageDescription
             -> [PackageCheck]
checkPackage gpkg mpkg =
     checkConfiguredPackage pkg
  ++ checkConditionals gpkg
  where
    pkg = fromMaybe (flattenPackageDescription gpkg) mpkg

--TODO: make this variant go away
--      we should alwaws know the GenericPackageDescription
checkConfiguredPackage :: PackageDescription -> [PackageCheck]
checkConfiguredPackage pkg =
    checkSanity pkg
 ++ checkFields pkg
 ++ checkLicense pkg
 ++ checkSourceRepos pkg
 ++ checkGhcOptions pkg
 ++ checkCCOptions pkg
 ++ checkPaths pkg


-- ------------------------------------------------------------
-- * Basic sanity checks
-- ------------------------------------------------------------

-- | Check that this package description is sane.
--
checkSanity :: PackageDescription -> [PackageCheck]
checkSanity pkg =
  catMaybes [

    check (null . (\(PackageName n) -> n) . packageName $ pkg) $
      PackageBuildImpossible "No 'name' field."

  , check (null . versionBranch . packageVersion $ pkg) $
      PackageBuildImpossible "No 'version' field."

  , check (null (executables pkg) && isNothing (library pkg)) $
      PackageBuildImpossible
        "No executables and no library found. Nothing to do."
  ]

  ++ maybe []  checkLibrary    (library pkg)
  ++ concatMap checkExecutable (executables pkg)

  ++ catMaybes [

    check (not $ cabalVersion `withinRange` requiredCabalVersion) $
      PackageBuildImpossible $
           "This package requires Cabal version: "
        ++ display requiredCabalVersion
  ]

  where requiredCabalVersion = descCabalVersion pkg

checkLibrary :: Library -> [PackageCheck]
checkLibrary lib =
  catMaybes [

    check (not (null moduleDuplicates)) $
       PackageBuildWarning $
         "Duplicate modules in library: " ++ commaSep moduleDuplicates
  ]

  where moduleDuplicates = [ display module_
                           | let modules = exposedModules lib
                                        ++ otherModules (libBuildInfo lib)
                           , (module_:_:_) <- group (sort modules) ]

checkExecutable :: Executable -> [PackageCheck]
checkExecutable exe =
  catMaybes [

    check (null (modulePath exe)) $
      PackageBuildImpossible $
        "No 'Main-Is' field found for executable " ++ exeName exe

  , check (not (null (modulePath exe))
       && takeExtension (modulePath exe) `notElem` [".hs", ".lhs"]) $
      PackageBuildImpossible $
           "The 'Main-Is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor)."

  , check (not (null moduleDuplicates)) $
       PackageBuildWarning $
            "Duplicate modules in executable '" ++ exeName exe ++ "': "
         ++ commaSep moduleDuplicates
  ]

  where moduleDuplicates = [ display module_
                           | let modules = otherModules (buildInfo exe)
                           , (module_:_:_) <- group (sort modules) ]

-- ------------------------------------------------------------
-- * Additional pure checks
-- ------------------------------------------------------------

checkFields :: PackageDescription -> [PackageCheck]
checkFields pkg =
  catMaybes [

    check (not . FilePath.Windows.isValid . display . packageName $ pkg) $
      PackageDistInexcusable $
           "Unfortunately, the package name '" ++ display (packageName pkg)
        ++ "' is one of the reserved system file names on Windows. Many tools "
        ++ "need to convert package names to file names so using this name "
        ++ "would cause problems."

  , check (isNothing (buildType pkg)) $
      PackageBuildWarning $
           "No 'build-type' specified. If you do not need a custom Setup.hs or "
        ++ "./configure script then use 'build-type: Simple'."

  , case buildType pkg of
      Just (UnknownBuildType unknown) -> Just $
        PackageBuildWarning $
             quote unknown ++ " is not a known 'build-type'. "
          ++ "The known build types are: "
          ++ intercalate ", " (map display knownBuildTypes)
      _ -> Nothing

  , check (not (null unknownCompilers)) $
      PackageBuildWarning $
        "Unknown compiler " ++ intercalate ", " (map quote unknownCompilers)
                            ++ " in 'tested-with' field."

  , check (not (null unknownExtensions)) $
      PackageBuildWarning $
        "Unknown extensions: " ++ intercalate ", " unknownExtensions

  , check (null (category pkg)) $
      PackageDistSuspicious "No 'category' field."

  , check (null (description pkg)) $
      PackageDistSuspicious "No 'description' field."

  , check (null (maintainer pkg)) $
      PackageDistSuspicious "No 'maintainer' field."

  , check (null (synopsis pkg)) $
      PackageDistSuspicious "No 'synopsis' field."

  , check (length (synopsis pkg) >= 80) $
      PackageDistSuspicious
        "The 'synopsis' field is rather long (max 80 chars is recommended)."
  ]
  where
    unknownCompilers  = [ name | (OtherCompiler name, _) <- testedWith pkg ]
    unknownExtensions = [ name | bi <- allBuildInfo pkg
                               , UnknownExtension name <- extensions bi ]

checkLicense :: PackageDescription -> [PackageCheck]
checkLicense pkg =
  catMaybes [

    check (license pkg == AllRightsReserved) $
      PackageDistInexcusable
        "The 'license' field is missing or specified as AllRightsReserved."

  , case license pkg of
      UnknownLicense l -> Just $
        PackageBuildWarning $
          quote ("license: " ++ l) ++ " is not a recognised license."
      _ -> Nothing

  , check (license pkg == BSD4) $
      PackageDistSuspicious $
           "Using 'license: BSD4' is almost always a misunderstanding. 'BSD4' "
        ++ "refers to the old 4-clause BSD license with the advertising "
        ++ "clause. 'BSD3' refers the new 3-clause BSD license."

  , check (license pkg `notElem` [AllRightsReserved, PublicDomain]
           -- AllRightsReserved and PublicDomain are not strictly
           -- licenses so don't need license files.
        && null (licenseFile pkg)) $
      PackageDistSuspicious "A 'license-file' is not specified."
  ]

checkSourceRepos :: PackageDescription -> [PackageCheck]
checkSourceRepos pkg =
  catMaybes $ concat [[

    case repoKind repo of
      RepoKindUnknown kind -> Just $ PackageDistInexcusable $
        quote kind ++ " is not a recognised kind of source-repository. "
                   ++ "The repo kind is usually 'head' or 'this'"
      _ -> Nothing

  , check (repoType repo == Nothing) $
      PackageDistInexcusable
        "The source-repository 'type' is a required field."

  , check (repoLocation repo == Nothing) $
      PackageDistInexcusable
        "The source-repository 'location' is a required field."

  , check (repoType repo == Just CVS && repoModule repo == Nothing) $
      PackageDistInexcusable
        "For a CVS source-repository, the 'module' is a required field."

  , check (repoKind repo == RepoThis && repoTag repo == Nothing) $
      PackageDistInexcusable $
           "For the 'this' kind of source-repository, the 'tag' is a required "
        ++ "field. It should specify the tag corresponding to this version "
        ++ "or release of the package."

  , check (maybe False System.FilePath.isAbsolute (repoSubdir repo)) $
      PackageDistInexcusable
        "The 'subdir' field of a source-repository must be a relative path."
  ]
  | repo <- sourceRepos pkg ]

--TODO: check location looks like a URL for some repo types.

checkGhcOptions :: PackageDescription -> [PackageCheck]
checkGhcOptions pkg =
  catMaybes [

    check has_WerrorWall $
      PackageDistInexcusable $
           "'ghc-options: -Wall -Werror' makes the package very easy to "
        ++ "break with future GHC versions because new GHC versions often "
        ++ "add new warnings. Use just 'ghc-options: -Wall' instead."

  , check (not has_WerrorWall && has_Werror) $
      PackageDistSuspicious $
           "'ghc-options: -Werror' makes the package easy to "
        ++ "break with future GHC versions because new GHC versions often "
        ++ "add new warnings."

  , checkFlags ["-fasm"] $
      PackageDistInexcusable $
           "'ghc-options: -fasm' is unnecessary and breaks on all "
        ++ "arches except for x86, x86-64 and ppc."

  , checkFlags ["-fvia-C"] $
      PackageDistSuspicious $
           "'ghc-options: -fvia-C' is usually unnecessary. If your package "
        ++ "needs -via-C for correctness rather than performance then it "
        ++ "is using the FFI incorrectly and will probably not work with GHC "
        ++ "6.10 or later."

  , checkFlags ["-fhpc"] $
      PackageDistInexcusable $
        "'ghc-options: -fhpc' is not appropriate for a distributed package."

  , check (any ("-d" `isPrefixOf`) all_ghc_options) $
      PackageDistInexcusable $
        "'ghc-options: -d*' debug flags are not appropriate for a distributed package."

  , checkFlags ["-prof"] $
      PackageDistInexcusable $
        "'ghc-options: -prof' is not needed. Use the --enable-library-profiling configure flag."

  , checkFlags ["-o"] $
      PackageDistInexcusable $
        "'ghc-options: -o' is not allowed. The output files are named automatically."

  , checkFlags ["-hide-package"] $
      PackageDistInexcusable $
           "'ghc-options: -hide-package' is never needed. Cabal hides all packages."

  , checkFlags ["-main-is"] $
      PackageDistSuspicious $
           "'ghc-options: -main-is' is not portable."

  , checkFlags ["-O0", "-Onot"] $
      PackageDistInexcusable $
        "'ghc-options: -O0' is not needed. Use the --disable-optimization configure flag."

  , checkFlags [ "-O", "-O1"] $
      PackageDistInexcusable $
           "'ghc-options: -O' is not needed. Cabal automatically adds the '-O' flag. "
        ++ "Setting it yourself interferes with the --disable-optimization flag."

  , checkFlags ["-O2"] $
      PackageDistSuspicious $
           "'ghc-options: -O2' is rarely needed. Check that it is giving a real benefit "
        ++ "and not just imposing longer compile times on your users."

  , checkFlags ["-split-objs"] $
      PackageDistInexcusable $
        "'ghc-options: -split-objs' is not needed. Use the --enable-split-objs configure flag."

  , checkFlags ["-optl-Wl,-s", "-optl-s"] $
      PackageDistInexcusable $
           "'ghc-options: -optl-Wl,-s' is not needed and is not portable to all"
        ++ " operating systems. Cabal 1.4 and later automatically strip"
        ++ " executables. Cabal also has a flag --disable-executable-stripping"
        ++ " which is necessary when building packages for some Linux"
        ++ " distributions and using '-optl-Wl,-s' prevents that from working."

  , checkFlags ["-fglasgow-exts"] $
      PackageDistSuspicious $
        "Instead of 'ghc-options: -fglasgow-exts' it is preferable to use the 'extensions' field."

  , checkAlternatives "ghc-options" "extensions"
      [ (flag, display extension) | flag <- all_ghc_options
                                  , Just extension <- [ghcExtension flag] ]

  , checkAlternatives "ghc-options" "extensions"
      [ (flag, extension) | flag@('-':'X':extension) <- all_ghc_options
                          , case simpleParse extension of
                              Just (UnknownExtension _) -> True
                              Just ext -> ext `elem` compatExtensions
                                       || not (Version [1,1,6] []
                                 `withinRange` descCabalVersion pkg)
                              Nothing  -> False ]

  , checkAlternatives "ghc-options" "cpp-options" $
         [ (flag, flag) | flag@('-':'D':_) <- all_ghc_options ]
      ++ [ (flag, flag) | flag@('-':'U':_) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ghc_options ]

  , checkAlternatives "ghc-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ghc_options ]
  ]

  where
    has_WerrorWall = flip any ghc_options $ \opts ->
                               "-Werror" `elem` opts
                           && ("-Wall"   `elem` opts || "-W" `elem` opts)
    has_Werror     = any (\opts -> "-Werror" `elem` opts) ghc_options

    ghc_options = [ strs | bi <- allBuildInfo pkg
                         , (GHC, strs) <- options bi ]
    all_ghc_options = concat ghc_options

    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) all_ghc_options)

    ghcExtension ('-':'f':name) = case name of
      "allow-overlapping-instances" -> Just OverlappingInstances
      "th"                          -> Just TemplateHaskell
      "ffi"                         -> Just ForeignFunctionInterface
      "fi"                          -> Just ForeignFunctionInterface
      "no-monomorphism-restriction" -> Just NoMonomorphismRestriction
      "no-mono-pat-binds"           -> Just NoMonoPatBinds
      "allow-undecidable-instances" -> Just UndecidableInstances
      "allow-incoherent-instances"  -> Just IncoherentInstances
      "arrows"                      -> Just Arrows
      "generics"                    -> Just Generics
      "no-implicit-prelude"         -> Just NoImplicitPrelude
      "implicit-params"             -> Just ImplicitParams
      "bang-patterns"               -> Just BangPatterns
      "scoped-type-variables"       -> Just ScopedTypeVariables
      "extended-default-rules"      -> Just ExtendedDefaultRules
      _                             -> Nothing
    ghcExtension ('-':'c':"pp")     = Just CPP
    ghcExtension _                  = Nothing

    -- the known extensions in Cabal-1.1.6 that came with ghc-6.6:
    -- we can drop this test when Cabal-1.4+ is widely deployed because
    -- from that point on we can add new extensions without worrying about
    -- breaking old versions of cabal.
    compatExtensions =
      [ OverlappingInstances, UndecidableInstances, IncoherentInstances
      , RecursiveDo, ParallelListComp, MultiParamTypeClasses
      , NoMonomorphismRestriction, FunctionalDependencies, Rank2Types
      , RankNTypes, PolymorphicComponents, ExistentialQuantification
      , ScopedTypeVariables, ImplicitParams, FlexibleContexts
      , FlexibleInstances, EmptyDataDecls, CPP, BangPatterns
      , TypeSynonymInstances, TemplateHaskell, ForeignFunctionInterface
      , Arrows, Generics, NoImplicitPrelude, NamedFieldPuns, PatternGuards
      , GeneralizedNewtypeDeriving, ExtensibleRecords, RestrictedTypeSynonyms
      , HereDocuments
      ]

checkCCOptions :: PackageDescription -> [PackageCheck]
checkCCOptions pkg =
  catMaybes [

    checkAlternatives "cc-options" "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_ccOptions ]

  , checkAlternatives "cc-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ccOptions ]

  , checkAlternatives "cc-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ccOptions ]

  , checkAlternatives "ld-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ldOptions ]

  , checkAlternatives "ld-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ldOptions ]
  ]

  where all_ccOptions = [ opts | bi <- allBuildInfo pkg
                              , opts <- ccOptions bi ]
        all_ldOptions = [ opts | bi <- allBuildInfo pkg
                               , opts <- ldOptions bi ]

checkAlternatives :: String -> String -> [(String, String)] -> Maybe PackageCheck
checkAlternatives badField goodField flags =
  check (not (null badFlags)) $
    PackageBuildWarning $
         "Instead of " ++ quote (badField ++ ": " ++ unwords badFlags)
      ++ " use " ++ quote (goodField ++ ": " ++ unwords goodFlags)

  where (badFlags, goodFlags) = unzip flags

checkPaths :: PackageDescription -> [PackageCheck]
checkPaths pkg =
  [ PackageBuildWarning {
     explanation = quote (kind ++ ": " ++ dir)
                ++ " is a relative path outside of the source tree. "
                ++ "This will not work when generating a tarball with 'sdist'."
   }
  | bi <- allBuildInfo pkg
  , (dir, kind) <- [ (dir, "extra-lib-dirs") | dir <- extraLibDirs bi ]
                ++ [ (dir, "include-dirs")   | dir <- includeDirs  bi ]
                ++ [ (dir, "hs-source-dirs") | dir <- hsSourceDirs bi ]
  , isOutsideTree dir ]
  where isOutsideTree dir = case splitDirectories dir of
                              "..":_ -> True
                              _      -> False

--TODO: check for absolute and outside-of-tree paths in extra-src-files,
-- data-files, hs-src-dirs, etc.

-- ------------------------------------------------------------
-- * Checks on the GenericPackageDescription
-- ------------------------------------------------------------

checkConditionals :: GenericPackageDescription -> [PackageCheck]
checkConditionals pkg =
  catMaybes [

    check (not $ null unknownOSs) $
      PackageDistInexcusable $
           "Unknown operating system name "
        ++ intercalate ", " (map quote unknownOSs)

  , check (not $ null unknownArches) $
      PackageDistInexcusable $
           "Unknown architecture name "
        ++ intercalate ", " (map quote unknownArches)

  , check (not $ null unknownImpls) $
      PackageDistInexcusable $
           "Unknown compiler name "
        ++ intercalate ", " (map quote unknownImpls)
  ]
  where
    unknownOSs    = [ os   | OS   (OtherOS os)           <- conditions ]
    unknownArches = [ arch | Arch (OtherArch arch)       <- conditions ]
    unknownImpls  = [ impl | Impl (OtherCompiler impl) _ <- conditions ]
    conditions = maybe [] freeVars (condLibrary pkg)
              ++ concatMap (freeVars . snd) (condExecutables pkg)
    freeVars (CondNode _ _ ifs) = concatMap compfv ifs
    compfv (c, ct, mct) = condfv c ++ freeVars ct ++ maybe [] freeVars mct
    condfv c = case c of
      Var v      -> [v]
      Lit _      -> []
      CNot c1    -> condfv c1
      COr  c1 c2 -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2

-- ------------------------------------------------------------
-- * Checks involving files in the package
-- ------------------------------------------------------------

-- | Sanity check things that requires IO. It looks at the files in the
-- package and expects to find the package unpacked in at the given filepath.
--
checkPackageFiles :: PackageDescription -> FilePath -> IO [PackageCheck]
checkPackageFiles pkg root = checkPackageContent checkFilesIO pkg
  where
    checkFilesIO = CheckPackageContentOps {
      doesFileExist      = System.doesFileExist      . relative,
      doesDirectoryExist = System.doesDirectoryExist . relative
    }
    relative path = root </> path

-- | A record of operations needed to check the contents of packages.
-- Used by 'checkPackageContent'.
--
data CheckPackageContentOps m = CheckPackageContentOps {
    doesFileExist      :: FilePath -> m Bool,
    doesDirectoryExist :: FilePath -> m Bool
  }

-- | Sanity check things that requires looking at files in the package.
-- This is a generalised version of 'checkPackageFiles' that can work in any
-- monad for which you can provide 'CheckPackageContentOps' operations.
--
-- The point of this extra generality is to allow doing checks in some virtual
-- file system, for example a tarball in memory.
--
checkPackageContent :: Monad m => CheckPackageContentOps m
                    -> PackageDescription
                    -> m [PackageCheck]
checkPackageContent ops pkg = do
  licenseError    <- checkLicenseExists   ops pkg
  setupError      <- checkSetupExists     ops pkg
  configureError  <- checkConfigureExists ops pkg
  localPathErrors <- checkLocalPathsExist ops pkg

  return $ catMaybes [licenseError, setupError, configureError]
        ++ localPathErrors

checkLicenseExists :: Monad m => CheckPackageContentOps m
                   -> PackageDescription
                   -> m (Maybe PackageCheck)
checkLicenseExists ops pkg
  | null (licenseFile pkg) = return Nothing
  | otherwise = do
    exists <- doesFileExist ops file
    return $ check (not exists) $
      PackageBuildWarning $
           "The 'license-file' field refers to the file " ++ quote file
        ++ " which does not exist."

  where
    file = licenseFile pkg

checkSetupExists :: Monad m => CheckPackageContentOps m
                 -> PackageDescription
                 -> m (Maybe PackageCheck)
checkSetupExists ops _ = do
  hsexists  <- doesFileExist ops "Setup.hs"
  lhsexists <- doesFileExist ops "Setup.lhs"
  return $ check (not hsexists && not lhsexists) $
    PackageDistInexcusable $
      "The package is missing a Setup.hs or Setup.lhs script."

checkConfigureExists :: Monad m => CheckPackageContentOps m
                     -> PackageDescription
                     -> m (Maybe PackageCheck)
checkConfigureExists ops PackageDescription { buildType = Just Configure } = do
  exists <- doesFileExist ops "configure"
  return $ check (not exists) $
    PackageBuildWarning $
      "The 'build-type' is 'Configure' but there is no 'configure' script."
checkConfigureExists _ _ = return Nothing

checkLocalPathsExist :: Monad m => CheckPackageContentOps m
                     -> PackageDescription
                     -> m [PackageCheck]
checkLocalPathsExist ops pkg = do
  let dirs = [ (dir, kind)
             | bi <- allBuildInfo pkg
             , (dir, kind) <-
                  [ (dir, "extra-lib-dirs") | dir <- extraLibDirs bi ]
               ++ [ (dir, "include-dirs")   | dir <- includeDirs  bi ]
               ++ [ (dir, "hs-source-dirs") | dir <- hsSourceDirs bi ]
             , isRelative dir ]
  missing <- filterM (liftM not . doesDirectoryExist ops . fst) dirs
  return [ PackageBuildWarning {
             explanation = quote (kind ++ ": " ++ dir)
                        ++ " directory does not exist."
           }
         | (dir, kind) <- missing ]

-- ------------------------------------------------------------
-- * Checks involving files in the package
-- ------------------------------------------------------------

-- | Check the names of all files in a package for portability problems. This
-- should be done for example when creating or validating a package tarball.
--
checkPackageFileNames :: [FilePath] -> [PackageCheck]
checkPackageFileNames files =
     (take 1 . catMaybes . map checkWindowsPath $ files)
  ++ (take 1 . catMaybes . map checkTarPath     $ files)
      -- If we get any of these checks triggering then we're likely to get
      -- many, and that's probably not helpful, so return at most one.

checkWindowsPath :: FilePath -> Maybe PackageCheck
checkWindowsPath path =
  check (not $ FilePath.Windows.isValid path') $
    PackageDistInexcusable $
         "Unfortunately, the file " ++ quote path ++ " is not a valid file "
      ++ "name on Windows which would cause portability problems for this "
      ++ "package. Windows file names cannot contain any of the characters "
      ++ "\":*?<>|\" and there are a few reserved names including \"aux\", "
      ++ "\"nul\", \"con\", \"prn\", \"com1-9\", \"lpt1-9\" and \"clock$\"."
  where
    path' = ".\\" ++ path
    -- force a relative name to catch invalid file names like "f:oo" which
    -- otherwise parse as file "oo" in the current directory on the 'f' drive.

-- | Check a file name is valid for the portable POSIX tar format.
--
-- The POSIX tar format has a restriction on the length of file names. It is
-- unfortunately not a simple restriction like a maximum length. The exact
-- restriction is that either the whole path be 100 characters or less, or it
-- be possible to split the path on a directory separator such that the first
-- part is 155 characters or less and the second part 100 characters or less.
--
checkTarPath :: FilePath -> Maybe PackageCheck
checkTarPath path
  | length path > 255   = Just longPath
  | otherwise = case pack nameMax (reverse (splitPath path)) of
    Left err           -> Just err
    Right []           -> Nothing
    Right (first:rest) -> case pack prefixMax remainder of
      Left err         -> Just err
      Right []         -> Nothing
      Right (_:_)      -> Just noSplit
      where
        -- drop the '/' between the name and prefix:
        remainder = init first : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    pack _   []     = Left emptyName
    pack maxLen (c:cs)
      | n > maxLen  = Left longName
      | otherwise   = Right (pack' maxLen n cs)
      where n = length c

    pack' maxLen n (c:cs)
      | n' <= maxLen = pack' maxLen n' cs
      where n' = n + length c
    pack' _     _ cs = cs

    longPath = PackageDistInexcusable $
         "The following file name is too long to store in a portable POSIX "
      ++ "format tar archive. The maximum length is 255 ASCII characters.\n"
      ++ "The file in question is:\n  " ++ path
    longName = PackageDistInexcusable $
         "The following file name is too long to store in a portable POSIX "
      ++ "format tar archive. The maximum length for the name part (including "
      ++ "extension) is 100 ASCII characters. The maximum length for any "
      ++ "individual directory component is 155.\n"
      ++ "The file in question is:\n  " ++ path
    noSplit = PackageDistInexcusable $
         "The following file name is too long to store in a portable POSIX "
      ++ "format tar archive. While the total length is less than 255 ASCII "
      ++ "characters, there are unfortunately further restrictions. It has to "
      ++ "be possible to split the file path on a directory separator into "
      ++ "two parts such that the first part fits in 155 characters or less "
      ++ "and the second part fits in 100 characters or less. Basically you "
      ++ "have to make the file name or directory names shorter, or you could "
      ++ "split a long directory name into nested subdirectories with shorter "
      ++ "names.\nThe file in question is:\n  " ++ path
    emptyName = PackageDistInexcusable $
         "Encountered a file with an empty name, something is very wrong! "
      ++ "Files with an empty name cannot be stored in a tar archive or in "
      ++ "standard file systems."

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

quote :: String -> String
quote s = "'" ++ s ++ "'"

commaSep :: [String] -> String
commaSep = intercalate ","
