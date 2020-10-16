-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Check
-- Copyright   :  Lennart Kolmodin 2008
-- License     :  BSD3
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
-- we consider only basic problems. The higher standard is uses when
-- preparing a source tarball and by Hackage when uploading new packages. The
-- reason for this is that we want to hold packages that are expected to be
-- distributed to a higher standard than packages that are only ever expected
-- to be used on the author's own environment.

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

import Distribution.Compat.Prelude
import Prelude ()

import Data.List                                     (group)
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Pretty                           (prettyShow)
import Distribution.Simple.BuildPaths                (autogenPathsModuleName)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.CCompiler
import Distribution.Simple.Glob
import Distribution.Simple.Utils                     hiding (findPackageDesc, notice)
import Distribution.System
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.ModuleReexport
import Distribution.Utils.Generic                    (isAscii)
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath
       (splitDirectories, splitExtension, splitPath, takeExtension, takeFileName, (<.>), (</>))

import qualified Data.ByteString.Lazy      as BS
import qualified Data.Map                  as Map
import qualified Distribution.Compat.DList as DList
import qualified Distribution.SPDX         as SPDX
import qualified System.Directory          as System

import qualified System.Directory        (getDirectoryContents)
import qualified System.FilePath.Windows as FilePath.Windows (isValid)

import qualified Data.Set as Set
import qualified Distribution.Utils.ShortText as ShortText

import qualified Distribution.Types.BuildInfo.Lens                 as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens        as L

-- $setup
-- >>> import Control.Arrow ((&&&))

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
       -- might be annoying or detrimental when the package is distributed to
       -- users. We should encourage distributed packages to be free from these
       -- issues, but occasionally there are justifiable reasons so we cannot
       -- ban them entirely.
     | PackageDistSuspicious { explanation :: String }

       -- | Like PackageDistSuspicious but will only display warnings
       -- rather than causing abnormal exit when you run 'cabal check'.
     | PackageDistSuspiciousWarn { explanation :: String }

       -- | An issue that is OK in the author's environment but is almost
       -- certain to be a portability problem for other environments. We can
       -- quite legitimately refuse to publicly distribute packages with these
       -- problems.
     | PackageDistInexcusable { explanation :: String }
  deriving (Eq, Ord)

instance Show PackageCheck where
    show notice = explanation notice

check :: Bool -> PackageCheck -> Maybe PackageCheck
check False _  = Nothing
check True  pc = Just pc

checkSpecVersion :: PackageDescription -> CabalSpecVersion -> Bool -> PackageCheck
                 -> Maybe PackageCheck
checkSpecVersion pkg specver cond pc
  | specVersion pkg >= specver  = Nothing
  | otherwise                   = check cond pc

-- ------------------------------------------------------------
-- * Standard checks
-- ------------------------------------------------------------

-- | Check for common mistakes and problems in package descriptions.
--
-- This is the standard collection of checks covering all aspects except
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
  ++ checkPackageVersions gpkg
  ++ checkDevelopmentOnlyFlags gpkg
  ++ checkFlagNames gpkg
  ++ checkUnusedFlags gpkg
  ++ checkUnicodeXFields gpkg
  ++ checkPathsModuleExtensions pkg
  where
    pkg = fromMaybe (flattenPackageDescription gpkg) mpkg

--TODO: make this variant go away
--      we should always know the GenericPackageDescription
checkConfiguredPackage :: PackageDescription -> [PackageCheck]
checkConfiguredPackage pkg =
    checkSanity pkg
 ++ checkFields pkg
 ++ checkLicense pkg
 ++ checkSourceRepos pkg
 ++ checkAllGhcOptions pkg
 ++ checkCCOptions pkg
 ++ checkCxxOptions pkg
 ++ checkCPPOptions pkg
 ++ checkPaths pkg
 ++ checkCabalVersion pkg


-- ------------------------------------------------------------
-- * Basic sanity checks
-- ------------------------------------------------------------

-- | Check that this package description is sane.
--
checkSanity :: PackageDescription -> [PackageCheck]
checkSanity pkg =
  catMaybes [

    check (null . unPackageName . packageName $ pkg) $
      PackageBuildImpossible "No 'name' field."

  , check (nullVersion == packageVersion pkg) $
      PackageBuildImpossible "No 'version' field."

  , check (all ($ pkg) [ null . executables
                       , null . testSuites
                       , null . benchmarks
                       , null . allLibraries
                       , null . foreignLibs ]) $
      PackageBuildImpossible
        "No executables, libraries, tests, or benchmarks found. Nothing to do."

  , check (any (== LMainLibName) (map libName $ subLibraries pkg)) $
      PackageBuildImpossible $ "Found one or more unnamed internal libraries. "
        ++ "Only the non-internal library can have the same name as the package."

  , check (not (null duplicateNames)) $
      PackageBuildImpossible $ "Duplicate sections: "
        ++ commaSep (map unUnqualComponentName duplicateNames)
        ++ ". The name of every library, executable, test suite,"
        ++ " and benchmark section in"
        ++ " the package must be unique."

  -- NB: but it's OK for executables to have the same name!
  -- TODO shouldn't need to compare on the string level
  , check (any (== prettyShow (packageName pkg)) (prettyShow <$> subLibNames)) $
      PackageBuildImpossible $ "Illegal internal library name "
        ++ prettyShow (packageName pkg)
        ++ ". Internal libraries cannot have the same name as the package."
        ++ " Maybe you wanted a non-internal library?"
        ++ " If so, rewrite the section stanza"
        ++ " from 'library: '" ++ prettyShow (packageName pkg) ++ "' to 'library'."
  ]
  --TODO: check for name clashes case insensitively: windows file systems cannot
  --cope.

  ++ concatMap (checkLibrary    pkg) (allLibraries pkg)
  ++ concatMap (checkExecutable pkg) (executables pkg)
  ++ concatMap (checkTestSuite  pkg) (testSuites pkg)
  ++ concatMap (checkBenchmark  pkg) (benchmarks pkg)

  where
    -- The public 'library' gets special dispensation, because it
    -- is common practice to export a library and name the executable
    -- the same as the package.
    subLibNames = mapMaybe (libraryNameString . libName) $ subLibraries pkg
    exeNames = map exeName $ executables pkg
    testNames = map testName $ testSuites pkg
    bmNames = map benchmarkName $ benchmarks pkg
    duplicateNames = dups $ subLibNames ++ exeNames ++ testNames ++ bmNames

checkLibrary :: PackageDescription -> Library -> [PackageCheck]
checkLibrary pkg lib =
  catMaybes [

    check (not (null moduleDuplicates)) $
       PackageBuildImpossible $
            "Duplicate modules in library: "
         ++ commaSep (map prettyShow moduleDuplicates)

  -- TODO: This check is bogus if a required-signature was passed through
  , check (null (explicitLibModules lib) && null (reexportedModules lib)) $
      PackageDistSuspiciousWarn $
           showLibraryName (libName lib) ++ " does not expose any modules"

    -- check use of signatures sections
  , checkVersion CabalSpecV2_0 (not (null (signatures lib))) $
      PackageDistInexcusable $
           "To use the 'signatures' field the package needs to specify "
        ++ "at least 'cabal-version: 2.0'."

    -- check that all autogen-modules appear on other-modules or exposed-modules
  , check
      (not $ and $ map (flip elem (explicitLibModules lib)) (libModulesAutogen lib)) $
      PackageBuildImpossible $
           "An 'autogen-module' is neither on 'exposed-modules' or "
        ++ "'other-modules'."

    -- check that all autogen-includes appear on includes or install-includes
  , check
      (not $ and $ map (flip elem (allExplicitIncludes lib)) (view L.autogenIncludes lib)) $
      PackageBuildImpossible $
           "An include in 'autogen-includes' is neither in 'includes' or "
        ++ "'install-includes'."
  ]

  where
    checkVersion :: CabalSpecVersion -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= ver = Nothing
      | otherwise              = check cond pc

    -- TODO: not sure if this check is always right in Backpack
    moduleDuplicates = dups (explicitLibModules lib ++
                             map moduleReexportName (reexportedModules lib))

allExplicitIncludes :: L.HasBuildInfo a => a -> [FilePath]
allExplicitIncludes x = view L.includes x ++ view L.installIncludes x

checkExecutable :: PackageDescription -> Executable -> [PackageCheck]
checkExecutable pkg exe =
  catMaybes [

    check (null (modulePath exe)) $
      PackageBuildImpossible $
        "No 'main-is' field found for executable " ++ prettyShow (exeName exe)

  , check (not (null (modulePath exe))
       && (not $ fileExtensionSupportedLanguage $ modulePath exe)) $
      PackageBuildImpossible $
           "The 'main-is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor), "
        ++ "or it may specify a C/C++/obj-C source file."

  , checkSpecVersion pkg CabalSpecV1_18
          (fileExtensionSupportedLanguage (modulePath exe)
        && takeExtension (modulePath exe) `notElem` [".hs", ".lhs"]) $
      PackageDistInexcusable $
           "The package uses a C/C++/obj-C source file for the 'main-is' field. "
        ++ "To use this feature you must specify 'cabal-version: >= 1.18'."

  , check (not (null moduleDuplicates)) $
       PackageBuildImpossible $
            "Duplicate modules in executable '" ++ prettyShow (exeName exe) ++ "': "
         ++ commaSep (map prettyShow moduleDuplicates)

    -- check that all autogen-modules appear on other-modules
  , check
      (not $ and $ map (flip elem (exeModules exe)) (exeModulesAutogen exe)) $
      PackageBuildImpossible $
           "On executable '" ++ prettyShow (exeName exe) ++ "' an 'autogen-module' is not "
        ++ "on 'other-modules'"

    -- check that all autogen-includes appear on includes
  , check
      (not $ and $ map (flip elem (view L.includes exe)) (view L.autogenIncludes exe)) $
      PackageBuildImpossible "An include in 'autogen-includes' is not in 'includes'."
  ]
  where
    moduleDuplicates = dups (exeModules exe)

checkTestSuite :: PackageDescription -> TestSuite -> [PackageCheck]
checkTestSuite pkg test =
  catMaybes [

    case testInterface test of
      TestSuiteUnsupported tt@(TestTypeUnknown _ _) -> Just $
        PackageBuildWarning $
             quote (prettyShow tt) ++ " is not a known type of test suite. "
          ++ "The known test suite types are: "
          ++ commaSep (map prettyShow knownTestTypes)

      TestSuiteUnsupported tt -> Just $
        PackageBuildWarning $
             quote (prettyShow tt) ++ " is not a supported test suite version. "
          ++ "The known test suite types are: "
          ++ commaSep (map prettyShow knownTestTypes)
      _ -> Nothing

  , check (not $ null moduleDuplicates) $
      PackageBuildImpossible $
           "Duplicate modules in test suite '" ++ prettyShow (testName test) ++ "': "
        ++ commaSep (map prettyShow moduleDuplicates)

  , check mainIsWrongExt $
      PackageBuildImpossible $
           "The 'main-is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor), "
        ++ "or it may specify a C/C++/obj-C source file."

  , checkSpecVersion pkg CabalSpecV1_18 (mainIsNotHsExt && not mainIsWrongExt) $
      PackageDistInexcusable $
           "The package uses a C/C++/obj-C source file for the 'main-is' field. "
        ++ "To use this feature you must specify 'cabal-version: >= 1.18'."

    -- check that all autogen-modules appear on other-modules
  , check
      (not $ and $ map (flip elem (testModules test)) (testModulesAutogen test)) $
      PackageBuildImpossible $
           "On test suite '" ++ prettyShow (testName test) ++ "' an 'autogen-module' is not "
        ++ "on 'other-modules'"

    -- check that all autogen-includes appear on includes
  , check
      (not $ and $ map (flip elem (view L.includes test)) (view L.autogenIncludes test)) $
      PackageBuildImpossible "An include in 'autogen-includes' is not in 'includes'."
  ]
  where
    moduleDuplicates = dups $ testModules test

    mainIsWrongExt = case testInterface test of
      TestSuiteExeV10 _ f -> not $ fileExtensionSupportedLanguage f
      _                   -> False

    mainIsNotHsExt = case testInterface test of
      TestSuiteExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
      _                   -> False

checkBenchmark :: PackageDescription -> Benchmark -> [PackageCheck]
checkBenchmark _pkg bm =
  catMaybes [

    case benchmarkInterface bm of
      BenchmarkUnsupported tt@(BenchmarkTypeUnknown _ _) -> Just $
        PackageBuildWarning $
             quote (prettyShow tt) ++ " is not a known type of benchmark. "
          ++ "The known benchmark types are: "
          ++ commaSep (map prettyShow knownBenchmarkTypes)

      BenchmarkUnsupported tt -> Just $
        PackageBuildWarning $
             quote (prettyShow tt) ++ " is not a supported benchmark version. "
          ++ "The known benchmark types are: "
          ++ commaSep (map prettyShow knownBenchmarkTypes)
      _ -> Nothing

  , check (not $ null moduleDuplicates) $
      PackageBuildImpossible $
           "Duplicate modules in benchmark '" ++ prettyShow (benchmarkName bm) ++ "': "
        ++ commaSep (map prettyShow moduleDuplicates)

  , check mainIsWrongExt $
      PackageBuildImpossible $
           "The 'main-is' field must specify a '.hs' or '.lhs' file "
        ++ "(even if it is generated by a preprocessor)."

    -- check that all autogen-modules appear on other-modules
  , check
      (not $ and $ map (flip elem (benchmarkModules bm)) (benchmarkModulesAutogen bm)) $
      PackageBuildImpossible $
             "On benchmark '" ++ prettyShow (benchmarkName bm) ++ "' an 'autogen-module' is "
          ++ "not on 'other-modules'"

    -- check that all autogen-includes appear on includes
  , check
      (not $ and $ map (flip elem (view L.includes bm)) (view L.autogenIncludes bm)) $
      PackageBuildImpossible "An include in 'autogen-includes' is not in 'includes'."
  ]
  where
    moduleDuplicates = dups $ benchmarkModules bm

    mainIsWrongExt = case benchmarkInterface bm of
      BenchmarkExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
      _                   -> False

-- ------------------------------------------------------------
-- * Additional pure checks
-- ------------------------------------------------------------

checkFields :: PackageDescription -> [PackageCheck]
checkFields pkg =
  catMaybes [

    check (not . FilePath.Windows.isValid . prettyShow . packageName $ pkg) $
      PackageDistInexcusable $
           "Unfortunately, the package name '" ++ prettyShow (packageName pkg)
        ++ "' is one of the reserved system file names on Windows. Many tools "
        ++ "need to convert package names to file names so using this name "
        ++ "would cause problems."

  , check ((isPrefixOf "z-") . prettyShow . packageName $ pkg) $
      PackageDistInexcusable $
           "Package names with the prefix 'z-' are reserved by Cabal and "
        ++ "cannot be used."

  , check (isNothing (buildTypeRaw pkg) && specVersion pkg < CabalSpecV2_2) $
      PackageBuildWarning $
           "No 'build-type' specified. If you do not need a custom Setup.hs or "
        ++ "./configure script then use 'build-type: Simple'."

  , check (isJust (setupBuildInfo pkg) && buildType pkg /= Custom) $
      PackageBuildWarning $
           "Ignoring the 'custom-setup' section because the 'build-type' is "
        ++ "not 'Custom'. Use 'build-type: Custom' if you need to use a "
        ++ "custom Setup.hs script."

  , check (not (null unknownCompilers)) $
      PackageBuildWarning $
        "Unknown compiler " ++ commaSep (map quote unknownCompilers)
                            ++ " in 'tested-with' field."

  , check (not (null unknownLanguages)) $
      PackageBuildWarning $
        "Unknown languages: " ++ commaSep unknownLanguages

  , check (not (null unknownExtensions)) $
      PackageBuildWarning $
        "Unknown extensions: " ++ commaSep unknownExtensions

  , check (not (null languagesUsedAsExtensions)) $
      PackageBuildWarning $
           "Languages listed as extensions: "
        ++ commaSep languagesUsedAsExtensions
        ++ ". Languages must be specified in either the 'default-language' "
        ++ " or the 'other-languages' field."

  , check (not (null ourDeprecatedExtensions)) $
      PackageDistSuspicious $
           "Deprecated extensions: "
        ++ commaSep (map (quote . prettyShow . fst) ourDeprecatedExtensions)
        ++ ". " ++ unwords
             [ "Instead of '" ++ prettyShow ext
            ++ "' use '" ++ prettyShow replacement ++ "'."
             | (ext, Just replacement) <- ourDeprecatedExtensions ]

  , check (ShortText.null (category pkg)) $
      PackageDistSuspicious "No 'category' field."

  , check (ShortText.null (maintainer pkg)) $
      PackageDistSuspicious "No 'maintainer' field."

  , check (ShortText.null (synopsis pkg) && ShortText.null (description pkg)) $
      PackageDistInexcusable "No 'synopsis' or 'description' field."

  , check (ShortText.null (description pkg) && not (ShortText.null (synopsis pkg))) $
      PackageDistSuspicious "No 'description' field."

  , check (ShortText.null (synopsis pkg) && not (ShortText.null (description pkg))) $
      PackageDistSuspicious "No 'synopsis' field."

    --TODO: recommend the bug reports URL, author and homepage fields
    --TODO: recommend not using the stability field
    --TODO: recommend specifying a source repo

  , check (ShortText.length (synopsis pkg) >= 80) $
      PackageDistSuspicious
        "The 'synopsis' field is rather long (max 80 chars is recommended)."

    -- See also https://github.com/haskell/cabal/pull/3479
  , check (not (ShortText.null (description pkg))
           && ShortText.length (description pkg) <= ShortText.length (synopsis pkg)) $
      PackageDistSuspicious $
           "The 'description' field should be longer than the 'synopsis' "
        ++ "field. "
        ++ "It's useful to provide an informative 'description' to allow "
        ++ "Haskell programmers who have never heard about your package to "
        ++ "understand the purpose of your package. "
        ++ "The 'description' field content is typically shown by tooling "
        ++ "(e.g. 'cabal info', Haddock, Hackage) below the 'synopsis' which "
        ++ "serves as a headline. "
        ++ "Please refer to <https://www.haskell.org/"
        ++ "cabal/users-guide/developing-packages.html#package-properties>"
        ++ " for more details."

    -- check use of impossible constraints "tested-with: GHC== 6.10 && ==6.12"
  , check (not (null testedWithImpossibleRanges)) $
      PackageDistInexcusable $
           "Invalid 'tested-with' version range: "
        ++ commaSep (map prettyShow testedWithImpossibleRanges)
        ++ ". To indicate that you have tested a package with multiple "
        ++ "different versions of the same compiler use multiple entries, "
        ++ "for example 'tested-with: GHC==6.10.4, GHC==6.12.3' and not "
        ++ "'tested-with: GHC==6.10.4 && ==6.12.3'."

  , check (not (null depInternalLibraryWithExtraVersion)) $
      PackageBuildWarning $
           "The package has an extraneous version range for a dependency on an "
        ++ "internal library: "
        ++ commaSep (map prettyShow depInternalLibraryWithExtraVersion)
        ++ ". This version range includes the current package but isn't needed "
        ++ "as the current package's library will always be used."

  , check (not (null depInternalLibraryWithImpossibleVersion)) $
      PackageBuildImpossible $
           "The package has an impossible version range for a dependency on an "
        ++ "internal library: "
        ++ commaSep (map prettyShow depInternalLibraryWithImpossibleVersion)
        ++ ". This version range does not include the current package, and must "
        ++ "be removed as the current package's library will always be used."

  , check (not (null depInternalExecutableWithExtraVersion)) $
      PackageBuildWarning $
           "The package has an extraneous version range for a dependency on an "
        ++ "internal executable: "
        ++ commaSep (map prettyShow depInternalExecutableWithExtraVersion)
        ++ ". This version range includes the current package but isn't needed "
        ++ "as the current package's executable will always be used."

  , check (not (null depInternalExecutableWithImpossibleVersion)) $
      PackageBuildImpossible $
           "The package has an impossible version range for a dependency on an "
        ++ "internal executable: "
        ++ commaSep (map prettyShow depInternalExecutableWithImpossibleVersion)
        ++ ". This version range does not include the current package, and must "
        ++ "be removed as the current package's executable will always be used."

  , check (not (null depMissingInternalExecutable)) $
      PackageBuildImpossible $
           "The package depends on a missing internal executable: "
        ++ commaSep (map prettyShow depInternalExecutableWithImpossibleVersion)
  ]
  where
    unknownCompilers  = [ name | (OtherCompiler name, _) <- testedWith pkg ]
    unknownLanguages  = [ name | bi <- allBuildInfo pkg
                               , UnknownLanguage name <- allLanguages bi ]
    unknownExtensions = [ name | bi <- allBuildInfo pkg
                               , UnknownExtension name <- allExtensions bi
                               , name `notElem` map prettyShow knownLanguages ]
    ourDeprecatedExtensions = nub $ catMaybes
      [ find ((==ext) . fst) deprecatedExtensions
      | bi <- allBuildInfo pkg
      , ext <- allExtensions bi ]
    languagesUsedAsExtensions =
      [ name | bi <- allBuildInfo pkg
             , UnknownExtension name <- allExtensions bi
             , name `elem` map prettyShow knownLanguages ]

    testedWithImpossibleRanges =
      [ Dependency (mkPackageName (prettyShow compiler)) vr mainLibSet
      | (compiler, vr) <- testedWith pkg
      , isNoVersion vr ]

    internalLibraries =
        map (maybe (packageName pkg) (unqualComponentNameToPackageName) . libraryNameString . libName)
            (allLibraries pkg)

    internalExecutables = map exeName $ executables pkg

    internalLibDeps =
      [ dep
      | bi <- allBuildInfo pkg
      , dep@(Dependency name _ _) <- targetBuildDepends bi
      , name `elem` internalLibraries
      ]

    internalExeDeps =
      [ dep
      | bi <- allBuildInfo pkg
      , dep <- getAllToolDependencies pkg bi
      , isInternal pkg dep
      ]

    depInternalLibraryWithExtraVersion =
      [ dep
      | dep@(Dependency _ versionRange _) <- internalLibDeps
      , not $ isAnyVersion versionRange
      , packageVersion pkg `withinRange` versionRange
      ]

    depInternalLibraryWithImpossibleVersion =
      [ dep
      | dep@(Dependency _ versionRange _) <- internalLibDeps
      , not $ packageVersion pkg `withinRange` versionRange
      ]

    depInternalExecutableWithExtraVersion =
      [ dep
      | dep@(ExeDependency _ _ versionRange) <- internalExeDeps
      , not $ isAnyVersion versionRange
      , packageVersion pkg `withinRange` versionRange
      ]

    depInternalExecutableWithImpossibleVersion =
      [ dep
      | dep@(ExeDependency _ _ versionRange) <- internalExeDeps
      , not $ packageVersion pkg `withinRange` versionRange
      ]

    depMissingInternalExecutable =
      [ dep
      | dep@(ExeDependency _ eName _) <- internalExeDeps
      , not $ eName `elem` internalExecutables
      ]


checkLicense :: PackageDescription -> [PackageCheck]
checkLicense pkg = case licenseRaw pkg of
    Right l -> checkOldLicense pkg l
    Left  l -> checkNewLicense pkg l

checkNewLicense :: PackageDescription -> SPDX.License -> [PackageCheck]
checkNewLicense _pkg lic = catMaybes
    [ check (lic == SPDX.NONE) $
        PackageDistInexcusable
            "The 'license' field is missing or is NONE."
    ]

checkOldLicense :: PackageDescription -> License -> [PackageCheck]
checkOldLicense pkg lic = catMaybes
  [ check (lic == UnspecifiedLicense) $
      PackageDistInexcusable
        "The 'license' field is missing."

  , check (lic == AllRightsReserved) $
      PackageDistSuspicious
        "The 'license' is AllRightsReserved. Is that really what you want?"

  , checkVersion CabalSpecV1_4 (lic `notElem` compatLicenses) $
      PackageDistInexcusable $
           "Unfortunately the license " ++ quote (prettyShow (license pkg))
        ++ " messes up the parser in earlier Cabal versions so you need to "
        ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then use 'OtherLicense'."

  , case lic of
      UnknownLicense l -> Just $
        PackageBuildWarning $
             quote ("license: " ++ l) ++ " is not a recognised license. The "
          ++ "known licenses are: "
          ++ commaSep (map prettyShow knownLicenses)
      _ -> Nothing

  , check (lic == BSD4) $
      PackageDistSuspicious $
           "Using 'license: BSD4' is almost always a misunderstanding. 'BSD4' "
        ++ "refers to the old 4-clause BSD license with the advertising "
        ++ "clause. 'BSD3' refers the new 3-clause BSD license."

  , case unknownLicenseVersion (lic) of
      Just knownVersions -> Just $
        PackageDistSuspicious $
             "'license: " ++ prettyShow (lic) ++ "' is not a known "
          ++ "version of that license. The known versions are "
          ++ commaSep (map prettyShow knownVersions)
          ++ ". If this is not a mistake and you think it should be a known "
          ++ "version then please file a ticket."
      _ -> Nothing

  , check (lic `notElem` [ AllRightsReserved
                                 , UnspecifiedLicense, PublicDomain]
           -- AllRightsReserved and PublicDomain are not strictly
           -- licenses so don't need license files.
        && null (licenseFiles pkg)) $
      PackageDistSuspicious "A 'license-file' is not specified."
  ]
  where
    unknownLicenseVersion (GPL  (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | GPL  (Just v') <- knownLicenses ]
    unknownLicenseVersion (LGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | LGPL (Just v') <- knownLicenses ]
    unknownLicenseVersion (AGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | AGPL (Just v') <- knownLicenses ]
    unknownLicenseVersion (Apache  (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where knownVersions = [ v' | Apache  (Just v') <- knownLicenses ]
    unknownLicenseVersion _ = Nothing

    checkVersion :: CabalSpecVersion -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= ver  = Nothing
      | otherwise               = check cond pc

    compatLicenses = [ GPL Nothing, LGPL Nothing, AGPL Nothing, BSD3, BSD4
                     , PublicDomain, AllRightsReserved
                     , UnspecifiedLicense, OtherLicense ]

checkSourceRepos :: PackageDescription -> [PackageCheck]
checkSourceRepos pkg =
  catMaybes $ concat [[

    case repoKind repo of
      RepoKindUnknown kind -> Just $ PackageDistInexcusable $
        quote kind ++ " is not a recognised kind of source-repository. "
                   ++ "The repo kind is usually 'head' or 'this'"
      _ -> Nothing

  , check (isNothing (repoType repo)) $
      PackageDistInexcusable
        "The source-repository 'type' is a required field."

  , check (isNothing (repoLocation repo)) $
      PackageDistInexcusable
        "The source-repository 'location' is a required field."

  , check (repoType repo == Just (KnownRepoType CVS) && isNothing (repoModule repo)) $
      PackageDistInexcusable
        "For a CVS source-repository, the 'module' is a required field."

  , check (repoKind repo == RepoThis && isNothing (repoTag repo)) $
      PackageDistInexcusable $
           "For the 'this' kind of source-repository, the 'tag' is a required "
        ++ "field. It should specify the tag corresponding to this version "
        ++ "or release of the package."

  , check (maybe False isAbsoluteOnAnyPlatform (repoSubdir repo)) $
      PackageDistInexcusable
        "The 'subdir' field of a source-repository must be a relative path."

  , check (maybe False isAbsoluteOnAnyPlatform (repoSubdir repo)) $
      PackageDistInexcusable
        "The 'subdir' field of a source-repository must be a relative path."

  , do
      subdir <- repoSubdir repo
      err    <- isGoodRelativeDirectoryPath subdir
      return $ PackageDistInexcusable $
        "The 'subdir' field of a source-repository is not a good relative path: " ++ show err
  ]
  | repo <- sourceRepos pkg ]

--TODO: check location looks like a URL for some repo types.

-- | Checks GHC options from all ghc-*-options fields in the given
-- PackageDescription and reports commonly misused or non-portable flags
checkAllGhcOptions :: PackageDescription -> [PackageCheck]
checkAllGhcOptions pkg =
    checkGhcOptions "ghc-options" (hcOptions GHC) pkg
 ++ checkGhcOptions "ghc-prof-options" (hcProfOptions GHC) pkg
 ++ checkGhcOptions "ghc-shared-options" (hcSharedOptions GHC) pkg

-- | Extracts GHC options belonging to the given field from the given
-- PackageDescription using given function and checks them for commonly misused
-- or non-portable flags
checkGhcOptions :: String -> (BuildInfo -> [String]) -> PackageDescription -> [PackageCheck]
checkGhcOptions fieldName getOptions pkg =
  catMaybes [

    checkFlags ["-fasm"] $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -fasm' is unnecessary and will not work on CPU "
        ++ "architectures other than x86, x86-64, ppc or sparc."

  , checkFlags ["-fvia-C"] $
      PackageDistSuspicious $
           "'" ++ fieldName ++": -fvia-C' is usually unnecessary. If your package "
        ++ "needs -via-C for correctness rather than performance then it "
        ++ "is using the FFI incorrectly and will probably not work with GHC "
        ++ "6.10 or later."

  , checkFlags ["-fhpc"] $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -fhpc' is not necessary. Use the configure flag "
        ++ " --enable-coverage instead."

  , checkFlags ["-prof"] $
      PackageBuildWarning $
           "'" ++ fieldName ++ ": -prof' is not necessary and will lead to problems "
        ++ "when used on a library. Use the configure flag "
        ++ "--enable-library-profiling and/or --enable-profiling."

  , checkFlags ["-o"] $
      PackageBuildWarning $
           "'" ++ fieldName ++ ": -o' is not needed. "
        ++ "The output files are named automatically."

  , checkFlags ["-hide-package"] $
      PackageBuildWarning $
      "'" ++ fieldName ++ ": -hide-package' is never needed. "
      ++ "Cabal hides all packages."

  , checkFlags ["--make"] $
      PackageBuildWarning $
      "'" ++ fieldName ++ ": --make' is never needed. Cabal uses this automatically."

  , checkFlags ["-main-is"] $
      PackageDistSuspicious $
      "'" ++ fieldName ++ ": -main-is' is not portable."

  , checkNonTestAndBenchmarkFlags ["-O0", "-Onot"] $
      PackageDistSuspicious $
      "'" ++ fieldName ++ ": -O0' is not needed. "
      ++ "Use the --disable-optimization configure flag."

  , checkTestAndBenchmarkFlags ["-O0", "-Onot"] $
      PackageDistSuspiciousWarn $
      "'" ++ fieldName ++ ": -O0' is not needed. "
      ++ "Use the --disable-optimization configure flag."

  , checkFlags [ "-O", "-O1"] $
      PackageDistInexcusable $
      "'" ++ fieldName ++ ": -O' is not needed. "
      ++ "Cabal automatically adds the '-O' flag. "
      ++ "Setting it yourself interferes with the --disable-optimization flag."

  , checkFlags ["-O2"] $
      PackageDistSuspiciousWarn $
      "'" ++ fieldName ++ ": -O2' is rarely needed. "
      ++ "Check that it is giving a real benefit "
      ++ "and not just imposing longer compile times on your users."

  , checkFlags ["-split-sections"] $
      PackageBuildWarning $
        "'" ++ fieldName ++ ": -split-sections' is not needed. "
        ++ "Use the --enable-split-sections configure flag."

  , checkFlags ["-split-objs"] $
      PackageBuildWarning $
        "'" ++ fieldName ++ ": -split-objs' is not needed. "
        ++ "Use the --enable-split-objs configure flag."

  , checkFlags ["-optl-Wl,-s", "-optl-s"] $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -optl-Wl,-s' is not needed and is not portable to all"
        ++ " operating systems. Cabal 1.4 and later automatically strip"
        ++ " executables. Cabal also has a flag --disable-executable-stripping"
        ++ " which is necessary when building packages for some Linux"
        ++ " distributions and using '-optl-Wl,-s' prevents that from working."

  , checkFlags ["-fglasgow-exts"] $
      PackageDistSuspicious $
        "Instead of '" ++ fieldName ++ ": -fglasgow-exts' it is preferable to use "
        ++ "the 'extensions' field."

  , check ("-threaded" `elem` lib_ghc_options) $
      PackageBuildWarning $
           "'" ++ fieldName ++ ": -threaded' has no effect for libraries. It should "
        ++ "only be used for executables."

  , check ("-rtsopts" `elem` lib_ghc_options) $
      PackageBuildWarning $
           "'" ++ fieldName ++ ": -rtsopts' has no effect for libraries. It should "
        ++ "only be used for executables."

  , check (any (\opt -> "-with-rtsopts" `isPrefixOf` opt) lib_ghc_options) $
      PackageBuildWarning $
           "'" ++ fieldName ++ ": -with-rtsopts' has no effect for libraries. It "
        ++ "should only be used for executables."

  , checkAlternatives fieldName "extensions"
      [ (flag, prettyShow extension) | flag <- all_ghc_options
                                  , Just extension <- [ghcExtension flag] ]

  , checkAlternatives fieldName "extensions"
      [ (flag, extension) | flag@('-':'X':extension) <- all_ghc_options ]

  , checkAlternatives fieldName "cpp-options" $
         [ (flag, flag) | flag@('-':'D':_) <- all_ghc_options ]
      ++ [ (flag, flag) | flag@('-':'U':_) <- all_ghc_options ]

  , checkAlternatives fieldName "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_ghc_options ]

  , checkAlternatives fieldName "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ghc_options ]

  , checkAlternatives fieldName "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ghc_options ]

  , checkAlternatives fieldName "frameworks"
      [ (flag, fmwk) | (flag@"-framework", fmwk) <-
           zip all_ghc_options (safeTail all_ghc_options) ]

  , checkAlternatives fieldName "extra-framework-dirs"
      [ (flag, dir) | (flag@"-framework-path", dir) <-
           zip all_ghc_options (safeTail all_ghc_options) ]
  ]

  where
    all_ghc_options    = concatMap getOptions (allBuildInfo pkg)
    lib_ghc_options    = concatMap (getOptions . libBuildInfo)
                         (allLibraries pkg)
    test_ghc_options      = concatMap (getOptions . testBuildInfo)
                            (testSuites pkg)
    benchmark_ghc_options = concatMap (getOptions . benchmarkBuildInfo)
                            (benchmarks pkg)
    test_and_benchmark_ghc_options     = test_ghc_options ++
                                         benchmark_ghc_options
    non_test_and_benchmark_ghc_options = concatMap getOptions
                                         (allBuildInfo (pkg { testSuites = []
                                                            , benchmarks = []
                                                            }))

    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) all_ghc_options)

    checkTestAndBenchmarkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkTestAndBenchmarkFlags flags = check (any (`elem` flags) test_and_benchmark_ghc_options)

    checkNonTestAndBenchmarkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkNonTestAndBenchmarkFlags flags = check (any (`elem` flags) non_test_and_benchmark_ghc_options)

    ghcExtension ('-':'f':name) = case name of
      "allow-overlapping-instances"    -> enable  OverlappingInstances
      "no-allow-overlapping-instances" -> disable OverlappingInstances
      "th"                             -> enable  TemplateHaskell
      "no-th"                          -> disable TemplateHaskell
      "ffi"                            -> enable  ForeignFunctionInterface
      "no-ffi"                         -> disable ForeignFunctionInterface
      "fi"                             -> enable  ForeignFunctionInterface
      "no-fi"                          -> disable ForeignFunctionInterface
      "monomorphism-restriction"       -> enable  MonomorphismRestriction
      "no-monomorphism-restriction"    -> disable MonomorphismRestriction
      "mono-pat-binds"                 -> enable  MonoPatBinds
      "no-mono-pat-binds"              -> disable MonoPatBinds
      "allow-undecidable-instances"    -> enable  UndecidableInstances
      "no-allow-undecidable-instances" -> disable UndecidableInstances
      "allow-incoherent-instances"     -> enable  IncoherentInstances
      "no-allow-incoherent-instances"  -> disable IncoherentInstances
      "arrows"                         -> enable  Arrows
      "no-arrows"                      -> disable Arrows
      "generics"                       -> enable  Generics
      "no-generics"                    -> disable Generics
      "implicit-prelude"               -> enable  ImplicitPrelude
      "no-implicit-prelude"            -> disable ImplicitPrelude
      "implicit-params"                -> enable  ImplicitParams
      "no-implicit-params"             -> disable ImplicitParams
      "bang-patterns"                  -> enable  BangPatterns
      "no-bang-patterns"               -> disable BangPatterns
      "scoped-type-variables"          -> enable  ScopedTypeVariables
      "no-scoped-type-variables"       -> disable ScopedTypeVariables
      "extended-default-rules"         -> enable  ExtendedDefaultRules
      "no-extended-default-rules"      -> disable ExtendedDefaultRules
      _                                -> Nothing
    ghcExtension "-cpp"             = enable CPP
    ghcExtension _                  = Nothing

    enable  e = Just (EnableExtension e)
    disable e = Just (DisableExtension e)

checkCCOptions :: PackageDescription -> [PackageCheck]
checkCCOptions = checkCLikeOptions "C" "cc-options" ccOptions

checkCxxOptions :: PackageDescription -> [PackageCheck]
checkCxxOptions = checkCLikeOptions "C++" "cxx-options" cxxOptions

checkCLikeOptions :: String -> String -> (BuildInfo -> [String]) -> PackageDescription -> [PackageCheck]
checkCLikeOptions label prefix accessor pkg =
  catMaybes [

    checkAlternatives prefix "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_cLikeOptions ]

  , checkAlternatives prefix "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_cLikeOptions ]

  , checkAlternatives prefix "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_cLikeOptions ]

  , checkAlternatives "ld-options" "extra-libraries"
      [ (flag, lib) | flag@('-':'l':lib) <- all_ldOptions ]

  , checkAlternatives "ld-options" "extra-lib-dirs"
      [ (flag, dir) | flag@('-':'L':dir) <- all_ldOptions ]

  , checkCCFlags [ "-O", "-Os", "-O0", "-O1", "-O2", "-O3" ] $
      PackageDistSuspicious $
           "'"++prefix++": -O[n]' is generally not needed. When building with "
        ++ " optimisations Cabal automatically adds '-O2' for "++label++" code. "
        ++ "Setting it yourself interferes with the --disable-optimization flag."
  ]

  where all_cLikeOptions = [ opts | bi <- allBuildInfo pkg
                                  , opts <- accessor bi ]
        all_ldOptions = [ opts | bi <- allBuildInfo pkg
                               , opts <- ldOptions bi ]

        checkCCFlags :: [String] -> PackageCheck -> Maybe PackageCheck
        checkCCFlags flags = check (any (`elem` flags) all_cLikeOptions)

checkCPPOptions :: PackageDescription -> [PackageCheck]
checkCPPOptions pkg = catMaybes
    [ checkAlternatives "cpp-options" "include-dirs"
      [ (flag, dir) | flag@('-':'I':dir) <- all_cppOptions ]
    ]
    ++
    [ PackageBuildWarning $ "'cpp-options': " ++ opt ++ " is not portable C-preprocessor flag"
    | opt <- all_cppOptions
    -- "-I" is handled above, we allow only -DNEWSTUFF and -UOLDSTUFF
    , not $ any (`isPrefixOf` opt) ["-D", "-U", "-I" ]
    ]
  where
    all_cppOptions = [ opts | bi <- allBuildInfo pkg, opts <- cppOptions bi ]

checkAlternatives :: String -> String -> [(String, String)]
                  -> Maybe PackageCheck
checkAlternatives badField goodField flags =
  check (not (null badFlags)) $
    PackageBuildWarning $
         "Instead of " ++ quote (badField ++ ": " ++ unwords badFlags)
      ++ " use " ++ quote (goodField ++ ": " ++ unwords goodFlags)

  where (badFlags, goodFlags) = unzip flags

data PathKind
    = PathKindFile
    | PathKindDirectory
    | PathKindGlob

checkPaths :: PackageDescription -> [PackageCheck]
checkPaths pkg =
  [ PackageBuildWarning $
         quote (field ++ ": " ++ path)
      ++ " is a relative path outside of the source tree. "
      ++ "This will not work when generating a tarball with 'sdist'."
  | (path, field, _) <- relPaths ++ absPaths
  , isOutsideTree path ]
  ++
  [ PackageDistInexcusable $
      quote (field ++ ": " ++ path) ++ " is an absolute path."
  | (path, field, _) <- relPaths
  , isAbsoluteOnAnyPlatform path ]
  ++
  [ PackageDistInexcusable $
      quote (field ++ ": " ++ path) ++ " is not good relative path: " ++ err
  | (path, field, kind) <- relPaths
  -- these are not paths, but globs...
  , err <- maybeToList $ case kind of
      PathKindFile      -> isGoodRelativeFilePath path
      PathKindGlob      -> isGoodRelativeGlob path
      PathKindDirectory -> isGoodRelativeDirectoryPath path
  ]
  ++
  [ PackageDistInexcusable $
         quote (field ++ ": " ++ path) ++ " points inside the 'dist' "
      ++ "directory. This is not reliable because the location of this "
      ++ "directory is configurable by the user (or package manager). In "
      ++ "addition the layout of the 'dist' directory is subject to change "
      ++ "in future versions of Cabal."
  | (path, field, _) <- relPaths ++ absPaths
  , isInsideDist path ]
  ++
  [ PackageDistInexcusable $
         "The 'ghc-options' contains the path '" ++ path ++ "' which points "
      ++ "inside the 'dist' directory. This is not reliable because the "
      ++ "location of this directory is configurable by the user (or package "
      ++ "manager). In addition the layout of the 'dist' directory is subject "
      ++ "to change in future versions of Cabal."
  | bi <- allBuildInfo pkg
  , (GHC, flags) <- perCompilerFlavorToList $ options bi
  , path <- flags
  , isInsideDist path ]
  ++
  [ PackageDistInexcusable $
        "In the 'data-files' field: " ++ explainGlobSyntaxError pat err
  | pat <- dataFiles pkg
  , Left err <- [parseFileGlob (specVersion pkg) pat]
  ]
  ++
  [ PackageDistInexcusable $
        "In the 'extra-source-files' field: " ++ explainGlobSyntaxError pat err
  | pat <- extraSrcFiles pkg
  , Left err <- [parseFileGlob (specVersion pkg) pat]
  ]
  ++
  [ PackageDistInexcusable $
        "In the 'extra-doc-files' field: " ++ explainGlobSyntaxError pat err
  | pat <- extraDocFiles pkg
  , Left err <- [parseFileGlob (specVersion pkg) pat]
  ]
  where
    isOutsideTree path = case splitDirectories path of
      "..":_     -> True
      ".":"..":_ -> True
      _          -> False
    isInsideDist path = case map lowercase (splitDirectories path) of
      "dist"    :_ -> True
      ".":"dist":_ -> True
      _            -> False

    -- paths that must be relative
    relPaths :: [(FilePath, String, PathKind)]
    relPaths =
      [ (path, "extra-source-files", PathKindGlob)      | path <- extraSrcFiles pkg ] ++
      [ (path, "extra-tmp-files",    PathKindFile)      | path <- extraTmpFiles pkg ] ++
      [ (path, "extra-doc-files",    PathKindGlob)      | path <- extraDocFiles pkg ] ++
      [ (path, "data-files",         PathKindGlob)      | path <- dataFiles     pkg ] ++
      [ (path, "data-dir",           PathKindDirectory) | path <- [dataDir      pkg]] ++
      [ (path, "license-file",       PathKindFile)      | path <- licenseFiles  pkg ] ++
      concat
        [ [ (path, "asm-sources",      PathKindFile)      | path <- asmSources      bi ] ++
          [ (path, "cmm-sources",      PathKindFile)      | path <- cmmSources      bi ] ++
          [ (path, "c-sources",        PathKindFile)      | path <- cSources        bi ] ++
          [ (path, "cxx-sources",      PathKindFile)      | path <- cxxSources      bi ] ++
          [ (path, "js-sources",       PathKindFile)      | path <- jsSources       bi ] ++
          [ (path, "install-includes", PathKindFile)      | path <- installIncludes bi ] ++
          [ (path, "hs-source-dirs",   PathKindDirectory) | path <- hsSourceDirs bi ]
        | bi <- allBuildInfo pkg
        ]

    -- paths that are allowed to be absolute
    absPaths :: [(FilePath, String, PathKind)]
    absPaths = concat
      [ [ (path, "includes",       PathKindFile)      | path <- includes     bi ] ++
        [ (path, "include-dirs",   PathKindDirectory) | path <- includeDirs  bi ] ++
        [ (path, "extra-lib-dirs", PathKindDirectory) | path <- extraLibDirs bi ]
      | bi <- allBuildInfo pkg
      ]

--TODO: check sets of paths that would be interpreted differently between Unix
-- and windows, ie case-sensitive or insensitive. Things that might clash, or
-- conversely be distinguished.

--TODO: use the tar path checks on all the above paths

-- | Check that the package declares the version in the @\"cabal-version\"@
-- field correctly.
--
checkCabalVersion :: PackageDescription -> [PackageCheck]
checkCabalVersion pkg =
  catMaybes [

    -- check use of test suite sections
    checkVersion CabalSpecV1_8 (not (null $ testSuites pkg)) $
      PackageDistInexcusable $
           "The 'test-suite' section is new in Cabal 1.10. "
        ++ "Unfortunately it messes up the parser in older Cabal versions "
        ++ "so you must specify at least 'cabal-version: >= 1.8', but note "
        ++ "that only Cabal 1.10 and later can actually run such test suites."

    -- check use of default-language field
    -- note that we do not need to do an equivalent check for the
    -- other-language field since that one does not change behaviour
  , checkVersion CabalSpecV1_10 (any isJust (buildInfoField defaultLanguage)) $
      PackageBuildWarning $
           "To use the 'default-language' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.10'."

  , check (specVersion pkg >= CabalSpecV1_10 && specVersion pkg < CabalSpecV3_4
           && (any isNothing (buildInfoField defaultLanguage))) $
      PackageBuildWarning $
           "Packages using 'cabal-version: >= 1.10' and before 'cabal-version: 3.4' must specify the "
        ++ "'default-language' field for each component (e.g. Haskell98 or "
        ++ "Haskell2010). If a component uses different languages in "
        ++ "different modules then list the other ones in the "
        ++ "'other-languages' field."

  , checkVersion CabalSpecV1_18
    (not . null $ extraDocFiles pkg) $
      PackageDistInexcusable $
           "To use the 'extra-doc-files' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.18'."

  , checkVersion CabalSpecV2_0
    (not (null (subLibraries pkg))) $
      PackageDistInexcusable $
           "To use multiple 'library' sections or a named library section "
        ++ "the package needs to specify at least 'cabal-version: 2.0'."

    -- check use of reexported-modules sections
  , checkVersion CabalSpecV1_22
    (any (not.null.reexportedModules) (allLibraries pkg)) $
      PackageDistInexcusable $
           "To use the 'reexported-module' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.22'."

    -- check use of thinning and renaming
  , checkVersion CabalSpecV2_0 usesBackpackIncludes $
      PackageDistInexcusable $
           "To use the 'mixins' field the package needs to specify "
        ++ "at least 'cabal-version: 2.0'."

    -- check use of 'extra-framework-dirs' field
  , checkVersion CabalSpecV1_24 (any (not . null) (buildInfoField extraFrameworkDirs)) $
      -- Just a warning, because this won't break on old Cabal versions.
      PackageDistSuspiciousWarn $
           "To use the 'extra-framework-dirs' field the package needs to specify"
        ++ " at least 'cabal-version: >= 1.24'."

    -- check use of default-extensions field
    -- don't need to do the equivalent check for other-extensions
  , checkVersion CabalSpecV1_10 (any (not . null) (buildInfoField defaultExtensions)) $
      PackageBuildWarning $
           "To use the 'default-extensions' field the package needs to specify "
        ++ "at least 'cabal-version: >= 1.10'."

    -- check use of extensions field
  , check (specVersion pkg >= CabalSpecV1_10
           && (any (not . null) (buildInfoField oldExtensions))) $
      PackageBuildWarning $
           "For packages using 'cabal-version: >= 1.10' the 'extensions' "
        ++ "field is deprecated. The new 'default-extensions' field lists "
        ++ "extensions that are used in all modules in the component, while "
        ++ "the 'other-extensions' field lists extensions that are used in "
        ++ "some modules, e.g. via the {-# LANGUAGE #-} pragma."

  , checkVersion CabalSpecV3_0 (any (not . null)
                        (concatMap buildInfoField
                         [ asmSources
                         , cmmSources
                         , extraBundledLibs
                         , extraLibFlavours ])) $
      PackageDistInexcusable $
           "The use of 'asm-sources', 'cmm-sources', 'extra-bundled-libraries' "
        ++ " and 'extra-library-flavours' requires the package "
        ++ " to specify at least 'cabal-version: 3.0'."

  , checkVersion CabalSpecV3_0 (any (not . null) $ buildInfoField extraDynLibFlavours) $
      PackageDistInexcusable $
           "The use of 'extra-dynamic-library-flavours' requires the package "
        ++ " to specify at least 'cabal-version: 3.0'. The flavours are: "
        ++ commaSep [ flav
                    | flavs <- buildInfoField extraDynLibFlavours
                    , flav <- flavs ]

  , checkVersion CabalSpecV2_2 (any (not . null)
                        (buildInfoField virtualModules)) $
      PackageDistInexcusable $
           "The use of 'virtual-modules' requires the package "
        ++ " to specify at least 'cabal-version: 2.2'."

    -- check use of "source-repository" section
  , checkVersion CabalSpecV1_6 (not (null (sourceRepos pkg))) $
      PackageDistInexcusable $
           "The 'source-repository' section is new in Cabal 1.6. "
        ++ "Unfortunately it messes up the parser in earlier Cabal versions "
        ++ "so you need to specify 'cabal-version: >= 1.6'."

    -- check for new language extensions
  , checkVersion CabalSpecV1_4 (not (null mentionedExtensionsThatNeedCabal12)) $
      PackageDistInexcusable $
           "Unfortunately the language extensions "
        ++ commaSep (map (quote . prettyShow) mentionedExtensionsThatNeedCabal12)
        ++ " break the parser in earlier Cabal versions so you need to "
        ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then you may be able to "
        ++ "use an equivalent compiler-specific flag."

  , checkVersion CabalSpecV1_4 (not (null mentionedExtensionsThatNeedCabal14)) $
      PackageDistInexcusable $
           "Unfortunately the language extensions "
        ++ commaSep (map (quote . prettyShow) mentionedExtensionsThatNeedCabal14)
        ++ " break the parser in earlier Cabal versions so you need to "
        ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
        ++ "compatibility with earlier Cabal versions then you may be able to "
        ++ "use an equivalent compiler-specific flag."

  , check (specVersion pkg >= CabalSpecV1_24
           && isNothing (setupBuildInfo pkg)
           && buildType pkg == Custom) $
      PackageBuildWarning $
           "Packages using 'cabal-version: >= 1.24' with 'build-type: Custom' "
        ++ "must use a 'custom-setup' section with a 'setup-depends' field "
        ++ "that specifies the dependencies of the Setup.hs script itself. "
        ++ "The 'setup-depends' field uses the same syntax as 'build-depends', "
        ++ "so a simple example would be 'setup-depends: base, Cabal'."

  , check (specVersion pkg < CabalSpecV1_24
           && isNothing (setupBuildInfo pkg)
           && buildType pkg == Custom) $
      PackageDistSuspiciousWarn $
           "From version 1.24 cabal supports specifying explicit dependencies "
        ++ "for Custom setup scripts. Consider using cabal-version >= 1.24 and "
        ++ "adding a 'custom-setup' section with a 'setup-depends' field "
        ++ "that specifies the dependencies of the Setup.hs script itself. "
        ++ "The 'setup-depends' field uses the same syntax as 'build-depends', "
        ++ "so a simple example would be 'setup-depends: base, Cabal'."

  , check (specVersion pkg >= CabalSpecV2_0
           && elem (autogenPathsModuleName pkg) allModuleNames
           && not (elem (autogenPathsModuleName pkg) allModuleNamesAutogen) ) $
      PackageDistInexcusable $
           "Packages using 'cabal-version: 2.0' and the autogenerated "
        ++ "module Paths_* must include it also on the 'autogen-modules' field "
        ++ "besides 'exposed-modules' and 'other-modules'. This specifies that "
        ++ "the module does not come with the package and is generated on "
        ++ "setup. Modules built with a custom Setup.hs script also go here "
        ++ "to ensure that commands like sdist don't fail."

  ]
  where
    -- Perform a check on packages that use a version of the spec less than
    -- the version given. This is for cases where a new Cabal version adds
    -- a new feature and we want to check that it is not used prior to that
    -- version.
    checkVersion :: CabalSpecVersion -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= ver = Nothing
      | otherwise              = check cond pc

    buildInfoField field         = map field (allBuildInfo pkg)

    usesBackpackIncludes = any (not . null . mixins) (allBuildInfo pkg)

    mentionedExtensions = [ ext | bi <- allBuildInfo pkg
                                , ext <- allExtensions bi ]
    mentionedExtensionsThatNeedCabal12 =
      nub (filter (`elem` compatExtensionsExtra) mentionedExtensions)

    -- As of Cabal-1.4 we can add new extensions without worrying about
    -- breaking old versions of cabal.
    mentionedExtensionsThatNeedCabal14 =
      nub (filter (`notElem` compatExtensions) mentionedExtensions)

    -- The known extensions in Cabal-1.2.3
    compatExtensions =
      map EnableExtension
      [ OverlappingInstances, UndecidableInstances, IncoherentInstances
      , RecursiveDo, ParallelListComp, MultiParamTypeClasses
      , FunctionalDependencies, Rank2Types
      , RankNTypes, PolymorphicComponents, ExistentialQuantification
      , ScopedTypeVariables, ImplicitParams, FlexibleContexts
      , FlexibleInstances, EmptyDataDecls, CPP, BangPatterns
      , TypeSynonymInstances, TemplateHaskell, ForeignFunctionInterface
      , Arrows, Generics, NamedFieldPuns, PatternGuards
      , GeneralizedNewtypeDeriving, ExtensibleRecords, RestrictedTypeSynonyms
      , HereDocuments] ++
      map DisableExtension
      [MonomorphismRestriction, ImplicitPrelude] ++
      compatExtensionsExtra

    -- The extra known extensions in Cabal-1.2.3 vs Cabal-1.1.6
    -- (Cabal-1.1.6 came with ghc-6.6. Cabal-1.2 came with ghc-6.8)
    compatExtensionsExtra =
      map EnableExtension
      [ KindSignatures, MagicHash, TypeFamilies, StandaloneDeriving
      , UnicodeSyntax, PatternSignatures, UnliftedFFITypes, LiberalTypeSynonyms
      , TypeOperators, RecordWildCards, RecordPuns, DisambiguateRecordFields
      , OverloadedStrings, GADTs, RelaxedPolyRec
      , ExtendedDefaultRules, UnboxedTuples, DeriveDataTypeable
      , ConstrainedClassMethods
      ] ++
      map DisableExtension
      [MonoPatBinds]

    allModuleNames =
         (case library pkg of
           Nothing -> []
           (Just lib) -> explicitLibModules lib
         )
      ++ concatMap otherModules (allBuildInfo pkg)

    allModuleNamesAutogen = concatMap autogenModules (allBuildInfo pkg)

-- ------------------------------------------------------------
-- * Checks on the GenericPackageDescription
-- ------------------------------------------------------------

-- | Check the build-depends fields for any weirdness or bad practice.
--
checkPackageVersions :: GenericPackageDescription -> [PackageCheck]
checkPackageVersions pkg =
  catMaybes [

    -- Check that the version of base is bounded above.
    -- For example this bans "build-depends: base >= 3".
    -- It should probably be "build-depends: base >= 3 && < 4"
    -- which is the same as  "build-depends: base == 3.*"
    check (not (boundedAbove baseDependency)) $
      PackageDistInexcusable $
           "The dependency 'build-depends: base' does not specify an upper "
        ++ "bound on the version number. Each major release of the 'base' "
        ++ "package changes the API in various ways and most packages will "
        ++ "need some changes to compile with it. The recommended practice "
        ++ "is to specify an upper bound on the version of the 'base' "
        ++ "package. This ensures your package will continue to build when a "
        ++ "new major version of the 'base' package is released. If you are "
        ++ "not sure what upper bound to use then use the next  major "
        ++ "version. For example if you have tested your package with 'base' "
        ++ "version 4.5 and 4.6 then use 'build-depends: base >= 4.5 && < 4.7'."

  ]
  where
    -- TODO: What we really want to do is test if there exists any
    -- configuration in which the base version is unbounded above.
    -- However that's a bit tricky because there are many possible
    -- configurations. As a cheap easy and safe approximation we will
    -- pick a single "typical" configuration and check if that has an
    -- open upper bound. To get a typical configuration we finalise
    -- using no package index and the current platform.
    finalised = finalizePD
                              mempty defaultComponentRequestedSpec (const True)
                              buildPlatform
                              (unknownCompilerInfo
                                (CompilerId buildCompilerFlavor nullVersion)
                                NoAbiTag)
                              [] pkg
    baseDependency = case finalised of
      Right (pkg', _) | not (null baseDeps) ->
          foldr intersectVersionRanges anyVersion baseDeps
        where
          baseDeps =
            [ vr | Dependency pname vr _ <- allBuildDepends pkg'
                 , pname == mkPackageName "base" ]

      -- Just in case finalizePD fails for any reason,
      -- or if the package doesn't depend on the base package at all,
      -- then we will just skip the check, since boundedAbove noVersion = True
      _          -> noVersion

    boundedAbove :: VersionRange -> Bool
    boundedAbove vr = case asVersionIntervals vr of
      []     -> True -- this is the inconsistent version range.
      (x:xs) -> case last (x:|xs) of
        (_,   UpperBound _ _) -> True
        (_, NoUpperBound    ) -> False


checkConditionals :: GenericPackageDescription -> [PackageCheck]
checkConditionals pkg =
  catMaybes [

    check (not $ null unknownOSs) $
      PackageDistInexcusable $
           "Unknown operating system name "
        ++ commaSep (map quote unknownOSs)

  , check (not $ null unknownArches) $
      PackageDistInexcusable $
           "Unknown architecture name "
        ++ commaSep (map quote unknownArches)

  , check (not $ null unknownImpls) $
      PackageDistInexcusable $
           "Unknown compiler name "
        ++ commaSep (map quote unknownImpls)
  ]
  where
    unknownOSs    = [ os   | OS   (OtherOS os)           <- conditions ]
    unknownArches = [ arch | Arch (OtherArch arch)       <- conditions ]
    unknownImpls  = [ impl | Impl (OtherCompiler impl) _ <- conditions ]
    conditions = concatMap fvs (maybeToList (condLibrary pkg))
              ++ concatMap (fvs . snd) (condSubLibraries pkg)
              ++ concatMap (fvs . snd) (condForeignLibs pkg)
              ++ concatMap (fvs . snd) (condExecutables pkg)
              ++ concatMap (fvs . snd) (condTestSuites pkg)
              ++ concatMap (fvs . snd) (condBenchmarks pkg)
    fvs (CondNode _ _ ifs) = concatMap compfv ifs -- free variables
    compfv (CondBranch c ct mct) = condfv c ++ fvs ct ++ maybe [] fvs mct
    condfv c = case c of
      Var v      -> [v]
      Lit _      -> []
      CNot c1    -> condfv c1
      COr  c1 c2 -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2

checkFlagNames :: GenericPackageDescription -> [PackageCheck]
checkFlagNames gpd
    | null invalidFlagNames = []
    | otherwise             = [ PackageDistInexcusable
        $ "Suspicious flag names: " ++ unwords invalidFlagNames ++ ". "
        ++ "To avoid ambiguity in command line interfaces, flag shouldn't "
        ++ "start with a dash. Also for better compatibility, flag names "
        ++ "shouldn't contain non-ascii characters."
        ]
  where
    invalidFlagNames =
        [ fn
        | flag <- genPackageFlags gpd
        , let fn = unFlagName (flagName flag)
        , invalidFlagName fn
        ]
    -- starts with dash
    invalidFlagName ('-':_) = True
    -- mon ascii letter
    invalidFlagName cs = any (not . isAscii) cs

checkUnusedFlags :: GenericPackageDescription -> [PackageCheck]
checkUnusedFlags gpd
    | declared == used = []
    | otherwise        = [ PackageDistSuspicious
        $ "Declared and used flag sets differ: "
        ++ s declared ++ " /= " ++ s used ++ ". "
        ]
  where
    s :: Set.Set FlagName -> String
    s = commaSep . map unFlagName . Set.toList

    declared :: Set.Set FlagName
    declared = toSetOf (L.genPackageFlags . traverse . L.flagName) gpd

    used :: Set.Set FlagName
    used = mconcat
        [ toSetOf (L.condLibrary      . traverse      . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condSubLibraries . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condForeignLibs  . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condExecutables  . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condTestSuites   . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condBenchmarks   . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        ]

checkUnicodeXFields :: GenericPackageDescription -> [PackageCheck]
checkUnicodeXFields gpd
    | null nonAsciiXFields = []
    | otherwise            = [ PackageDistInexcusable
        $ "Non ascii custom fields: " ++ unwords nonAsciiXFields ++ ". "
        ++ "For better compatibility, custom field names "
        ++ "shouldn't contain non-ascii characters."
        ]
  where
    nonAsciiXFields :: [String]
    nonAsciiXFields = [ n | (n, _) <- xfields, any (not . isAscii) n ]

    xfields :: [(String,String)]
    xfields = DList.runDList $ mconcat
        [ toDListOf (L.packageDescription . L.customFieldsPD . traverse) gpd
        , toDListOf (L.traverseBuildInfos . L.customFieldsBI . traverse) gpd
        ]

-- | cabal-version <2.2 + Paths_module + default-extensions: doesn't build.
checkPathsModuleExtensions :: PackageDescription -> [PackageCheck]
checkPathsModuleExtensions pd
    | specVersion pd >= CabalSpecV2_2 = []
    | any checkBI (allBuildInfo pd) || any checkLib (allLibraries pd)
        = return $ PackageBuildImpossible $ unwords
            [ "The package uses RebindableSyntax with OverloadedStrings or OverloadedLists"
            , "in default-extensions, and also Paths_ autogen module."
            , "That configuration is known to cause compile failures with Cabal < 2.2."
            , "To use these default-extensions with Paths_ autogen module"
            , "specify at least 'cabal-version: 2.2'."
            ]
    | otherwise = []
  where
    mn = autogenPathsModuleName pd

    checkLib :: Library -> Bool
    checkLib l = mn `elem` exposedModules l && checkExts (l ^. L.defaultExtensions)

    checkBI :: BuildInfo -> Bool
    checkBI bi =
        (mn `elem` otherModules bi || mn `elem` autogenModules bi) &&
        checkExts (bi ^. L.defaultExtensions)

    checkExts exts = rebind `elem` exts && (strings `elem` exts || lists `elem` exts)
      where
        rebind  = EnableExtension RebindableSyntax
        strings = EnableExtension OverloadedStrings
        lists   = EnableExtension OverloadedLists

-- | Checks GHC options from all ghc-*-options fields from the given BuildInfo
-- and reports flags that are OK during development process, but are
-- unacceptable in a distrubuted package
checkDevelopmentOnlyFlagsBuildInfo :: BuildInfo -> [PackageCheck]
checkDevelopmentOnlyFlagsBuildInfo bi =
    checkDevelopmentOnlyFlagsOptions "ghc-options" (hcOptions GHC bi)
 ++ checkDevelopmentOnlyFlagsOptions "ghc-prof-options" (hcProfOptions GHC bi)
 ++ checkDevelopmentOnlyFlagsOptions "ghc-shared-options" (hcSharedOptions GHC bi)

-- | Checks the given list of flags belonging to the given field and reports
-- flags that are OK during development process, but are unacceptable in a
-- distributed package
checkDevelopmentOnlyFlagsOptions :: String -> [String] -> [PackageCheck]
checkDevelopmentOnlyFlagsOptions fieldName ghcOptions =
  catMaybes [

    check has_WerrorWall $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -Wall -Werror' makes the package very easy to "
        ++ "break with future GHC versions because new GHC versions often "
        ++ "add new warnings. Use just '" ++ fieldName ++ ": -Wall' instead."
        ++ extraExplanation

  , check (not has_WerrorWall && has_Werror) $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -Werror' makes the package easy to "
        ++ "break with future GHC versions because new GHC versions often "
        ++ "add new warnings. "
        ++ extraExplanation

  , check (has_J) $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -j[N]' can make sense for specific user's setup,"
        ++ " but it is not appropriate for a distributed package."
        ++ extraExplanation

  , checkFlags ["-fdefer-type-errors"] $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -fdefer-type-errors' is fine during development but "
        ++ "is not appropriate for a distributed package. "
        ++ extraExplanation

    -- -dynamic is not a debug flag
  , check (any (\opt -> "-d" `isPrefixOf` opt && opt /= "-dynamic")
           ghcOptions) $
      PackageDistInexcusable $
           "'" ++ fieldName ++ ": -d*' debug flags are not appropriate "
        ++ "for a distributed package. "
        ++ extraExplanation

  , checkFlags ["-fprof-auto", "-fprof-auto-top", "-fprof-auto-calls",
               "-fprof-cafs", "-fno-prof-count-entries",
               "-auto-all", "-auto", "-caf-all"] $
      PackageDistSuspicious $
           "'" ++ fieldName ++ ": -fprof*' profiling flags are typically not "
        ++ "appropriate for a distributed library package. These flags are "
        ++ "useful to profile this package, but when profiling other packages "
        ++ "that use this one these flags clutter the profile output with "
        ++ "excessive detail. If you think other packages really want to see "
        ++ "cost centres from this package then use '-fprof-auto-exported' "
        ++ "which puts cost centres only on exported functions. "
        ++ extraExplanation
  ]
  where
    extraExplanation =
         " Alternatively, if you want to use this, make it conditional based "
      ++ "on a Cabal configuration flag (with 'manual: True' and 'default: "
      ++ "False') and enable that flag during development."

    has_WerrorWall   = has_Werror && ( has_Wall || has_W )
    has_Werror       = "-Werror" `elem` ghcOptions
    has_Wall         = "-Wall"   `elem` ghcOptions
    has_W            = "-W"      `elem` ghcOptions
    has_J            = any
                         (\o -> case o of
                           "-j"                -> True
                           ('-' : 'j' : d : _) -> isDigit d
                           _                   -> False
                         )
                         ghcOptions
    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) ghcOptions)

checkDevelopmentOnlyFlags :: GenericPackageDescription -> [PackageCheck]
checkDevelopmentOnlyFlags pkg =
    concatMap checkDevelopmentOnlyFlagsBuildInfo
              [ bi
              | (conditions, bi) <- allConditionalBuildInfo
              , not (any guardedByManualFlag conditions) ]
  where
    guardedByManualFlag = definitelyFalse

    -- We've basically got three-values logic here: True, False or unknown
    -- hence this pattern to propagate the unknown cases properly.
    definitelyFalse (Var (PackageFlag n)) = maybe False not (Map.lookup n manualFlags)
    definitelyFalse (Var _)        = False
    definitelyFalse (Lit  b)       = not b
    definitelyFalse (CNot c)       = definitelyTrue c
    definitelyFalse (COr  c1 c2)   = definitelyFalse c1 && definitelyFalse c2
    definitelyFalse (CAnd c1 c2)   = definitelyFalse c1 || definitelyFalse c2

    definitelyTrue (Var (PackageFlag n)) = fromMaybe False (Map.lookup n manualFlags)
    definitelyTrue (Var _)        = False
    definitelyTrue (Lit  b)       = b
    definitelyTrue (CNot c)       = definitelyFalse c
    definitelyTrue (COr  c1 c2)   = definitelyTrue c1 || definitelyTrue c2
    definitelyTrue (CAnd c1 c2)   = definitelyTrue c1 && definitelyTrue c2

    manualFlags = Map.fromList
                    [ (flagName flag, flagDefault flag)
                    | flag <- genPackageFlags pkg
                    , flagManual flag ]

    allConditionalBuildInfo :: [([Condition ConfVar], BuildInfo)]
    allConditionalBuildInfo =
        concatMap (collectCondTreePaths libBuildInfo)
                  (maybeToList (condLibrary pkg))

     ++ concatMap (collectCondTreePaths libBuildInfo . snd)
                  (condSubLibraries pkg)

     ++ concatMap (collectCondTreePaths buildInfo . snd)
                  (condExecutables pkg)

     ++ concatMap (collectCondTreePaths testBuildInfo . snd)
                  (condTestSuites pkg)

     ++ concatMap (collectCondTreePaths benchmarkBuildInfo . snd)
                  (condBenchmarks pkg)

    -- get all the leaf BuildInfo, paired up with the path (in the tree sense)
    -- of if-conditions that guard it
    collectCondTreePaths :: (a -> b)
                         -> CondTree v c a
                         -> [([Condition v], b)]
    collectCondTreePaths mapData = go []
      where
        go conditions condNode =
            -- the data at this level in the tree:
            (reverse conditions, mapData (condTreeData condNode))

          : concat
            [ go (condition:conditions) ifThen
            | (CondBranch condition ifThen _) <- condTreeComponents condNode ]

         ++ concat
            [ go (condition:conditions) elseThen
            | (CondBranch condition _ (Just elseThen)) <- condTreeComponents condNode ]


-- ------------------------------------------------------------
-- * Checks involving files in the package
-- ------------------------------------------------------------

-- | Sanity check things that requires IO. It looks at the files in the
-- package and expects to find the package unpacked in at the given file path.
--
checkPackageFiles :: Verbosity -> PackageDescription -> FilePath -> IO [PackageCheck]
checkPackageFiles verbosity pkg root = do
  contentChecks <- checkPackageContent checkFilesIO pkg
  preDistributionChecks <- checkPackageFilesPreDistribution verbosity pkg root
  -- Sort because different platforms will provide files from
  -- `getDirectoryContents` in different orders, and we'd like to be
  -- stable for test output.
  return (sort contentChecks ++ sort preDistributionChecks)
  where
    checkFilesIO = CheckPackageContentOps {
      doesFileExist        = System.doesFileExist                  . relative,
      doesDirectoryExist   = System.doesDirectoryExist             . relative,
      getDirectoryContents = System.Directory.getDirectoryContents . relative,
      getFileContents      = BS.readFile                           . relative
    }
    relative path = root </> path

-- | A record of operations needed to check the contents of packages.
-- Used by 'checkPackageContent'.
--
data CheckPackageContentOps m = CheckPackageContentOps {
    doesFileExist        :: FilePath -> m Bool,
    doesDirectoryExist   :: FilePath -> m Bool,
    getDirectoryContents :: FilePath -> m [FilePath],
    getFileContents      :: FilePath -> m BS.ByteString
  }

-- | Sanity check things that requires looking at files in the package.
-- This is a generalised version of 'checkPackageFiles' that can work in any
-- monad for which you can provide 'CheckPackageContentOps' operations.
--
-- The point of this extra generality is to allow doing checks in some virtual
-- file system, for example a tarball in memory.
--
checkPackageContent :: (Monad m, Applicative m)
                    => CheckPackageContentOps m
                    -> PackageDescription
                    -> m [PackageCheck]
checkPackageContent ops pkg = do
  cabalBomError   <- checkCabalFileBOM    ops
  cabalNameError  <- checkCabalFileName   ops pkg
  licenseErrors   <- checkLicensesExist   ops pkg
  setupError      <- checkSetupExists     ops pkg
  configureError  <- checkConfigureExists ops pkg
  localPathErrors <- checkLocalPathsExist ops pkg
  vcsLocation     <- checkMissingVcsInfo  ops pkg

  return $ licenseErrors
        ++ catMaybes [cabalBomError, cabalNameError, setupError, configureError]
        ++ localPathErrors
        ++ vcsLocation

checkCabalFileBOM :: Monad m => CheckPackageContentOps m
                  -> m (Maybe PackageCheck)
checkCabalFileBOM ops = do
  epdfile <- findPackageDesc ops
  case epdfile of
    -- MASSIVE HACK.  If the Cabal file doesn't exist, that is
    -- a very strange situation to be in, because the driver code
    -- in 'Distribution.Setup' ought to have noticed already!
    -- But this can be an issue, see #3552 and also when
    -- --cabal-file is specified.  So if you can't find the file,
    -- just don't bother with this check.
    Left _       -> return $ Nothing
    Right pdfile -> (flip check pc . BS.isPrefixOf bomUtf8)
                    `liftM` (getFileContents ops pdfile)
      where pc = PackageDistInexcusable $
                 pdfile ++ " starts with an Unicode byte order mark (BOM)."
                 ++ " This may cause problems with older cabal versions."

  where
    bomUtf8 :: BS.ByteString
    bomUtf8 = BS.pack [0xef,0xbb,0xbf] -- U+FEFF encoded as UTF8

checkCabalFileName :: Monad m => CheckPackageContentOps m
                 -> PackageDescription
                 -> m (Maybe PackageCheck)
checkCabalFileName ops pkg = do
  -- findPackageDesc already takes care to detect missing/multiple
  -- .cabal files; we don't include this check in 'findPackageDesc' in
  -- order not to short-cut other checks which call 'findPackageDesc'
  epdfile <- findPackageDesc ops
  case epdfile of
    -- see "MASSIVE HACK" note in 'checkCabalFileBOM'
    Left _       -> return Nothing
    Right pdfile
      | takeFileName pdfile == expectedCabalname -> return Nothing
      | otherwise -> return $ Just $ PackageDistInexcusable $
                 "The filename " ++ pdfile ++ " does not match package name " ++
                 "(expected: " ++ expectedCabalname ++ ")"
  where
    pkgname = unPackageName . packageName $ pkg
    expectedCabalname = pkgname <.> "cabal"


-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.  Like 'Distribution.Simple.Utils.findPackageDesc',
-- but generalized over monads.
findPackageDesc :: Monad m => CheckPackageContentOps m
                 -> m (Either PackageCheck FilePath) -- ^<pkgname>.cabal
findPackageDesc ops
 = do let dir = "."
      files <- getDirectoryContents ops dir
      -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
      -- file we filter to exclude dirs and null base file names:
      cabalFiles <- filterM (doesFileExist ops)
                       [ dir </> file
                       | file <- files
                       , let (name, ext) = splitExtension file
                       , not (null name) && ext == ".cabal" ]
      case cabalFiles of
        []          -> return (Left $ PackageBuildImpossible noDesc)
        [cabalFile] -> return (Right cabalFile)
        multiple    -> return (Left $ PackageBuildImpossible
                               $ multiDesc multiple)

  where
    noDesc :: String
    noDesc = "No cabal file found.\n"
             ++ "Please create a package description file <pkgname>.cabal"

    multiDesc :: [String] -> String
    multiDesc l = "Multiple cabal files found while checking.\n"
                  ++ "Please use only one of: "
                  ++ intercalate ", " l

checkLicensesExist :: (Monad m, Applicative m)
                   => CheckPackageContentOps m
                   -> PackageDescription
                   -> m [PackageCheck]
checkLicensesExist ops pkg = do
    exists <- traverse (doesFileExist ops) (licenseFiles pkg)
    return
      [ PackageBuildWarning $
           "The '" ++ fieldname ++ "' field refers to the file "
        ++ quote file ++ " which does not exist."
      | (file, False) <- zip (licenseFiles pkg) exists ]
  where
    fieldname | length (licenseFiles pkg) == 1 = "license-file"
              | otherwise                      = "license-files"

checkSetupExists :: Monad m => CheckPackageContentOps m
                 -> PackageDescription
                 -> m (Maybe PackageCheck)
checkSetupExists ops pkg = do
  let simpleBuild = buildType pkg == Simple
  hsexists  <- doesFileExist ops "Setup.hs"
  lhsexists <- doesFileExist ops "Setup.lhs"
  return $ check (not simpleBuild && not hsexists && not lhsexists) $
    PackageDistInexcusable $
      "The package is missing a Setup.hs or Setup.lhs script."

checkConfigureExists :: Monad m => CheckPackageContentOps m
                     -> PackageDescription
                     -> m (Maybe PackageCheck)
checkConfigureExists ops pd
  | buildType pd == Configure = do
      exists <- doesFileExist ops "configure"
      return $ check (not exists) $
        PackageBuildWarning $
          "The 'build-type' is 'Configure' but there is no 'configure' script. "
          ++ "You probably need to run 'autoreconf -i' to generate it."
  | otherwise = return Nothing

checkLocalPathsExist :: Monad m => CheckPackageContentOps m
                     -> PackageDescription
                     -> m [PackageCheck]
checkLocalPathsExist ops pkg = do
  let dirs = [ (dir, kind)
             | bi <- allBuildInfo pkg
             , (dir, kind) <-
                  [ (dir, "extra-lib-dirs") | dir <- extraLibDirs bi ]
               ++ [ (dir, "extra-framework-dirs")
                  | dir <- extraFrameworkDirs  bi ]
               ++ [ (dir, "include-dirs")   | dir <- includeDirs  bi ]
               ++ [ (dir, "hs-source-dirs") | dir <- hsSourceDirs bi ]
             , isRelativeOnAnyPlatform dir ]
  missing <- filterM (liftM not . doesDirectoryExist ops . fst) dirs
  return [ PackageBuildWarning {
             explanation = quote (kind ++ ": " ++ dir)
                        ++ " directory does not exist."
           }
         | (dir, kind) <- missing ]

checkMissingVcsInfo :: (Monad m, Applicative m)
                    => CheckPackageContentOps m
                    -> PackageDescription
                    -> m [PackageCheck]
checkMissingVcsInfo ops pkg | null (sourceRepos pkg) = do
    vcsInUse <- liftM or $ traverse (doesDirectoryExist ops) repoDirnames
    if vcsInUse
      then return [ PackageDistSuspicious message ]
      else return []
  where
    repoDirnames = [ dirname | repo    <- knownRepoTypes
                             , dirname <- repoTypeDirname repo]
    message  = "When distributing packages it is encouraged to specify source "
            ++ "control information in the .cabal file using one or more "
            ++ "'source-repository' sections. See the Cabal user guide for "
            ++ "details."

checkMissingVcsInfo _ _ = return []

repoTypeDirname :: KnownRepoType -> [FilePath]
repoTypeDirname Darcs     = ["_darcs"]
repoTypeDirname Git       = [".git"]
repoTypeDirname SVN       = [".svn"]
repoTypeDirname CVS       = ["CVS"]
repoTypeDirname Mercurial = [".hg"]
repoTypeDirname GnuArch   = [".arch-params"]
repoTypeDirname Bazaar    = [".bzr"]
repoTypeDirname Monotone  = ["_MTN"]
repoTypeDirname Pijul     = [".pijul"]

-- ------------------------------------------------------------
-- * Checks involving files in the package
-- ------------------------------------------------------------

-- | Check the names of all files in a package for portability problems. This
-- should be done for example when creating or validating a package tarball.
--
checkPackageFileNames :: [FilePath] -> [PackageCheck]
checkPackageFileNames files =
     (take 1 . mapMaybe checkWindowsPath $ files)
  ++ (take 1 . mapMaybe checkTarPath     $ files)
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
    Right (h:rest) -> case pack prefixMax remainder of
      Left err         -> Just err
      Right []         -> Nothing
      Right (_:_)      -> Just noSplit
     where
        -- drop the '/' between the name and prefix:
        remainder = safeInit h : rest

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

-- --------------------------------------------------------------
-- * Checks for missing content and other pre-distribution checks
-- --------------------------------------------------------------

-- | Similar to 'checkPackageContent', 'checkPackageFilesPreDistribution'
-- inspects the files included in the package, but is primarily looking for
-- files in the working tree that may have been missed or other similar
-- problems that can only be detected pre-distribution.
--
-- Because Hackage necessarily checks the uploaded tarball, it is too late to
-- check these on the server; these checks only make sense in the development
-- and package-creation environment. Hence we can use IO, rather than needing
-- to pass a 'CheckPackageContentOps' dictionary around.
checkPackageFilesPreDistribution :: Verbosity -> PackageDescription -> FilePath -> IO [PackageCheck]
-- Note: this really shouldn't return any 'Inexcusable' warnings,
-- because that will make us say that Hackage would reject the package.
-- But, because Hackage doesn't run these tests, that will be a lie!
checkPackageFilesPreDistribution = checkGlobFiles

-- | Discover problems with the package's wildcards.
checkGlobFiles :: Verbosity
               -> PackageDescription
               -> FilePath
               -> IO [PackageCheck]
checkGlobFiles verbosity pkg root =
  fmap concat $ for allGlobs $ \(field, dir, glob) ->
    -- Note: we just skip over parse errors here; they're reported elsewhere.
    case parseFileGlob (specVersion pkg) glob of
      Left _ -> return []
      Right parsedGlob -> do
        results <- runDirFileGlob verbosity (root </> dir) parsedGlob
        let individualWarnings = results >>= getWarning field glob
            noMatchesWarning =
              [ PackageDistSuspiciousWarn $
                     "In '" ++ field ++ "': the pattern '" ++ glob ++ "' does not"
                  ++ " match any files."
              | all (not . suppressesNoMatchesWarning) results
              ]
        return (noMatchesWarning ++ individualWarnings)
  where
    adjustedDataDir = if null (dataDir pkg) then "." else dataDir pkg
    allGlobs = concat
      [ (,,) "extra-source-files" "." <$> extraSrcFiles pkg
      , (,,) "extra-doc-files" "." <$> extraDocFiles pkg
      , (,,) "data-files" adjustedDataDir <$> dataFiles pkg
      ]

    -- If there's a missing directory in play, since our globs don't
    -- (currently) support disjunction, that will always mean there are no
    -- matches. The no matches error in this case is strictly less informative
    -- than the missing directory error, so sit on it.
    suppressesNoMatchesWarning (GlobMatch _) = True
    suppressesNoMatchesWarning (GlobWarnMultiDot _) = False
    suppressesNoMatchesWarning (GlobMissingDirectory _) = True

    getWarning :: String -> FilePath -> GlobResult FilePath -> [PackageCheck]
    getWarning _ _ (GlobMatch _) =
      []
    -- Before Cabal 2.4, the extensions of globs had to match the file
    -- exactly. This has been relaxed in 2.4 to allow matching only the
    -- suffix. This warning detects when pre-2.4 package descriptions are
    -- omitting files purely because of the stricter check.
    getWarning field glob (GlobWarnMultiDot file) =
      [ PackageDistSuspiciousWarn $
             "In '" ++ field ++ "': the pattern '" ++ glob ++ "' does not"
          ++ " match the file '" ++ file ++ "' because the extensions do not"
          ++ " exactly match (e.g., foo.en.html does not exactly match *.html)."
          ++ " To enable looser suffix-only matching, set 'cabal-version: 2.4' or higher."
      ]
    getWarning field glob (GlobMissingDirectory dir) =
      [ PackageDistSuspiciousWarn $
             "In '" ++ field ++ "': the pattern '" ++ glob ++ "' attempts to"
          ++ " match files in the directory '" ++ dir ++ "', but there is no"
          ++ " directory by that name."
      ]

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

quote :: String -> String
quote s = "'" ++ s ++ "'"

commaSep :: [String] -> String
commaSep = intercalate ", "

dups :: Ord a => [a] -> [a]
dups xs = [ x | (x:_:_) <- group (sort xs) ]

fileExtensionSupportedLanguage :: FilePath -> Bool
fileExtensionSupportedLanguage path =
    isHaskell || isC
  where
    extension = takeExtension path
    isHaskell = extension `elem` [".hs", ".lhs"]
    isC       = isJust (filenameCDialect extension)

-- | Whether a path is a good relative path.
--
-- >>> let test fp = putStrLn $ show (isGoodRelativeDirectoryPath fp) ++ "; " ++ show (isGoodRelativeFilePath fp)
--
-- >>> test "foo/bar/quu"
-- Nothing; Nothing
--
-- Trailing slash is not allowed for files, for directories it is ok.
--
-- >>> test "foo/"
-- Nothing; Just "trailing slash"
--
-- Leading @./@ is fine, but @.@ and @./@ are not valid files.
--
-- >>> traverse_ test [".", "./", "./foo/bar"]
-- Nothing; Just "trailing dot segment"
-- Nothing; Just "trailing slash"
-- Nothing; Nothing
--
-- Lastly, not good file nor directory cases:
--
-- >>> traverse_ test ["", "/tmp/src", "foo//bar", "foo/.", "foo/./bar", "foo/../bar", "foo*bar"]
-- Just "empty path"; Just "empty path"
-- Just "posix absolute path"; Just "posix absolute path"
-- Just "empty path segment"; Just "empty path segment"
-- Just "trailing same directory segment: ."; Just "trailing same directory segment: ."
-- Just "same directory segment: ."; Just "same directory segment: .."
-- Just "parent directory segment: .."; Just "parent directory segment: .."
-- Just "reserved character '*'"; Just "reserved character '*'"
--
-- For the last case, 'isGoodRelativeGlob' doesn't warn:
--
-- >>> traverse_ (print . isGoodRelativeGlob) ["foo/../bar", "foo*bar"]
-- Just "parent directory segment: .."
-- Nothing
--
isGoodRelativeFilePath :: FilePath -> Maybe String
isGoodRelativeFilePath = state0
  where
    -- Reserved characters
    -- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file
    isReserved c = c `elem` "<>:\"\\/|?*"

    -- initial state
    state0 []                    = Just "empty path"
    state0 (c:cs) | c == '.'     = state1 cs
                  | c == '/'     = Just "posix absolute path"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state3 cs

    -- after .
    state1 []                    = Just "trailing dot segment"
    state1 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = state2 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- after ./
    state2 []                    = Just "trailing slash"
    state2 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = Just "empty path segment"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- after non-first segment's .
    state3 []                    = Just "trailing same directory segment: ."
    state3 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = Just "same directory segment: .."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- after non-first segment's ..
    state4 []                    = Just "trailing parent directory segment: .."
    state4 (c:cs) | c == '.'     = state5 cs
                  | c == '/'     = Just "parent directory segment: .."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- in a segment which is ok.
    state5 []                    = Nothing
    state5 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = state2 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

-- | See 'isGoodRelativeFilePath'.
--
-- This is barebones function. We check whether the glob is a valid file
-- by replacing stars @*@ with @x@ses.
isGoodRelativeGlob :: FilePath -> Maybe String
isGoodRelativeGlob = isGoodRelativeFilePath . map f where
    f '*' = 'x'
    f c   = c

-- | See 'isGoodRelativeFilePath'.
isGoodRelativeDirectoryPath :: FilePath -> Maybe String
isGoodRelativeDirectoryPath = state0
  where
    -- Reserved characters
    -- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file
    isReserved c = c `elem` "<>:\"\\/|?*"

    -- initial state
    state0 []                    = Just "empty path"
    state0 (c:cs) | c == '.'     = state5 cs
                  | c == '/'     = Just "posix absolute path"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after ./
    state1 []                    = Nothing -- "./"
    state1 (c:cs) | c == '.'     = state2 cs
                  | c == '/'     = Just "empty path segment"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after non-first setgment's .
    state2 []                    = Just "trailing same directory segment: ."
    state2 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = Just "same directory segment: ."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after non-first segment's ..
    state3 []                    = Just "trailing parent directory segment: ."
    state3 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = Just "parent directory segment: .."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- in a segment which is ok.
    state4 []                    = Nothing
    state4 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = state1 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after .
    state5 []                    = Nothing -- "."
    state5 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = state1 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

-- [Note: Good relative paths]
--
-- Using @kleene@ we can define an extended regex:
--
-- @
-- import Algebra.Lattice
-- import Kleene
-- import Kleene.ERE (ERE (..), intersections)
--
-- data C = CDot | CSlash | COtherReserved | CChar
--   deriving (Eq, Ord, Enum, Bounded, Show)
--
-- reservedR :: ERE C
-- reservedR = notChar CSlash /\ notChar COtherReserved
--
-- pathPieceR :: ERE C
-- pathPieceR = intersections
--     [ plus reservedR
--     , ERENot (string [CDot])
--     , ERENot (string [CDot,CDot])
--     ]
--
-- filePathR :: ERE C
-- filePathR = optional (string [CDot, CSlash]) <> pathPieceR <> star (char CSlash <> pathPieceR)
--
-- dirPathR :: ERE C
-- dirPathR = (char CDot \/ filePathR) <> optional (char CSlash)
--
-- plus :: ERE C -> ERE C
-- plus r = r <> star r
--
-- optional :: ERE C -> ERE C
-- optional r = mempty \/ r
-- @
--
-- Results in following state machine for @filePathR@
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 1
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 1 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 2
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 4 -> \x -> if
--     | x <= CDot           -> 5
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 5+ -> \x -> if
--     | x <= CDot           -> 5
--     | x <= CSlash         -> 2
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 6 -> \_ -> 6 -- black hole
-- @
--
-- and @dirPathR@:
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 5
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 1+ -> \x -> if
--     | x <= CDot           -> 2
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 4+ -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 1
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 5+ -> \x -> if
--     | x <= CDot           -> 3
--     | x <= CSlash         -> 1
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 6 -> \_ -> 6 -- black hole
-- @
