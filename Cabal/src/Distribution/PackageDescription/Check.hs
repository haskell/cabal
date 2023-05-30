{-# LANGUAGE LambdaCase #-}

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
-- different kinds of checks so we can see which ones are appropriate to report
-- in different situations. This code gets used when configuring a package when
-- we consider only basic problems. The higher standard is used when
-- preparing a source tarball and by Hackage when uploading new packages. The
-- reason for this is that we want to hold packages that are expected to be
-- distributed to a higher standard than packages that are only ever expected
-- to be used on the author's own environment.
module Distribution.PackageDescription.Check
  ( -- * Package Checking
    CheckExplanation (..)
  , PackageCheck (..)
  , checkPackage
  , checkConfiguredPackage
  , wrapParseWarning
  , ppPackageCheck
  , isHackageDistError

    -- ** Checking package contents
  , checkPackageFiles
  , checkPackageContent
  , CheckPackageContentOps (..)
  , checkPackageFileNames
  ) where

import Data.Foldable (foldrM)
import Distribution.Compat.Prelude
import Prelude ()

import Data.List (delete, group)
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Parsec.Warning (PWarning, showPWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildPaths (autogenPackageInfoModuleName, autogenPathsModuleName)
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.CCompiler
import Distribution.Simple.Glob
import Distribution.Simple.Utils hiding (findPackageDesc, notice)
import Distribution.System
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.PackageName.Magic
import Distribution.Utils.Generic (isAscii)
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath
  ( makeRelative
  , normalise
  , splitDirectories
  , splitExtension
  , splitPath
  , takeExtension
  , takeFileName
  , (<.>)
  , (</>)
  )

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Distribution.Compat.DList as DList
import qualified Distribution.SPDX as SPDX
import qualified System.Directory as System

import qualified System.Directory (getDirectoryContents)
import qualified System.FilePath.Windows as FilePath.Windows (isValid)

import qualified Data.Set as Set
import qualified Distribution.Utils.ShortText as ShortText

import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens as L

-- $setup
-- >>> import Control.Arrow ((&&&))

-- ------------------------------------------------------------

-- * Warning messages

-- ------------------------------------------------------------

-- | Which stanza does `CheckExplanation` refer to?
data CEType = CETLibrary | CETExecutable | CETTest | CETBenchmark
  deriving (Eq, Ord, Show)

-- | Pretty printing `CEType`.
ppCE :: CEType -> String
ppCE CETLibrary = "library"
ppCE CETExecutable = "executable"
ppCE CETTest = "test suite"
ppCE CETBenchmark = "benchmark"

-- | Which field does `CheckExplanation` refer to?
data CEField
  = CEFCategory
  | CEFMaintainer
  | CEFSynopsis
  | CEFDescription
  | CEFSynOrDesc
  deriving (Eq, Ord, Show)

-- | Pretty printing `CEField`.
ppCEField :: CEField -> String
ppCEField CEFCategory = "category"
ppCEField CEFMaintainer = "maintainer"
ppCEField CEFSynopsis = "synopsis"
ppCEField CEFDescription = "description"
ppCEField CEFSynOrDesc = "synopsis' or 'description"

-- | Explanations of 'PackageCheck`'s errors/warnings.
data CheckExplanation
  = ParseWarning FilePath PWarning
  | NoNameField
  | NoVersionField
  | NoTarget
  | UnnamedInternal
  | DuplicateSections [UnqualComponentName]
  | IllegalLibraryName PackageDescription
  | NoModulesExposed Library
  | SignaturesCabal2
  | AutogenNotExposed
  | AutogenIncludesNotIncluded
  | NoMainIs Executable
  | NoHsLhsMain
  | MainCCabal1_18
  | AutogenNoOther CEType UnqualComponentName
  | AutogenIncludesNotIncludedExe
  | TestsuiteTypeNotKnown TestType
  | TestsuiteNotSupported TestType
  | BenchmarkTypeNotKnown BenchmarkType
  | BenchmarkNotSupported BenchmarkType
  | NoHsLhsMainBench
  | InvalidNameWin PackageDescription
  | ZPrefix
  | NoBuildType
  | NoCustomSetup
  | UnknownCompilers [String]
  | UnknownLanguages [String]
  | UnknownExtensions [String]
  | LanguagesAsExtension [String]
  | DeprecatedExtensions [(Extension, Maybe Extension)]
  | MissingField CEField
  | SynopsisTooLong
  | ShortDesc
  | InvalidTestWith [Dependency]
  | ImpossibleInternalDep [Dependency]
  | ImpossibleInternalExe [ExeDependency]
  | MissingInternalExe [ExeDependency]
  | NONELicense
  | NoLicense
  | AllRightsReservedLicense
  | LicenseMessParse PackageDescription
  | UnrecognisedLicense String
  | UncommonBSD4
  | UnknownLicenseVersion License [Version]
  | NoLicenseFile
  | UnrecognisedSourceRepo String
  | MissingType
  | MissingLocation
  | MissingModule
  | MissingTag
  | SubdirRelPath
  | SubdirGoodRelPath String
  | OptFasm String
  | OptViaC String
  | OptHpc String
  | OptProf String
  | OptO String
  | OptHide String
  | OptMake String
  | OptONot String
  | OptOOne String
  | OptOTwo String
  | OptSplitSections String
  | OptSplitObjs String
  | OptWls String
  | OptExts String
  | OptRts String
  | OptWithRts String
  | COptONumber String String
  | COptCPP String
  | OptAlternatives String String [(String, String)]
  | RelativeOutside String FilePath
  | AbsolutePath String FilePath
  | BadRelativePAth String FilePath String
  | DistPoint (Maybe String) FilePath
  | GlobSyntaxError String String
  | RecursiveGlobInRoot String FilePath
  | InvalidOnWin [FilePath]
  | FilePathTooLong FilePath
  | FilePathNameTooLong FilePath
  | FilePathSplitTooLong FilePath
  | FilePathEmpty
  | CVTestSuite
  | CVDefaultLanguage
  | CVDefaultLanguageComponent
  | CVExtraDocFiles
  | CVMultiLib
  | CVReexported
  | CVMixins
  | CVExtraFrameworkDirs
  | CVDefaultExtensions
  | CVExtensionsDeprecated
  | CVSources
  | CVExtraDynamic [[String]]
  | CVVirtualModules
  | CVSourceRepository
  | CVExtensions CabalSpecVersion [Extension]
  | CVCustomSetup
  | CVExpliticDepsCustomSetup
  | CVAutogenPaths
  | CVAutogenPackageInfo
  | GlobNoMatch String String
  | GlobExactMatch String String FilePath
  | GlobNoDir String String FilePath
  | UnknownOS [String]
  | UnknownArch [String]
  | UnknownCompiler [String]
  | BaseNoUpperBounds
  | MissingUpperBounds [PackageName]
  | SuspiciousFlagName [String]
  | DeclaredUsedFlags (Set FlagName) (Set FlagName)
  | NonASCIICustomField [String]
  | RebindableClashPaths
  | RebindableClashPackageInfo
  | WErrorUnneeded String
  | JUnneeded String
  | FDeferTypeErrorsUnneeded String
  | DynamicUnneeded String
  | ProfilingUnneeded String
  | UpperBoundSetup String
  | DuplicateModule String [ModuleName]
  | PotentialDupModule String [ModuleName]
  | BOMStart FilePath
  | NotPackageName FilePath String
  | NoDesc
  | MultiDesc [String]
  | UnknownFile String (SymbolicPath PackageDir LicenseFile)
  | MissingSetupFile
  | MissingConfigureScript
  | UnknownDirectory String FilePath
  | MissingSourceControl
  | MissingExpectedDocFiles Bool [FilePath]
  | WrongFieldForExpectedDocFiles Bool String [FilePath]
  deriving (Eq, Ord, Show)

-- | Wraps `ParseWarning` into `PackageCheck`.
wrapParseWarning :: FilePath -> PWarning -> PackageCheck
wrapParseWarning fp pw = PackageDistSuspicious (ParseWarning fp pw)

-- TODO: as Jul 2022 there is no severity indication attached PWarnType.
--       Once that is added, we can output something more appropriate
--       than PackageDistSuspicious for every parse warning.
--       (see: Cabal-syntax/src/Distribution/Parsec/Warning.hs)

-- | Pretty printing `CheckExplanation`.
ppExplanation :: CheckExplanation -> String
ppExplanation (ParseWarning fp pp) = showPWarning fp pp
ppExplanation NoNameField = "No 'name' field."
ppExplanation NoVersionField = "No 'version' field."
ppExplanation NoTarget =
  "No executables, libraries, tests, or benchmarks found. Nothing to do."
ppExplanation UnnamedInternal =
  "Found one or more unnamed internal libraries. Only the non-internal"
    ++ " library can have the same name as the package."
ppExplanation (DuplicateSections duplicateNames) =
  "Duplicate sections: "
    ++ commaSep (map unUnqualComponentName duplicateNames)
    ++ ". The name of every library, executable, test suite,"
    ++ " and benchmark section in the package must be unique."
ppExplanation (IllegalLibraryName pkg) =
  "Illegal internal library name "
    ++ prettyShow (packageName pkg)
    ++ ". Internal libraries cannot have the same name as the package."
    ++ " Maybe you wanted a non-internal library?"
    ++ " If so, rewrite the section stanza"
    ++ " from 'library: '"
    ++ prettyShow (packageName pkg)
    ++ "' to 'library'."
ppExplanation (NoModulesExposed lib) =
  showLibraryName (libName lib) ++ " does not expose any modules"
ppExplanation SignaturesCabal2 =
  "To use the 'signatures' field the package needs to specify "
    ++ "at least 'cabal-version: 2.0'."
ppExplanation AutogenNotExposed =
  "An 'autogen-module' is neither on 'exposed-modules' or 'other-modules'."
ppExplanation AutogenIncludesNotIncluded =
  "An include in 'autogen-includes' is neither in 'includes' or "
    ++ "'install-includes'."
ppExplanation (NoMainIs exe) =
  "No 'main-is' field found for executable " ++ prettyShow (exeName exe)
ppExplanation NoHsLhsMain =
  "The 'main-is' field must specify a '.hs' or '.lhs' file "
    ++ "(even if it is generated by a preprocessor), "
    ++ "or it may specify a C/C++/obj-C source file."
ppExplanation MainCCabal1_18 =
  "The package uses a C/C++/obj-C source file for the 'main-is' field. "
    ++ "To use this feature you need to specify 'cabal-version: 1.18' or"
    ++ " higher."
ppExplanation (AutogenNoOther ct ucn) =
  "On "
    ++ ppCE ct
    ++ " '"
    ++ prettyShow ucn
    ++ "' an 'autogen-module'"
    ++ " is not on 'other-modules'"
ppExplanation AutogenIncludesNotIncludedExe =
  "An include in 'autogen-includes' is not in 'includes'."
ppExplanation (TestsuiteTypeNotKnown tt) =
  quote (prettyShow tt)
    ++ " is not a known type of test suite. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known test suite types are: "
    ++ commaSep (map prettyShow knownTestTypes)
ppExplanation (TestsuiteNotSupported tt) =
  quote (prettyShow tt)
    ++ " is not a supported test suite version. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known test suite types are: "
    ++ commaSep (map prettyShow knownTestTypes)
ppExplanation (BenchmarkTypeNotKnown tt) =
  quote (prettyShow tt)
    ++ " is not a known type of benchmark. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known benchmark types are: "
    ++ commaSep (map prettyShow knownBenchmarkTypes)
ppExplanation (BenchmarkNotSupported tt) =
  quote (prettyShow tt)
    ++ " is not a supported benchmark version. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known benchmark types are: "
    ++ commaSep (map prettyShow knownBenchmarkTypes)
ppExplanation NoHsLhsMainBench =
  "The 'main-is' field must specify a '.hs' or '.lhs' file "
    ++ "(even if it is generated by a preprocessor)."
ppExplanation (InvalidNameWin pkg) =
  "The package name '"
    ++ prettyShow (packageName pkg)
    ++ "' is "
    ++ "invalid on Windows. Many tools need to convert package names to "
    ++ "file names so using this name would cause problems."
ppExplanation ZPrefix =
  "Package names with the prefix 'z-' are reserved by Cabal and "
    ++ "cannot be used."
ppExplanation NoBuildType =
  "No 'build-type' specified. If you do not need a custom Setup.hs or "
    ++ "./configure script then use 'build-type: Simple'."
ppExplanation NoCustomSetup =
  "Ignoring the 'custom-setup' section because the 'build-type' is "
    ++ "not 'Custom'. Use 'build-type: Custom' if you need to use a "
    ++ "custom Setup.hs script."
ppExplanation (UnknownCompilers unknownCompilers) =
  "Unknown compiler "
    ++ commaSep (map quote unknownCompilers)
    ++ " in 'tested-with' field."
ppExplanation (UnknownLanguages unknownLanguages) =
  "Unknown languages: " ++ commaSep unknownLanguages
ppExplanation (UnknownExtensions unknownExtensions) =
  "Unknown extensions: " ++ commaSep unknownExtensions
ppExplanation (LanguagesAsExtension languagesUsedAsExtensions) =
  "Languages listed as extensions: "
    ++ commaSep languagesUsedAsExtensions
    ++ ". Languages must be specified in either the 'default-language' "
    ++ " or the 'other-languages' field."
ppExplanation (DeprecatedExtensions ourDeprecatedExtensions) =
  "Deprecated extensions: "
    ++ commaSep (map (quote . prettyShow . fst) ourDeprecatedExtensions)
    ++ ". "
    ++ unwords
      [ "Instead of '"
        ++ prettyShow ext
        ++ "' use '"
        ++ prettyShow replacement
        ++ "'."
      | (ext, Just replacement) <- ourDeprecatedExtensions
      ]
ppExplanation (MissingField cef) =
  "No '" ++ ppCEField cef ++ "' field."
ppExplanation SynopsisTooLong =
  "The 'synopsis' field is rather long (max 80 chars is recommended)."
ppExplanation ShortDesc =
  "The 'description' field should be longer than the 'synopsis' field. "
    ++ "It's useful to provide an informative 'description' to allow "
    ++ "Haskell programmers who have never heard about your package to "
    ++ "understand the purpose of your package. "
    ++ "The 'description' field content is typically shown by tooling "
    ++ "(e.g. 'cabal info', Haddock, Hackage) below the 'synopsis' which "
    ++ "serves as a headline. "
    ++ "Please refer to <https://cabal.readthedocs.io/en/stable/"
    ++ "cabal-package.html#package-properties> for more details."
ppExplanation (InvalidTestWith testedWithImpossibleRanges) =
  "Invalid 'tested-with' version range: "
    ++ commaSep (map prettyShow testedWithImpossibleRanges)
    ++ ". To indicate that you have tested a package with multiple "
    ++ "different versions of the same compiler use multiple entries, "
    ++ "for example 'tested-with: GHC==6.10.4, GHC==6.12.3' and not "
    ++ "'tested-with: GHC==6.10.4 && ==6.12.3'."
ppExplanation (ImpossibleInternalDep depInternalLibWithImpossibleVersion) =
  "The package has an impossible version range for a dependency on an "
    ++ "internal library: "
    ++ commaSep (map prettyShow depInternalLibWithImpossibleVersion)
    ++ ". This version range does not include the current package, and must "
    ++ "be removed as the current package's library will always be used."
ppExplanation (ImpossibleInternalExe depInternalExecWithImpossibleVersion) =
  "The package has an impossible version range for a dependency on an "
    ++ "internal executable: "
    ++ commaSep (map prettyShow depInternalExecWithImpossibleVersion)
    ++ ". This version range does not include the current package, and must "
    ++ "be removed as the current package's executable will always be used."
ppExplanation (MissingInternalExe depInternalExeWithImpossibleVersion) =
  "The package depends on a missing internal executable: "
    ++ commaSep (map prettyShow depInternalExeWithImpossibleVersion)
ppExplanation NONELicense = "The 'license' field is missing or is NONE."
ppExplanation NoLicense = "The 'license' field is missing."
ppExplanation AllRightsReservedLicense =
  "The 'license' is AllRightsReserved. Is that really what you want?"
ppExplanation (LicenseMessParse pkg) =
  "Unfortunately the license "
    ++ quote (prettyShow (license pkg))
    ++ " messes up the parser in earlier Cabal versions so you need to "
    ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
    ++ "compatibility with earlier Cabal versions then use 'OtherLicense'."
ppExplanation (UnrecognisedLicense l) =
  quote ("license: " ++ l)
    ++ " is not a recognised license. The "
    ++ "known licenses are: "
    ++ commaSep (map prettyShow knownLicenses)
ppExplanation UncommonBSD4 =
  "Using 'license: BSD4' is almost always a misunderstanding. 'BSD4' "
    ++ "refers to the old 4-clause BSD license with the advertising "
    ++ "clause. 'BSD3' refers the new 3-clause BSD license."
ppExplanation (UnknownLicenseVersion lic known) =
  "'license: "
    ++ prettyShow lic
    ++ "' is not a known "
    ++ "version of that license. The known versions are "
    ++ commaSep (map prettyShow known)
    ++ ". If this is not a mistake and you think it should be a known "
    ++ "version then please file a ticket."
ppExplanation NoLicenseFile = "A 'license-file' is not specified."
ppExplanation (UnrecognisedSourceRepo kind) =
  quote kind
    ++ " is not a recognised kind of source-repository. "
    ++ "The repo kind is usually 'head' or 'this'"
ppExplanation MissingType =
  "The source-repository 'type' is a required field."
ppExplanation MissingLocation =
  "The source-repository 'location' is a required field."
ppExplanation MissingModule =
  "For a CVS source-repository, the 'module' is a required field."
ppExplanation MissingTag =
  "For the 'this' kind of source-repository, the 'tag' is a required "
    ++ "field. It should specify the tag corresponding to this version "
    ++ "or release of the package."
ppExplanation SubdirRelPath =
  "The 'subdir' field of a source-repository must be a relative path."
ppExplanation (SubdirGoodRelPath err) =
  "The 'subdir' field of a source-repository is not a good relative path: "
    ++ show err
ppExplanation (OptFasm fieldName) =
  "'"
    ++ fieldName
    ++ ": -fasm' is unnecessary and will not work on CPU "
    ++ "architectures other than x86, x86-64, ppc or sparc."
ppExplanation (OptViaC fieldName) =
  "'"
    ++ fieldName
    ++ ": -fvia-C' is usually unnecessary. If your package "
    ++ "needs -via-C for correctness rather than performance then it "
    ++ "is using the FFI incorrectly and will probably not work with GHC "
    ++ "6.10 or later."
ppExplanation (OptHpc fieldName) =
  "'"
    ++ fieldName
    ++ ": -fhpc' is not necessary. Use the configure flag "
    ++ " --enable-coverage instead."
ppExplanation (OptProf fieldName) =
  "'"
    ++ fieldName
    ++ ": -prof' is not necessary and will lead to problems "
    ++ "when used on a library. Use the configure flag "
    ++ "--enable-library-profiling and/or --enable-profiling."
ppExplanation (OptO fieldName) =
  "'"
    ++ fieldName
    ++ ": -o' is not needed. "
    ++ "The output files are named automatically."
ppExplanation (OptHide fieldName) =
  "'"
    ++ fieldName
    ++ ": -hide-package' is never needed. "
    ++ "Cabal hides all packages."
ppExplanation (OptMake fieldName) =
  "'"
    ++ fieldName
    ++ ": --make' is never needed. Cabal uses this automatically."
ppExplanation (OptONot fieldName) =
  "'"
    ++ fieldName
    ++ ": -O0' is not needed. "
    ++ "Use the --disable-optimization configure flag."
ppExplanation (OptOOne fieldName) =
  "'"
    ++ fieldName
    ++ ": -O' is not needed. "
    ++ "Cabal automatically adds the '-O' flag. "
    ++ "Setting it yourself interferes with the --disable-optimization flag."
ppExplanation (OptOTwo fieldName) =
  "'"
    ++ fieldName
    ++ ": -O2' is rarely needed. "
    ++ "Check that it is giving a real benefit "
    ++ "and not just imposing longer compile times on your users."
ppExplanation (OptSplitSections fieldName) =
  "'"
    ++ fieldName
    ++ ": -split-sections' is not needed. "
    ++ "Use the --enable-split-sections configure flag."
ppExplanation (OptSplitObjs fieldName) =
  "'"
    ++ fieldName
    ++ ": -split-objs' is not needed. "
    ++ "Use the --enable-split-objs configure flag."
ppExplanation (OptWls fieldName) =
  "'"
    ++ fieldName
    ++ ": -optl-Wl,-s' is not needed and is not portable to"
    ++ " all operating systems. Cabal 1.4 and later automatically strip"
    ++ " executables. Cabal also has a flag --disable-executable-stripping"
    ++ " which is necessary when building packages for some Linux"
    ++ " distributions and using '-optl-Wl,-s' prevents that from working."
ppExplanation (OptExts fieldName) =
  "Instead of '"
    ++ fieldName
    ++ ": -fglasgow-exts' it is preferable to use "
    ++ "the 'extensions' field."
ppExplanation (OptRts fieldName) =
  "'"
    ++ fieldName
    ++ ": -rtsopts' has no effect for libraries. It should "
    ++ "only be used for executables."
ppExplanation (OptWithRts fieldName) =
  "'"
    ++ fieldName
    ++ ": -with-rtsopts' has no effect for libraries. It "
    ++ "should only be used for executables."
ppExplanation (COptONumber prefix label) =
  "'"
    ++ prefix
    ++ ": -O[n]' is generally not needed. When building with "
    ++ " optimisations Cabal automatically adds '-O2' for "
    ++ label
    ++ " code. Setting it yourself interferes with the"
    ++ " --disable-optimization flag."
ppExplanation (COptCPP opt) =
  "'cpp-options: " ++ opt ++ "' is not a portable C-preprocessor flag."
ppExplanation (OptAlternatives badField goodField flags) =
  "Instead of "
    ++ quote (badField ++ ": " ++ unwords badFlags)
    ++ " use "
    ++ quote (goodField ++ ": " ++ unwords goodFlags)
  where
    (badFlags, goodFlags) = unzip flags
ppExplanation (RelativeOutside field path) =
  quote (field ++ ": " ++ path)
    ++ " is a relative path outside of the source tree. "
    ++ "This will not work when generating a tarball with 'sdist'."
ppExplanation (AbsolutePath field path) =
  quote (field ++ ": " ++ path)
    ++ " specifies an absolute path, but the "
    ++ quote field
    ++ " field must use relative paths."
ppExplanation (BadRelativePAth field path err) =
  quote (field ++ ": " ++ path)
    ++ " is not a good relative path: "
    ++ show err
ppExplanation (DistPoint mfield path) =
  incipit
    ++ " points inside the 'dist' "
    ++ "directory. This is not reliable because the location of this "
    ++ "directory is configurable by the user (or package manager). In "
    ++ "addition the layout of the 'dist' directory is subject to change "
    ++ "in future versions of Cabal."
  where
    -- mfiled Nothing -> the path is inside `ghc-options`
    incipit =
      maybe
        ("'ghc-options' path " ++ quote path)
        (\field -> quote (field ++ ": " ++ path))
        mfield
ppExplanation (GlobSyntaxError field expl) =
  "In the '" ++ field ++ "' field: " ++ expl
ppExplanation (RecursiveGlobInRoot field glob) =
  "In the '"
    ++ field
    ++ "': glob '"
    ++ glob
    ++ "' starts at project root directory, this might "
    ++ "include `.git/`, ``dist-newstyle/``, or other large directories!"
ppExplanation (InvalidOnWin paths) =
  "The "
    ++ quotes paths
    ++ " invalid on Windows, which "
    ++ "would cause portability problems for this package. Windows file "
    ++ "names cannot contain any of the characters \":*?<>|\" and there "
    ++ "a few reserved names including \"aux\", \"nul\", \"con\", "
    ++ "\"prn\", \"com1-9\", \"lpt1-9\" and \"clock$\"."
  where
    quotes [failed] = "path " ++ quote failed ++ " is"
    quotes failed =
      "paths "
        ++ intercalate ", " (map quote failed)
        ++ " are"
ppExplanation (FilePathTooLong path) =
  "The following file name is too long to store in a portable POSIX "
    ++ "format tar archive. The maximum length is 255 ASCII characters.\n"
    ++ "The file in question is:\n  "
    ++ path
ppExplanation (FilePathNameTooLong path) =
  "The following file name is too long to store in a portable POSIX "
    ++ "format tar archive. The maximum length for the name part (including "
    ++ "extension) is 100 ASCII characters. The maximum length for any "
    ++ "individual directory component is 155.\n"
    ++ "The file in question is:\n  "
    ++ path
ppExplanation (FilePathSplitTooLong path) =
  "The following file name is too long to store in a portable POSIX "
    ++ "format tar archive. While the total length is less than 255 ASCII "
    ++ "characters, there are unfortunately further restrictions. It has to "
    ++ "be possible to split the file path on a directory separator into "
    ++ "two parts such that the first part fits in 155 characters or less "
    ++ "and the second part fits in 100 characters or less. Basically you "
    ++ "have to make the file name or directory names shorter, or you could "
    ++ "split a long directory name into nested subdirectories with shorter "
    ++ "names.\nThe file in question is:\n  "
    ++ path
ppExplanation FilePathEmpty =
  "Encountered a file with an empty name, something is very wrong! "
    ++ "Files with an empty name cannot be stored in a tar archive or in "
    ++ "standard file systems."
ppExplanation CVTestSuite =
  "The 'test-suite' section is new in Cabal 1.10. "
    ++ "Unfortunately it messes up the parser in older Cabal versions "
    ++ "so you must specify at least 'cabal-version: >= 1.8', but note "
    ++ "that only Cabal 1.10 and later can actually run such test suites."
ppExplanation CVDefaultLanguage =
  "To use the 'default-language' field the package needs to specify "
    ++ "at least 'cabal-version: >= 1.10'."
ppExplanation CVDefaultLanguageComponent =
  "Packages using 'cabal-version: >= 1.10' and before 'cabal-version: 3.4' "
    ++ "must specify the 'default-language' field for each component (e.g. "
    ++ "Haskell98 or Haskell2010). If a component uses different languages "
    ++ "in different modules then list the other ones in the "
    ++ "'other-languages' field."
ppExplanation CVExtraDocFiles =
  "To use the 'extra-doc-files' field the package needs to specify "
    ++ "'cabal-version: 1.18' or higher."
ppExplanation CVMultiLib =
  "To use multiple 'library' sections or a named library section "
    ++ "the package needs to specify at least 'cabal-version: 2.0'."
ppExplanation CVReexported =
  "To use the 'reexported-module' field the package needs to specify "
    ++ "'cabal-version: 1.22' or higher."
ppExplanation CVMixins =
  "To use the 'mixins' field the package needs to specify "
    ++ "at least 'cabal-version: 2.0'."
ppExplanation CVExtraFrameworkDirs =
  "To use the 'extra-framework-dirs' field the package needs to specify"
    ++ " 'cabal-version: 1.24' or higher."
ppExplanation CVDefaultExtensions =
  "To use the 'default-extensions' field the package needs to specify "
    ++ "at least 'cabal-version: >= 1.10'."
ppExplanation CVExtensionsDeprecated =
  "For packages using 'cabal-version: >= 1.10' the 'extensions' "
    ++ "field is deprecated. The new 'default-extensions' field lists "
    ++ "extensions that are used in all modules in the component, while "
    ++ "the 'other-extensions' field lists extensions that are used in "
    ++ "some modules, e.g. via the {-# LANGUAGE #-} pragma."
ppExplanation CVSources =
  "The use of 'asm-sources', 'cmm-sources', 'extra-bundled-libraries' "
    ++ " and 'extra-library-flavours' requires the package "
    ++ " to specify at least 'cabal-version: 3.0'."
ppExplanation (CVExtraDynamic flavs) =
  "The use of 'extra-dynamic-library-flavours' requires the package "
    ++ " to specify at least 'cabal-version: 3.0'. The flavours are: "
    ++ commaSep (concat flavs)
ppExplanation CVVirtualModules =
  "The use of 'virtual-modules' requires the package "
    ++ " to specify at least 'cabal-version: 2.2'."
ppExplanation CVSourceRepository =
  "The 'source-repository' section is new in Cabal 1.6. "
    ++ "Unfortunately it messes up the parser in earlier Cabal versions "
    ++ "so you need to specify 'cabal-version: >= 1.6'."
ppExplanation (CVExtensions version extCab12) =
  "Unfortunately the language extensions "
    ++ commaSep (map (quote . prettyShow) extCab12)
    ++ " break the parser in earlier Cabal versions so you need to "
    ++ "specify 'cabal-version: >= "
    ++ showCabalSpecVersion version
    ++ "'. Alternatively if you require compatibility with earlier "
    ++ "Cabal versions then you may be able to use an equivalent "
    ++ "compiler-specific flag."
ppExplanation CVCustomSetup =
  "Packages using 'cabal-version: 1.24' or higher with 'build-type: Custom' "
    ++ "must use a 'custom-setup' section with a 'setup-depends' field "
    ++ "that specifies the dependencies of the Setup.hs script itself. "
    ++ "The 'setup-depends' field uses the same syntax as 'build-depends', "
    ++ "so a simple example would be 'setup-depends: base, Cabal'."
ppExplanation CVExpliticDepsCustomSetup =
  "From version 1.24 cabal supports specifying explicit dependencies "
    ++ "for Custom setup scripts. Consider using 'cabal-version: 1.24' or "
    ++ "higher and adding a 'custom-setup' section with a 'setup-depends' "
    ++ "field that specifies the dependencies of the Setup.hs script "
    ++ "itself. The 'setup-depends' field uses the same syntax as "
    ++ "'build-depends', so a simple example would be 'setup-depends: base, "
    ++ "Cabal'."
ppExplanation CVAutogenPaths =
  "Packages using 'cabal-version: 2.0' and the autogenerated "
    ++ "module Paths_* must include it also on the 'autogen-modules' field "
    ++ "besides 'exposed-modules' and 'other-modules'. This specifies that "
    ++ "the module does not come with the package and is generated on "
    ++ "setup. Modules built with a custom Setup.hs script also go here "
    ++ "to ensure that commands like sdist don't fail."
ppExplanation CVAutogenPackageInfo =
  "Packages using 'cabal-version: 2.0' and the autogenerated "
    ++ "module PackageInfo_* must include it in 'autogen-modules' as well as"
    ++ " 'exposed-modules' and 'other-modules'. This specifies that "
    ++ "the module does not come with the package and is generated on "
    ++ "setup. Modules built with a custom Setup.hs script also go here "
    ++ "to ensure that commands like sdist don't fail."
ppExplanation (GlobNoMatch field glob) =
  "In '"
    ++ field
    ++ "': the pattern '"
    ++ glob
    ++ "' does not"
    ++ " match any files."
ppExplanation (GlobExactMatch field glob file) =
  "In '"
    ++ field
    ++ "': the pattern '"
    ++ glob
    ++ "' does not"
    ++ " match the file '"
    ++ file
    ++ "' because the extensions do not"
    ++ " exactly match (e.g., foo.en.html does not exactly match *.html)."
    ++ " To enable looser suffix-only matching, set 'cabal-version: 2.4' or"
    ++ " higher."
ppExplanation (GlobNoDir field glob dir) =
  "In '"
    ++ field
    ++ "': the pattern '"
    ++ glob
    ++ "' attempts to"
    ++ " match files in the directory '"
    ++ dir
    ++ "', but there is no"
    ++ " directory by that name."
ppExplanation (UnknownOS unknownOSs) =
  "Unknown operating system name " ++ commaSep (map quote unknownOSs)
ppExplanation (UnknownArch unknownArches) =
  "Unknown architecture name " ++ commaSep (map quote unknownArches)
ppExplanation (UnknownCompiler unknownImpls) =
  "Unknown compiler name " ++ commaSep (map quote unknownImpls)
ppExplanation (MissingUpperBounds names) =
  let separator = "\n  - "
   in "These packages miss upper bounds:"
        ++ separator
        ++ (intercalate separator (unPackageName <$> names))
        ++ "\n"
        ++ "Please add them, using `cabal gen-bounds` for suggestions."
        ++ " For more information see: "
        ++ " https://pvp.haskell.org/"
ppExplanation BaseNoUpperBounds =
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
ppExplanation (SuspiciousFlagName invalidFlagNames) =
  "Suspicious flag names: "
    ++ unwords invalidFlagNames
    ++ ". "
    ++ "To avoid ambiguity in command line interfaces, flag shouldn't "
    ++ "start with a dash. Also for better compatibility, flag names "
    ++ "shouldn't contain non-ascii characters."
ppExplanation (DeclaredUsedFlags declared used) =
  "Declared and used flag sets differ: "
    ++ s declared
    ++ " /= "
    ++ s used
    ++ ". "
  where
    s :: Set.Set FlagName -> String
    s = commaSep . map unFlagName . Set.toList
ppExplanation (NonASCIICustomField nonAsciiXFields) =
  "Non ascii custom fields: "
    ++ unwords nonAsciiXFields
    ++ ". "
    ++ "For better compatibility, custom field names "
    ++ "shouldn't contain non-ascii characters."
ppExplanation RebindableClashPaths =
  "Packages using RebindableSyntax with OverloadedStrings or"
    ++ " OverloadedLists in default-extensions, in conjunction with the"
    ++ " autogenerated module Paths_*, are known to cause compile failures"
    ++ " with Cabal < 2.2. To use these default-extensions with a Paths_*"
    ++ " autogen module, specify at least 'cabal-version: 2.2'."
ppExplanation RebindableClashPackageInfo =
  "Packages using RebindableSyntax with OverloadedStrings or"
    ++ " OverloadedLists in default-extensions, in conjunction with the"
    ++ " autogenerated module PackageInfo_*, are known to cause compile failures"
    ++ " with Cabal < 2.2. To use these default-extensions with a PackageInfo_*"
    ++ " autogen module, specify at least 'cabal-version: 2.2'."
ppExplanation (WErrorUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -Werror' makes the package easy to "
      ++ "break with future GHC versions because new GHC versions often "
      ++ "add new warnings."
ppExplanation (JUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -j[N]' can make sense for specific user's setup,"
      ++ " but it is not appropriate for a distributed package."
ppExplanation (FDeferTypeErrorsUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -fdefer-type-errors' is fine during development "
      ++ "but is not appropriate for a distributed package."
ppExplanation (DynamicUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -d*' debug flags are not appropriate "
      ++ "for a distributed package."
ppExplanation (ProfilingUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -fprof*' profiling flags are typically not "
      ++ "appropriate for a distributed library package. These flags are "
      ++ "useful to profile this package, but when profiling other packages "
      ++ "that use this one these flags clutter the profile output with "
      ++ "excessive detail. If you think other packages really want to see "
      ++ "cost centres from this package then use '-fprof-auto-exported' "
      ++ "which puts cost centres only on exported functions."
ppExplanation (UpperBoundSetup nm) =
  "The dependency 'setup-depends: '"
    ++ nm
    ++ "' does not specify an "
    ++ "upper bound on the version number. Each major release of the "
    ++ "'"
    ++ nm
    ++ "' package changes the API in various ways and most "
    ++ "packages will need some changes to compile with it. If you are "
    ++ "not sure what upper bound to use then use the next major "
    ++ "version."
ppExplanation (DuplicateModule s dupLibsLax) =
  "Duplicate modules in "
    ++ s
    ++ ": "
    ++ commaSep (map prettyShow dupLibsLax)
ppExplanation (PotentialDupModule s dupLibsStrict) =
  "Potential duplicate modules (subject to conditionals) in "
    ++ s
    ++ ": "
    ++ commaSep (map prettyShow dupLibsStrict)
ppExplanation (BOMStart pdfile) =
  pdfile
    ++ " starts with an Unicode byte order mark (BOM)."
    ++ " This may cause problems with older cabal versions."
ppExplanation (NotPackageName pdfile expectedCabalname) =
  "The filename "
    ++ quote pdfile
    ++ " does not match package name "
    ++ "(expected: "
    ++ quote expectedCabalname
    ++ ")"
ppExplanation NoDesc =
  "No cabal file found.\n"
    ++ "Please create a package description file <pkgname>.cabal"
ppExplanation (MultiDesc multiple) =
  "Multiple cabal files found while checking.\n"
    ++ "Please use only one of: "
    ++ intercalate ", " multiple
ppExplanation (UnknownFile fieldname file) =
  "The '"
    ++ fieldname
    ++ "' field refers to the file "
    ++ quote (getSymbolicPath file)
    ++ " which does not exist."
ppExplanation MissingSetupFile =
  "The package is missing a Setup.hs or Setup.lhs script."
ppExplanation MissingConfigureScript =
  "The 'build-type' is 'Configure' but there is no 'configure' script. "
    ++ "You probably need to run 'autoreconf -i' to generate it."
ppExplanation (UnknownDirectory kind dir) =
  quote (kind ++ ": " ++ dir)
    ++ " specifies a directory which does not exist."
ppExplanation MissingSourceControl =
  "When distributing packages it is encouraged to specify source "
    ++ "control information in the .cabal file using one or more "
    ++ "'source-repository' sections. See the Cabal user guide for "
    ++ "details."
ppExplanation (MissingExpectedDocFiles extraDocFileSupport paths) =
  "Please consider including the "
    ++ quotes paths
    ++ " in the '"
    ++ targetField
    ++ "' section of the .cabal file "
    ++ "if it contains useful information for users of the package."
  where
    quotes [p] = "file " ++ quote p
    quotes ps = "files " ++ intercalate ", " (map quote ps)
    targetField =
      if extraDocFileSupport
        then "extra-doc-files"
        else "extra-source-files"
ppExplanation (WrongFieldForExpectedDocFiles extraDocFileSupport field paths) =
  "Please consider moving the "
    ++ quotes paths
    ++ " from the '"
    ++ field
    ++ "' section of the .cabal file "
    ++ "to the section '"
    ++ targetField
    ++ "'."
  where
    quotes [p] = "file " ++ quote p
    quotes ps = "files " ++ intercalate ", " (map quote ps)
    targetField =
      if extraDocFileSupport
        then "extra-doc-files"
        else "extra-source-files"

-- | Results of some kind of failed package check.
--
-- There are a range of severities, from merely dubious to totally insane.
-- All of them come with a human readable explanation. In future we may augment
-- them with more machine readable explanations, for example to help an IDE
-- suggest automatic corrections.
data PackageCheck
  = -- | This package description is no good. There's no way it's going to
    -- build sensibly. This should give an error at configure time.
    PackageBuildImpossible {explanation :: CheckExplanation}
  | -- | A problem that is likely to affect building the package, or an
    -- issue that we'd like every package author to be aware of, even if
    -- the package is never distributed.
    PackageBuildWarning {explanation :: CheckExplanation}
  | -- | An issue that might not be a problem for the package author but
    -- might be annoying or detrimental when the package is distributed to
    -- users. We should encourage distributed packages to be free from these
    -- issues, but occasionally there are justifiable reasons so we cannot
    -- ban them entirely.
    PackageDistSuspicious {explanation :: CheckExplanation}
  | -- | Like PackageDistSuspicious but will only display warnings
    -- rather than causing abnormal exit when you run 'cabal check'.
    PackageDistSuspiciousWarn {explanation :: CheckExplanation}
  | -- | An issue that is OK in the author's environment but is almost
    -- certain to be a portability problem for other environments. We can
    -- quite legitimately refuse to publicly distribute packages with these
    -- problems.
    PackageDistInexcusable {explanation :: CheckExplanation}
  deriving (Eq, Ord)

-- | Would Hackage refuse a package because of this error?
isHackageDistError :: PackageCheck -> Bool
isHackageDistError = \case
  (PackageBuildImpossible{}) -> True
  (PackageBuildWarning{}) -> True
  (PackageDistInexcusable{}) -> True
  (PackageDistSuspicious{}) -> False
  (PackageDistSuspiciousWarn{}) -> False

-- | Pretty printing 'PackageCheck'.
ppPackageCheck :: PackageCheck -> String
ppPackageCheck e = ppExplanation (explanation e)

instance Show PackageCheck where
  show notice = ppPackageCheck notice

check :: Bool -> PackageCheck -> Maybe PackageCheck
check False _ = Nothing
check True pc = Just pc

checkSpecVersion
  :: PackageDescription
  -> CabalSpecVersion
  -> Bool
  -> PackageCheck
  -> Maybe PackageCheck
checkSpecVersion pkg specver cond pc
  | specVersion pkg >= specver = Nothing
  | otherwise = check cond pc

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
checkPackage
  :: GenericPackageDescription
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
    ++ checkPackageInfoModuleExtensions pkg
    ++ checkSetupVersions gpkg
    ++ checkDuplicateModules gpkg
  where
    pkg = fromMaybe (flattenPackageDescription gpkg) mpkg

-- TODO: make this variant go away
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
checkSanity :: PackageDescription -> [PackageCheck]
checkSanity pkg =
  catMaybes
    [ check (null . unPackageName . packageName $ pkg) $
        PackageBuildImpossible NoNameField
    , check (nullVersion == packageVersion pkg) $
        PackageBuildImpossible NoVersionField
    , check
        ( all
            ($ pkg)
            [ null . executables
            , null . testSuites
            , null . benchmarks
            , null . allLibraries
            , null . foreignLibs
            ]
        )
        $ PackageBuildImpossible NoTarget
    , check (any (== LMainLibName) (map libName $ subLibraries pkg)) $
        PackageBuildImpossible UnnamedInternal
    , check (not (null duplicateNames)) $
        PackageBuildImpossible (DuplicateSections duplicateNames)
    , -- NB: but it's OK for executables to have the same name!
      -- TODO shouldn't need to compare on the string level
      check
        ( any
            (== prettyShow (packageName pkg))
            (prettyShow <$> subLibNames)
        )
        $ PackageBuildImpossible (IllegalLibraryName pkg)
    ]
    -- TODO: check for name clashes case insensitively: windows file systems cannot
    -- cope.

    ++ concatMap (checkLibrary pkg) (allLibraries pkg)
    ++ concatMap (checkExecutable pkg) (executables pkg)
    ++ concatMap (checkTestSuite pkg) (testSuites pkg)
    ++ concatMap (checkBenchmark pkg) (benchmarks pkg)
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
  catMaybes
    [ -- TODO: This check is bogus if a required-signature was passed through
      check (null (explicitLibModules lib) && null (reexportedModules lib)) $
        PackageDistSuspiciousWarn (NoModulesExposed lib)
    , -- check use of signatures sections
      checkVersion CabalSpecV2_0 (not (null (signatures lib))) $
        PackageDistInexcusable SignaturesCabal2
    , -- check that all autogen-modules appear on other-modules or exposed-modules
      check
        (not $ and $ map (flip elem (explicitLibModules lib)) (libModulesAutogen lib))
        $ PackageBuildImpossible AutogenNotExposed
    , -- check that all autogen-includes appear on includes or install-includes
      check
        (not $ and $ map (flip elem (allExplicitIncludes lib)) (view L.autogenIncludes lib))
        $ PackageBuildImpossible AutogenIncludesNotIncluded
    ]
  where
    checkVersion :: CabalSpecVersion -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= ver = Nothing
      | otherwise = check cond pc

allExplicitIncludes :: L.HasBuildInfo a => a -> [FilePath]
allExplicitIncludes x = view L.includes x ++ view L.installIncludes x

checkExecutable :: PackageDescription -> Executable -> [PackageCheck]
checkExecutable pkg exe =
  catMaybes
    [ check (null (modulePath exe)) $
        PackageBuildImpossible (NoMainIs exe)
    , -- This check does not apply to scripts.
      check
        ( package pkg /= fakePackageId
            && not (null (modulePath exe))
            && not (fileExtensionSupportedLanguage $ modulePath exe)
        )
        $ PackageBuildImpossible NoHsLhsMain
    , checkSpecVersion
        pkg
        CabalSpecV1_18
        ( fileExtensionSupportedLanguage (modulePath exe)
            && takeExtension (modulePath exe) `notElem` [".hs", ".lhs"]
        )
        $ PackageDistInexcusable MainCCabal1_18
    , -- check that all autogen-modules appear on other-modules
      check
        (not $ and $ map (flip elem (exeModules exe)) (exeModulesAutogen exe))
        $ PackageBuildImpossible (AutogenNoOther CETExecutable (exeName exe))
    , -- check that all autogen-includes appear on includes
      check
        (not $ and $ map (flip elem (view L.includes exe)) (view L.autogenIncludes exe))
        $ PackageBuildImpossible AutogenIncludesNotIncludedExe
    ]

checkTestSuite :: PackageDescription -> TestSuite -> [PackageCheck]
checkTestSuite pkg test =
  catMaybes
    [ case testInterface test of
        TestSuiteUnsupported tt@(TestTypeUnknown _ _) ->
          Just $
            PackageBuildWarning (TestsuiteTypeNotKnown tt)
        TestSuiteUnsupported tt ->
          Just $
            PackageBuildWarning (TestsuiteNotSupported tt)
        _ -> Nothing
    , check mainIsWrongExt $
        PackageBuildImpossible NoHsLhsMain
    , checkSpecVersion pkg CabalSpecV1_18 (mainIsNotHsExt && not mainIsWrongExt) $
        PackageDistInexcusable MainCCabal1_18
    , -- check that all autogen-modules appear on other-modules
      check
        (not $ and $ map (flip elem (testModules test)) (testModulesAutogen test))
        $ PackageBuildImpossible (AutogenNoOther CETTest (testName test))
    , -- check that all autogen-includes appear on includes
      check
        (not $ and $ map (flip elem (view L.includes test)) (view L.autogenIncludes test))
        $ PackageBuildImpossible AutogenIncludesNotIncludedExe
    ]
  where
    mainIsWrongExt = case testInterface test of
      TestSuiteExeV10 _ f -> not $ fileExtensionSupportedLanguage f
      _ -> False

    mainIsNotHsExt = case testInterface test of
      TestSuiteExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
      _ -> False

checkBenchmark :: PackageDescription -> Benchmark -> [PackageCheck]
checkBenchmark _pkg bm =
  catMaybes
    [ case benchmarkInterface bm of
        BenchmarkUnsupported tt@(BenchmarkTypeUnknown _ _) ->
          Just $
            PackageBuildWarning (BenchmarkTypeNotKnown tt)
        BenchmarkUnsupported tt ->
          Just $
            PackageBuildWarning (BenchmarkNotSupported tt)
        _ -> Nothing
    , check mainIsWrongExt $
        PackageBuildImpossible NoHsLhsMainBench
    , -- check that all autogen-modules appear on other-modules
      check
        (not $ and $ map (flip elem (benchmarkModules bm)) (benchmarkModulesAutogen bm))
        $ PackageBuildImpossible (AutogenNoOther CETBenchmark (benchmarkName bm))
    , -- check that all autogen-includes appear on includes
      check
        (not $ and $ map (flip elem (view L.includes bm)) (view L.autogenIncludes bm))
        $ PackageBuildImpossible AutogenIncludesNotIncludedExe
    ]
  where
    mainIsWrongExt = case benchmarkInterface bm of
      BenchmarkExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
      _ -> False

-- ------------------------------------------------------------

-- * Additional pure checks

-- ------------------------------------------------------------

checkFields :: PackageDescription -> [PackageCheck]
checkFields pkg =
  catMaybes
    [ check (not . FilePath.Windows.isValid . prettyShow . packageName $ pkg) $
        PackageDistInexcusable (InvalidNameWin pkg)
    , check (isPrefixOf "z-" . prettyShow . packageName $ pkg) $
        PackageDistInexcusable ZPrefix
    , check (isNothing (buildTypeRaw pkg) && specVersion pkg < CabalSpecV2_2) $
        PackageBuildWarning NoBuildType
    , check (isJust (setupBuildInfo pkg) && buildType pkg /= Custom) $
        PackageBuildWarning NoCustomSetup
    , check (not (null unknownCompilers)) $
        PackageBuildWarning (UnknownCompilers unknownCompilers)
    , check (not (null unknownLanguages)) $
        PackageBuildWarning (UnknownLanguages unknownLanguages)
    , check (not (null unknownExtensions)) $
        PackageBuildWarning (UnknownExtensions unknownExtensions)
    , check (not (null languagesUsedAsExtensions)) $
        PackageBuildWarning (LanguagesAsExtension languagesUsedAsExtensions)
    , check (not (null ourDeprecatedExtensions)) $
        PackageDistSuspicious (DeprecatedExtensions ourDeprecatedExtensions)
    , check (ShortText.null (category pkg)) $
        PackageDistSuspicious (MissingField CEFCategory)
    , check (ShortText.null (maintainer pkg)) $
        PackageDistSuspicious (MissingField CEFMaintainer)
    , check (ShortText.null (synopsis pkg) && ShortText.null (description pkg)) $
        PackageDistInexcusable (MissingField CEFSynOrDesc)
    , check (ShortText.null (description pkg) && not (ShortText.null (synopsis pkg))) $
        PackageDistSuspicious (MissingField CEFDescription)
    , check (ShortText.null (synopsis pkg) && not (ShortText.null (description pkg))) $
        PackageDistSuspicious (MissingField CEFSynopsis)
    , -- TODO: recommend the bug reports URL, author and homepage fields
      -- TODO: recommend not using the stability field
      -- TODO: recommend specifying a source repo

      check (ShortText.length (synopsis pkg) > 80) $
        PackageDistSuspicious SynopsisTooLong
    , -- See also https://github.com/haskell/cabal/pull/3479
      check
        ( not (ShortText.null (description pkg))
            && ShortText.length (description pkg) <= ShortText.length (synopsis pkg)
        )
        $ PackageDistSuspicious ShortDesc
    , -- check use of impossible constraints "tested-with: GHC== 6.10 && ==6.12"
      check (not (null testedWithImpossibleRanges)) $
        PackageDistInexcusable (InvalidTestWith testedWithImpossibleRanges)
    , -- for more details on why the following was commented out,
      -- check https://github.com/haskell/cabal/pull/7470#issuecomment-875878507
      -- , check (not (null depInternalLibraryWithExtraVersion)) $
      --     PackageBuildWarning $
      --          "The package has an extraneous version range for a dependency on an "
      --       ++ "internal library: "
      --       ++ commaSep (map prettyShow depInternalLibraryWithExtraVersion)
      --       ++ ". This version range includes the current package but isn't needed "
      --       ++ "as the current package's library will always be used."

      check (not (null depInternalLibraryWithImpossibleVersion)) $
        PackageBuildImpossible
          (ImpossibleInternalDep depInternalLibraryWithImpossibleVersion)
    , -- , check (not (null depInternalExecutableWithExtraVersion)) $
      --     PackageBuildWarning $
      --          "The package has an extraneous version range for a dependency on an "
      --       ++ "internal executable: "
      --       ++ commaSep (map prettyShow depInternalExecutableWithExtraVersion)
      --       ++ ". This version range includes the current package but isn't needed "
      --       ++ "as the current package's executable will always be used."

      check (not (null depInternalExecutableWithImpossibleVersion)) $
        PackageBuildImpossible
          (ImpossibleInternalExe depInternalExecutableWithImpossibleVersion)
    , check (not (null depMissingInternalExecutable)) $
        PackageBuildImpossible (MissingInternalExe depMissingInternalExecutable)
    ]
  where
    unknownCompilers = [name | (OtherCompiler name, _) <- testedWith pkg]
    unknownLanguages =
      [ name | bi <- allBuildInfo pkg, UnknownLanguage name <- allLanguages bi
      ]
    unknownExtensions =
      [ name | bi <- allBuildInfo pkg, UnknownExtension name <- allExtensions bi, name `notElem` map prettyShow knownLanguages
      ]
    ourDeprecatedExtensions =
      nub $
        catMaybes
          [ find ((== ext) . fst) deprecatedExtensions
          | bi <- allBuildInfo pkg
          , ext <- allExtensions bi
          ]
    languagesUsedAsExtensions =
      [ name | bi <- allBuildInfo pkg, UnknownExtension name <- allExtensions bi, name `elem` map prettyShow knownLanguages
      ]

    testedWithImpossibleRanges =
      [ Dependency (mkPackageName (prettyShow compiler)) vr mainLibSet
      | (compiler, vr) <- testedWith pkg
      , isNoVersion vr
      ]

    internalLibraries =
      map
        (maybe (packageName pkg) unqualComponentNameToPackageName . libraryNameString . libName)
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

    -- depInternalLibraryWithExtraVersion =
    --   [ dep
    --   | dep@(Dependency _ versionRange _) <- internalLibDeps
    --   , not $ isAnyVersion versionRange
    --   , packageVersion pkg `withinRange` versionRange
    --   ]

    depInternalLibraryWithImpossibleVersion =
      [ dep
      | dep@(Dependency _ versionRange _) <- internalLibDeps
      , not $ packageVersion pkg `withinRange` versionRange
      ]

    -- depInternalExecutableWithExtraVersion =
    --   [ dep
    --   | dep@(ExeDependency _ _ versionRange) <- internalExeDeps
    --   , not $ isAnyVersion versionRange
    --   , packageVersion pkg `withinRange` versionRange
    --   ]

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
  Left l -> checkNewLicense pkg l

checkNewLicense :: PackageDescription -> SPDX.License -> [PackageCheck]
checkNewLicense _pkg lic =
  catMaybes
    [ check (lic == SPDX.NONE) $
        PackageDistInexcusable NONELicense
    ]

checkOldLicense :: PackageDescription -> License -> [PackageCheck]
checkOldLicense pkg lic =
  catMaybes
    [ check (lic == UnspecifiedLicense) $
        PackageDistInexcusable NoLicense
    , check (lic == AllRightsReserved) $
        PackageDistSuspicious AllRightsReservedLicense
    , checkVersion CabalSpecV1_4 (lic `notElem` compatLicenses) $
        PackageDistInexcusable (LicenseMessParse pkg)
    , case lic of
        UnknownLicense l -> Just $ PackageBuildWarning (UnrecognisedLicense l)
        _ -> Nothing
    , check (lic == BSD4) $
        PackageDistSuspicious UncommonBSD4
    , case unknownLicenseVersion lic of
        Just knownVersions ->
          Just $
            PackageDistSuspicious (UnknownLicenseVersion lic knownVersions)
        _ -> Nothing
    , check
        ( lic
            `notElem` [ AllRightsReserved
                      , UnspecifiedLicense
                      , PublicDomain
                      ]
            -- AllRightsReserved and PublicDomain are not strictly
            -- licenses so don't need license files.
            && null (licenseFiles pkg)
        )
        $ PackageDistSuspicious NoLicenseFile
    ]
  where
    unknownLicenseVersion (GPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | GPL (Just v') <- knownLicenses]
    unknownLicenseVersion (LGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | LGPL (Just v') <- knownLicenses]
    unknownLicenseVersion (AGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | AGPL (Just v') <- knownLicenses]
    unknownLicenseVersion (Apache (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | Apache (Just v') <- knownLicenses]
    unknownLicenseVersion _ = Nothing

    checkVersion :: CabalSpecVersion -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= ver = Nothing
      | otherwise = check cond pc

    compatLicenses =
      [ GPL Nothing
      , LGPL Nothing
      , AGPL Nothing
      , BSD3
      , BSD4
      , PublicDomain
      , AllRightsReserved
      , UnspecifiedLicense
      , OtherLicense
      ]

checkSourceRepos :: PackageDescription -> [PackageCheck]
checkSourceRepos pkg =
  catMaybes $
    concat
      [ [ case repoKind repo of
            RepoKindUnknown kind ->
              Just $
                PackageDistInexcusable $
                  UnrecognisedSourceRepo kind
            _ -> Nothing
        , check (isNothing (repoType repo)) $
            PackageDistInexcusable MissingType
        , check (isNothing (repoLocation repo)) $
            PackageDistInexcusable MissingLocation
        , check (repoType repo == Just (KnownRepoType CVS) && isNothing (repoModule repo)) $
            PackageDistInexcusable MissingModule
        , check (repoKind repo == RepoThis && isNothing (repoTag repo)) $
            PackageDistInexcusable MissingTag
        , check (maybe False isAbsoluteOnAnyPlatform (repoSubdir repo)) $
            PackageDistInexcusable SubdirRelPath
        , do
            subdir <- repoSubdir repo
            err <- isGoodRelativeDirectoryPath subdir
            return $ PackageDistInexcusable (SubdirGoodRelPath err)
        ]
      | repo <- sourceRepos pkg
      ]

-- TODO: check location looks like a URL for some repo types.

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
  catMaybes
    [ checkFlags ["-fasm"] $
        PackageDistInexcusable (OptFasm fieldName)
    , checkFlags ["-fvia-C"] $
        PackageDistSuspicious (OptViaC fieldName)
    , checkFlags ["-fhpc"] $
        PackageDistInexcusable (OptHpc fieldName)
    , checkFlags ["-prof"] $
        PackageBuildWarning (OptProf fieldName)
    , unlessScript . checkFlags ["-o"] $
        PackageBuildWarning (OptO fieldName)
    , checkFlags ["-hide-package"] $
        PackageBuildWarning (OptHide fieldName)
    , checkFlags ["--make"] $
        PackageBuildWarning (OptMake fieldName)
    , checkNonTestAndBenchmarkFlags ["-O0", "-Onot"] $
        PackageDistSuspicious (OptONot fieldName)
    , checkTestAndBenchmarkFlags ["-O0", "-Onot"] $
        PackageDistSuspiciousWarn (OptONot fieldName)
    , checkFlags ["-O", "-O1"] $
        PackageDistInexcusable (OptOOne fieldName)
    , checkFlags ["-O2"] $
        PackageDistSuspiciousWarn (OptOTwo fieldName)
    , checkFlags ["-split-sections"] $
        PackageBuildWarning (OptSplitSections fieldName)
    , checkFlags ["-split-objs"] $
        PackageBuildWarning (OptSplitObjs fieldName)
    , checkFlags ["-optl-Wl,-s", "-optl-s"] $
        PackageDistInexcusable (OptWls fieldName)
    , checkFlags ["-fglasgow-exts"] $
        PackageDistSuspicious (OptExts fieldName)
    , check ("-rtsopts" `elem` lib_ghc_options) $
        PackageBuildWarning (OptRts fieldName)
    , check (any (\opt -> "-with-rtsopts" `isPrefixOf` opt) lib_ghc_options) $
        PackageBuildWarning (OptWithRts fieldName)
    , checkAlternatives
        fieldName
        "extensions"
        [ (flag, prettyShow extension) | flag <- ghc_options_no_rtsopts, Just extension <- [ghcExtension flag]
        ]
    , checkAlternatives
        fieldName
        "extensions"
        [(flag, extension) | flag@('-' : 'X' : extension) <- ghc_options_no_rtsopts]
    , checkAlternatives fieldName "cpp-options" $
        [(flag, flag) | flag@('-' : 'D' : _) <- ghc_options_no_rtsopts]
          ++ [(flag, flag) | flag@('-' : 'U' : _) <- ghc_options_no_rtsopts]
    , checkAlternatives
        fieldName
        "include-dirs"
        [(flag, dir) | flag@('-' : 'I' : dir) <- ghc_options_no_rtsopts]
    , checkAlternatives
        fieldName
        "extra-libraries"
        [(flag, lib) | flag@('-' : 'l' : lib) <- ghc_options_no_rtsopts]
    , checkAlternatives
        fieldName
        "extra-libraries-static"
        [(flag, lib) | flag@('-' : 'l' : lib) <- ghc_options_no_rtsopts]
    , checkAlternatives
        fieldName
        "extra-lib-dirs"
        [(flag, dir) | flag@('-' : 'L' : dir) <- ghc_options_no_rtsopts]
    , checkAlternatives
        fieldName
        "extra-lib-dirs-static"
        [(flag, dir) | flag@('-' : 'L' : dir) <- ghc_options_no_rtsopts]
    , checkAlternatives
        fieldName
        "frameworks"
        [ (flag, fmwk)
        | (flag@"-framework", fmwk) <-
            zip ghc_options_no_rtsopts (safeTail ghc_options_no_rtsopts)
        ]
    , checkAlternatives
        fieldName
        "extra-framework-dirs"
        [ (flag, dir)
        | (flag@"-framework-path", dir) <-
            zip ghc_options_no_rtsopts (safeTail ghc_options_no_rtsopts)
        ]
    ]
  where
    all_ghc_options = concatMap getOptions (allBuildInfo pkg)
    ghc_options_no_rtsopts = rmRtsOpts all_ghc_options
    lib_ghc_options =
      concatMap
        (getOptions . libBuildInfo)
        (allLibraries pkg)
    test_ghc_options =
      concatMap
        (getOptions . testBuildInfo)
        (testSuites pkg)
    benchmark_ghc_options =
      concatMap
        (getOptions . benchmarkBuildInfo)
        (benchmarks pkg)
    test_and_benchmark_ghc_options =
      test_ghc_options
        ++ benchmark_ghc_options
    non_test_and_benchmark_ghc_options =
      concatMap
        getOptions
        ( allBuildInfo
            ( pkg
                { testSuites = []
                , benchmarks = []
                }
            )
        )

    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) all_ghc_options)

    unlessScript :: Maybe PackageCheck -> Maybe PackageCheck
    unlessScript pc
      | packageId pkg == fakePackageId = Nothing
      | otherwise = pc

    checkTestAndBenchmarkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkTestAndBenchmarkFlags flags = check (any (`elem` flags) test_and_benchmark_ghc_options)

    checkNonTestAndBenchmarkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkNonTestAndBenchmarkFlags flags = check (any (`elem` flags) non_test_and_benchmark_ghc_options)

    ghcExtension ('-' : 'f' : name) = case name of
      "allow-overlapping-instances" -> enable OverlappingInstances
      "no-allow-overlapping-instances" -> disable OverlappingInstances
      "th" -> enable TemplateHaskell
      "no-th" -> disable TemplateHaskell
      "ffi" -> enable ForeignFunctionInterface
      "no-ffi" -> disable ForeignFunctionInterface
      "fi" -> enable ForeignFunctionInterface
      "no-fi" -> disable ForeignFunctionInterface
      "monomorphism-restriction" -> enable MonomorphismRestriction
      "no-monomorphism-restriction" -> disable MonomorphismRestriction
      "mono-pat-binds" -> enable MonoPatBinds
      "no-mono-pat-binds" -> disable MonoPatBinds
      "allow-undecidable-instances" -> enable UndecidableInstances
      "no-allow-undecidable-instances" -> disable UndecidableInstances
      "allow-incoherent-instances" -> enable IncoherentInstances
      "no-allow-incoherent-instances" -> disable IncoherentInstances
      "arrows" -> enable Arrows
      "no-arrows" -> disable Arrows
      "generics" -> enable Generics
      "no-generics" -> disable Generics
      "implicit-prelude" -> enable ImplicitPrelude
      "no-implicit-prelude" -> disable ImplicitPrelude
      "implicit-params" -> enable ImplicitParams
      "no-implicit-params" -> disable ImplicitParams
      "bang-patterns" -> enable BangPatterns
      "no-bang-patterns" -> disable BangPatterns
      "scoped-type-variables" -> enable ScopedTypeVariables
      "no-scoped-type-variables" -> disable ScopedTypeVariables
      "extended-default-rules" -> enable ExtendedDefaultRules
      "no-extended-default-rules" -> disable ExtendedDefaultRules
      _ -> Nothing
    ghcExtension "-cpp" = enable CPP
    ghcExtension _ = Nothing

    enable e = Just (EnableExtension e)
    disable e = Just (DisableExtension e)

    rmRtsOpts :: [String] -> [String]
    rmRtsOpts ("-with-rtsopts" : _ : xs) = rmRtsOpts xs
    rmRtsOpts (x : xs) = x : rmRtsOpts xs
    rmRtsOpts [] = []

checkCCOptions :: PackageDescription -> [PackageCheck]
checkCCOptions = checkCLikeOptions "C" "cc-options" ccOptions

checkCxxOptions :: PackageDescription -> [PackageCheck]
checkCxxOptions = checkCLikeOptions "C++" "cxx-options" cxxOptions

checkCLikeOptions :: String -> String -> (BuildInfo -> [String]) -> PackageDescription -> [PackageCheck]
checkCLikeOptions label prefix accessor pkg =
  catMaybes
    [ checkAlternatives
        prefix
        "include-dirs"
        [(flag, dir) | flag@('-' : 'I' : dir) <- all_cLikeOptions]
    , checkAlternatives
        prefix
        "extra-libraries"
        [(flag, lib) | flag@('-' : 'l' : lib) <- all_cLikeOptions]
    , checkAlternatives
        prefix
        "extra-lib-dirs"
        [(flag, dir) | flag@('-' : 'L' : dir) <- all_cLikeOptions]
    , checkAlternatives
        "ld-options"
        "extra-libraries"
        [(flag, lib) | flag@('-' : 'l' : lib) <- all_ldOptions]
    , checkAlternatives
        "ld-options"
        "extra-lib-dirs"
        [(flag, dir) | flag@('-' : 'L' : dir) <- all_ldOptions]
    , checkCCFlags ["-O", "-Os", "-O0", "-O1", "-O2", "-O3"] $
        PackageDistSuspicious (COptONumber prefix label)
    ]
  where
    all_cLikeOptions =
      [ opts | bi <- allBuildInfo pkg, opts <- accessor bi
      ]
    all_ldOptions =
      [ opts | bi <- allBuildInfo pkg, opts <- ldOptions bi
      ]

    checkCCFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkCCFlags flags = check (any (`elem` flags) all_cLikeOptions)

checkCPPOptions :: PackageDescription -> [PackageCheck]
checkCPPOptions pkg =
  catMaybes
    [ checkAlternatives
        "cpp-options"
        "include-dirs"
        [(flag, dir) | flag@('-' : 'I' : dir) <- all_cppOptions]
    ]
    ++ [ PackageBuildWarning (COptCPP opt)
       | opt <- all_cppOptions
       , -- "-I" is handled above, we allow only -DNEWSTUFF and -UOLDSTUFF
       not $ any (`isPrefixOf` opt) ["-D", "-U", "-I"]
       ]
  where
    all_cppOptions = [opts | bi <- allBuildInfo pkg, opts <- cppOptions bi]

checkAlternatives
  :: String
  -> String
  -> [(String, String)]
  -> Maybe PackageCheck
checkAlternatives badField goodField flags =
  check (not (null badFlags)) $
    PackageBuildWarning (OptAlternatives badField goodField flags)
  where
    (badFlags, _) = unzip flags

data PathKind
  = PathKindFile
  | PathKindDirectory
  | PathKindGlob
  deriving (Eq)

checkPaths :: PackageDescription -> [PackageCheck]
checkPaths pkg =
  checkPackageFileNamesWithGlob
    [ (kind == PathKindGlob, path)
    | (path, _, kind) <- relPaths ++ absPaths
    ]
    ++ [ PackageBuildWarning (RelativeOutside field path)
       | (path, field, _) <- relPaths ++ absPaths
       , isOutsideTree path
       ]
    ++ [ PackageDistInexcusable (AbsolutePath field path)
       | (path, field, _) <- relPaths
       , isAbsoluteOnAnyPlatform path
       ]
    ++ [ PackageDistInexcusable (BadRelativePAth field path err)
       | (path, field, kind) <- relPaths
       , -- these are not paths, but globs...
       err <- maybeToList $ case kind of
        PathKindFile -> isGoodRelativeFilePath path
        PathKindGlob -> isGoodRelativeGlob path
        PathKindDirectory -> isGoodRelativeDirectoryPath path
       ]
    ++ [ PackageDistInexcusable $ DistPoint (Just field) path
       | (path, field, _) <- relPaths ++ absPaths
       , isInsideDist path
       ]
    ++ [ PackageDistInexcusable (DistPoint Nothing path)
       | bi <- allBuildInfo pkg
       , (GHC, flags) <- perCompilerFlavorToList $ options bi
       , path <- flags
       , isInsideDist path
       ]
    ++ [ PackageDistInexcusable $
        GlobSyntaxError "data-files" (explainGlobSyntaxError pat err)
       | (Left err, pat) <- zip globsDataFiles $ dataFiles pkg
       ]
    ++ [ PackageDistInexcusable
        (GlobSyntaxError "extra-source-files" (explainGlobSyntaxError pat err))
       | (Left err, pat) <- zip globsExtraSrcFiles $ extraSrcFiles pkg
       ]
    ++ [ PackageDistInexcusable $
        GlobSyntaxError "extra-doc-files" (explainGlobSyntaxError pat err)
       | (Left err, pat) <- zip globsExtraDocFiles $ extraDocFiles pkg
       ]
    ++ [ PackageDistSuspiciousWarn $
        RecursiveGlobInRoot "data-files" pat
       | (Right glob, pat) <- zip globsDataFiles $ dataFiles pkg
       , isRecursiveInRoot glob
       ]
    ++ [ PackageDistSuspiciousWarn $
        RecursiveGlobInRoot "extra-source-files" pat
       | (Right glob, pat) <- zip globsExtraSrcFiles $ extraSrcFiles pkg
       , isRecursiveInRoot glob
       ]
    ++ [ PackageDistSuspiciousWarn $
        RecursiveGlobInRoot "extra-doc-files" pat
       | (Right glob, pat) <- zip globsExtraDocFiles $ extraDocFiles pkg
       , isRecursiveInRoot glob
       ]
  where
    isOutsideTree path = case splitDirectories path of
      ".." : _ -> True
      "." : ".." : _ -> True
      _ -> False
    isInsideDist path = case map lowercase (splitDirectories path) of
      "dist" : _ -> True
      "." : "dist" : _ -> True
      _ -> False

    -- paths that must be relative
    relPaths :: [(FilePath, String, PathKind)]
    relPaths =
      [(path, "extra-source-files", PathKindGlob) | path <- extraSrcFiles pkg]
        ++ [(path, "extra-tmp-files", PathKindFile) | path <- extraTmpFiles pkg]
        ++ [(path, "extra-doc-files", PathKindGlob) | path <- extraDocFiles pkg]
        ++ [(path, "data-files", PathKindGlob) | path <- dataFiles pkg]
        ++ [(path, "data-dir", PathKindDirectory) | path <- [dataDir pkg]]
        ++ [(path, "license-file", PathKindFile) | path <- map getSymbolicPath $ licenseFiles pkg]
        ++ concat
          [ [(path, "asm-sources", PathKindFile) | path <- asmSources bi]
            ++ [(path, "cmm-sources", PathKindFile) | path <- cmmSources bi]
            ++ [(path, "c-sources", PathKindFile) | path <- cSources bi]
            ++ [(path, "cxx-sources", PathKindFile) | path <- cxxSources bi]
            ++ [(path, "js-sources", PathKindFile) | path <- jsSources bi]
            ++ [(path, "install-includes", PathKindFile) | path <- installIncludes bi]
            ++ [(path, "hs-source-dirs", PathKindDirectory) | path <- map getSymbolicPath $ hsSourceDirs bi]
          | bi <- allBuildInfo pkg
          ]

    -- paths that are allowed to be absolute
    absPaths :: [(FilePath, String, PathKind)]
    absPaths =
      concat
        [ [(path, "includes", PathKindFile) | path <- includes bi]
          ++ [(path, "include-dirs", PathKindDirectory) | path <- includeDirs bi]
          ++ [(path, "extra-lib-dirs", PathKindDirectory) | path <- extraLibDirs bi]
          ++ [(path, "extra-lib-dirs-static", PathKindDirectory) | path <- extraLibDirsStatic bi]
        | bi <- allBuildInfo pkg
        ]
    globsDataFiles :: [Either GlobSyntaxError Glob]
    globsDataFiles = parseFileGlob (specVersion pkg) <$> dataFiles pkg
    globsExtraSrcFiles :: [Either GlobSyntaxError Glob]
    globsExtraSrcFiles = parseFileGlob (specVersion pkg) <$> extraSrcFiles pkg
    globsExtraDocFiles :: [Either GlobSyntaxError Glob]
    globsExtraDocFiles = parseFileGlob (specVersion pkg) <$> extraDocFiles pkg

-- TODO: check sets of paths that would be interpreted differently between Unix
-- and windows, ie case-sensitive or insensitive. Things that might clash, or
-- conversely be distinguished.

-- TODO: use the tar path checks on all the above paths

-- | Check that the package declares the version in the @\"cabal-version\"@
-- field correctly.
checkCabalVersion :: PackageDescription -> [PackageCheck]
checkCabalVersion pkg =
  catMaybes
    [ -- check use of test suite sections
      checkVersion CabalSpecV1_8 (not (null $ testSuites pkg)) $
        PackageDistInexcusable CVTestSuite
    , -- check use of default-language field
      -- note that we do not need to do an equivalent check for the
      -- other-language field since that one does not change behaviour
      checkVersion CabalSpecV1_10 (any isJust (buildInfoField defaultLanguage)) $
        PackageBuildWarning CVDefaultLanguage
    , check
        ( specVersion pkg >= CabalSpecV1_10
            && specVersion pkg < CabalSpecV3_4
            && any isNothing (buildInfoField defaultLanguage)
        )
        $ PackageBuildWarning CVDefaultLanguageComponent
    , checkVersion
        CabalSpecV1_18
        (not . null $ extraDocFiles pkg)
        $ PackageDistInexcusable CVExtraDocFiles
    , checkVersion
        CabalSpecV2_0
        (not (null (subLibraries pkg)))
        $ PackageDistInexcusable CVMultiLib
    , -- check use of reexported-modules sections
      checkVersion
        CabalSpecV1_22
        (any (not . null . reexportedModules) (allLibraries pkg))
        $ PackageDistInexcusable CVReexported
    , -- check use of thinning and renaming
      checkVersion CabalSpecV2_0 usesBackpackIncludes $
        PackageDistInexcusable CVMixins
    , -- check use of 'extra-framework-dirs' field
      checkVersion CabalSpecV1_24 (any (not . null) (buildInfoField extraFrameworkDirs)) $
        -- Just a warning, because this won't break on old Cabal versions.
        PackageDistSuspiciousWarn CVExtraFrameworkDirs
    , -- check use of default-extensions field
      -- don't need to do the equivalent check for other-extensions
      checkVersion CabalSpecV1_10 (any (not . null) (buildInfoField defaultExtensions)) $
        PackageBuildWarning CVDefaultExtensions
    , -- check use of extensions field
      check
        ( specVersion pkg >= CabalSpecV1_10
            && any (not . null) (buildInfoField oldExtensions)
        )
        $ PackageBuildWarning CVExtensionsDeprecated
    , checkVersion
        CabalSpecV3_0
        ( any
            (not . null)
            ( concatMap
                buildInfoField
                [ asmSources
                , cmmSources
                , extraBundledLibs
                , extraLibFlavours
                ]
            )
        )
        $ PackageDistInexcusable CVSources
    , checkVersion CabalSpecV3_0 (any (not . null) $ buildInfoField extraDynLibFlavours) $
        PackageDistInexcusable
          (CVExtraDynamic $ buildInfoField extraDynLibFlavours)
    , checkVersion
        CabalSpecV2_2
        ( any
            (not . null)
            (buildInfoField virtualModules)
        )
        $ PackageDistInexcusable CVVirtualModules
    , -- check use of "source-repository" section
      checkVersion CabalSpecV1_6 (not (null (sourceRepos pkg))) $
        PackageDistInexcusable CVSourceRepository
    , -- check for new language extensions
      checkVersion CabalSpecV1_2 (not (null mentionedExtensionsThatNeedCabal12)) $
        PackageDistInexcusable
          (CVExtensions CabalSpecV1_2 mentionedExtensionsThatNeedCabal12)
    , checkVersion CabalSpecV1_4 (not (null mentionedExtensionsThatNeedCabal14)) $
        PackageDistInexcusable
          (CVExtensions CabalSpecV1_4 mentionedExtensionsThatNeedCabal14)
    , check
        ( specVersion pkg >= CabalSpecV1_24
            && isNothing (setupBuildInfo pkg)
            && buildType pkg == Custom
        )
        $ PackageBuildWarning CVCustomSetup
    , check
        ( specVersion pkg < CabalSpecV1_24
            && isNothing (setupBuildInfo pkg)
            && buildType pkg == Custom
        )
        $ PackageDistSuspiciousWarn CVExpliticDepsCustomSetup
    , check
        ( specVersion pkg >= CabalSpecV2_0
            && elem (autogenPathsModuleName pkg) allModuleNames
            && not (elem (autogenPathsModuleName pkg) allModuleNamesAutogen)
        )
        $ PackageDistInexcusable CVAutogenPaths
    , check
        ( specVersion pkg >= CabalSpecV2_0
            && elem (autogenPackageInfoModuleName pkg) allModuleNames
            && not (elem (autogenPackageInfoModuleName pkg) allModuleNamesAutogen)
        )
        $ PackageDistInexcusable CVAutogenPackageInfo
    ]
  where
    -- Perform a check on packages that use a version of the spec less than
    -- the version given. This is for cases where a new Cabal version adds
    -- a new feature and we want to check that it is not used prior to that
    -- version.
    checkVersion :: CabalSpecVersion -> Bool -> PackageCheck -> Maybe PackageCheck
    checkVersion ver cond pc
      | specVersion pkg >= ver = Nothing
      | otherwise = check cond pc

    buildInfoField field = map field (allBuildInfo pkg)

    usesBackpackIncludes = any (not . null . mixins) (allBuildInfo pkg)

    mentionedExtensions =
      [ ext | bi <- allBuildInfo pkg, ext <- allExtensions bi
      ]
    mentionedExtensionsThatNeedCabal12 =
      nub (filter (`elem` compatExtensionsExtra) mentionedExtensions)

    -- As of Cabal-1.4 we can add new extensions without worrying about
    -- breaking old versions of cabal.
    mentionedExtensionsThatNeedCabal14 =
      nub (filter (`notElem` compatExtensions) mentionedExtensions)

    -- The known extensions in Cabal-1.2.3
    compatExtensions =
      map
        EnableExtension
        [ OverlappingInstances
        , UndecidableInstances
        , IncoherentInstances
        , RecursiveDo
        , ParallelListComp
        , MultiParamTypeClasses
        , FunctionalDependencies
        , Rank2Types
        , RankNTypes
        , PolymorphicComponents
        , ExistentialQuantification
        , ScopedTypeVariables
        , ImplicitParams
        , FlexibleContexts
        , FlexibleInstances
        , EmptyDataDecls
        , CPP
        , BangPatterns
        , TypeSynonymInstances
        , TemplateHaskell
        , ForeignFunctionInterface
        , Arrows
        , Generics
        , NamedFieldPuns
        , PatternGuards
        , GeneralizedNewtypeDeriving
        , ExtensibleRecords
        , RestrictedTypeSynonyms
        , HereDocuments
        ]
        ++ map
          DisableExtension
          [MonomorphismRestriction, ImplicitPrelude]
        ++ compatExtensionsExtra

    -- The extra known extensions in Cabal-1.2.3 vs Cabal-1.1.6
    -- (Cabal-1.1.6 came with ghc-6.6. Cabal-1.2 came with ghc-6.8)
    compatExtensionsExtra =
      map
        EnableExtension
        [ KindSignatures
        , MagicHash
        , TypeFamilies
        , StandaloneDeriving
        , UnicodeSyntax
        , PatternSignatures
        , UnliftedFFITypes
        , LiberalTypeSynonyms
        , TypeOperators
        , RecordWildCards
        , RecordPuns
        , DisambiguateRecordFields
        , OverloadedStrings
        , GADTs
        , RelaxedPolyRec
        , ExtendedDefaultRules
        , UnboxedTuples
        , DeriveDataTypeable
        , ConstrainedClassMethods
        ]
        ++ map
          DisableExtension
          [MonoPatBinds]

    allModuleNames =
      ( case library pkg of
          Nothing -> []
          (Just lib) -> explicitLibModules lib
      )
        ++ concatMap otherModules (allBuildInfo pkg)

    allModuleNamesAutogen = concatMap autogenModules (allBuildInfo pkg)

-- ------------------------------------------------------------

-- * Checks on the GenericPackageDescription

-- ------------------------------------------------------------

-- | Check the build-depends fields for any weirdness or bad practice.
checkPackageVersions :: GenericPackageDescription -> [PackageCheck]
checkPackageVersions pkg =
  -- if others is empty,
  -- the error will still fire but listing no dependencies.
  -- so we have to check
  if length others > 0
    then PackageDistSuspiciousWarn (MissingUpperBounds others) : baseErrors
    else baseErrors
  where
    baseErrors = PackageDistInexcusable BaseNoUpperBounds <$ bases
    deps = toDependencyVersionsMap allBuildDepends pkg
    -- base gets special treatment (it's more critical)
    (bases, others) =
      partition (("base" ==) . unPackageName) $
        [ name
        | (name, vr) <- Map.toList deps
        , not (hasUpperBound vr)
        ]

checkConditionals :: GenericPackageDescription -> [PackageCheck]
checkConditionals pkg =
  catMaybes
    [ check (not $ null unknownOSs) $
        PackageDistInexcusable (UnknownOS unknownOSs)
    , check (not $ null unknownArches) $
        PackageDistInexcusable (UnknownArch unknownArches)
    , check (not $ null unknownImpls) $
        PackageDistInexcusable (UnknownCompiler unknownImpls)
    ]
  where
    unknownOSs = [os | OS (OtherOS os) <- conditions]
    unknownArches = [arch | Arch (OtherArch arch) <- conditions]
    unknownImpls = [impl | Impl (OtherCompiler impl) _ <- conditions]
    conditions =
      concatMap fvs (maybeToList (condLibrary pkg))
        ++ concatMap (fvs . snd) (condSubLibraries pkg)
        ++ concatMap (fvs . snd) (condForeignLibs pkg)
        ++ concatMap (fvs . snd) (condExecutables pkg)
        ++ concatMap (fvs . snd) (condTestSuites pkg)
        ++ concatMap (fvs . snd) (condBenchmarks pkg)
    fvs (CondNode _ _ ifs) = concatMap compfv ifs -- free variables
    compfv (CondBranch c ct mct) = condfv c ++ fvs ct ++ maybe [] fvs mct
    condfv c = case c of
      Var v -> [v]
      Lit _ -> []
      CNot c1 -> condfv c1
      COr c1 c2 -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2

checkFlagNames :: GenericPackageDescription -> [PackageCheck]
checkFlagNames gpd
  | null invalidFlagNames = []
  | otherwise =
      [PackageDistInexcusable (SuspiciousFlagName invalidFlagNames)]
  where
    invalidFlagNames =
      [ fn
      | flag <- genPackageFlags gpd
      , let fn = unFlagName (flagName flag)
      , invalidFlagName fn
      ]
    -- starts with dash
    invalidFlagName ('-' : _) = True
    -- mon ascii letter
    invalidFlagName cs = any (not . isAscii) cs

checkUnusedFlags :: GenericPackageDescription -> [PackageCheck]
checkUnusedFlags gpd
  | declared == used = []
  | otherwise =
      [PackageDistSuspicious (DeclaredUsedFlags declared used)]
  where
    declared :: Set.Set FlagName
    declared = toSetOf (L.genPackageFlags . traverse . L.flagName) gpd

    used :: Set.Set FlagName
    used =
      mconcat
        [ toSetOf (L.condLibrary . traverse . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condSubLibraries . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condForeignLibs . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condExecutables . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condTestSuites . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        , toSetOf (L.condBenchmarks . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
        ]

checkUnicodeXFields :: GenericPackageDescription -> [PackageCheck]
checkUnicodeXFields gpd
  | null nonAsciiXFields = []
  | otherwise =
      [PackageDistInexcusable (NonASCIICustomField nonAsciiXFields)]
  where
    nonAsciiXFields :: [String]
    nonAsciiXFields = [n | (n, _) <- xfields, any (not . isAscii) n]

    xfields :: [(String, String)]
    xfields =
      DList.runDList $
        mconcat
          [ toDListOf (L.packageDescription . L.customFieldsPD . traverse) gpd
          , toDListOf (L.traverseBuildInfos . L.customFieldsBI . traverse) gpd
          ]

-- | cabal-version <2.2 + Paths_module + default-extensions: doesn't build.
checkPathsModuleExtensions :: PackageDescription -> [PackageCheck]
checkPathsModuleExtensions = checkAutogenModuleExtensions autogenPathsModuleName RebindableClashPaths

-- | cabal-version <2.2 + PackageInfo_module + default-extensions: doesn't build.
checkPackageInfoModuleExtensions :: PackageDescription -> [PackageCheck]
checkPackageInfoModuleExtensions = checkAutogenModuleExtensions autogenPackageInfoModuleName RebindableClashPackageInfo

-- | cabal-version <2.2 + *_module + default-extensions: doesn't build.
checkAutogenModuleExtensions
  :: (PackageDescription -> ModuleName)
  -> CheckExplanation
  -> PackageDescription
  -> [PackageCheck]
checkAutogenModuleExtensions autogenModuleName rebindableClashExplanation pd
  | specVersion pd >= CabalSpecV2_2 = []
  | any checkBI (allBuildInfo pd) || any checkLib (allLibraries pd) =
      return (PackageBuildImpossible rebindableClashExplanation)
  | otherwise = []
  where
    mn = autogenModuleName pd

    checkLib :: Library -> Bool
    checkLib l = mn `elem` exposedModules l && checkExts (l ^. L.defaultExtensions)

    checkBI :: BuildInfo -> Bool
    checkBI bi =
      (mn `elem` otherModules bi || mn `elem` autogenModules bi)
        && checkExts (bi ^. L.defaultExtensions)

    checkExts exts = rebind `elem` exts && (strings `elem` exts || lists `elem` exts)
      where
        rebind = EnableExtension RebindableSyntax
        strings = EnableExtension OverloadedStrings
        lists = EnableExtension OverloadedLists

-- | Checks GHC options from all ghc-*-options fields from the given BuildInfo
-- and reports flags that are OK during development process, but are
-- unacceptable in a distributed package
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
  catMaybes
    [ check has_Werror $
        PackageDistInexcusable (WErrorUnneeded fieldName)
    , check has_J $
        PackageDistInexcusable (JUnneeded fieldName)
    , checkFlags ["-fdefer-type-errors"] $
        PackageDistInexcusable (FDeferTypeErrorsUnneeded fieldName)
    , -- -dynamic is not a debug flag
      check
        ( any
            (\opt -> "-d" `isPrefixOf` opt && opt /= "-dynamic")
            ghcOptions
        )
        $ PackageDistInexcusable (DynamicUnneeded fieldName)
    , checkFlags
        [ "-fprof-auto"
        , "-fprof-auto-top"
        , "-fprof-auto-calls"
        , "-fprof-cafs"
        , "-fno-prof-count-entries"
        , "-auto-all"
        , "-auto"
        , "-caf-all"
        ]
        $ PackageDistSuspicious (ProfilingUnneeded fieldName)
    ]
  where
    has_Werror = "-Werror" `elem` ghcOptions
    has_J =
      any
        ( \o -> case o of
            "-j" -> True
            ('-' : 'j' : d : _) -> isDigit d
            _ -> False
        )
        ghcOptions
    checkFlags :: [String] -> PackageCheck -> Maybe PackageCheck
    checkFlags flags = check (any (`elem` flags) ghcOptions)

checkDevelopmentOnlyFlags :: GenericPackageDescription -> [PackageCheck]
checkDevelopmentOnlyFlags pkg =
  concatMap
    checkDevelopmentOnlyFlagsBuildInfo
    [ bi
    | (conditions, bi) <- allConditionalBuildInfo
    , not (any guardedByManualFlag conditions)
    ]
  where
    guardedByManualFlag = definitelyFalse

    -- We've basically got three-values logic here: True, False or unknown
    -- hence this pattern to propagate the unknown cases properly.
    definitelyFalse (Var (PackageFlag n)) = maybe False not (Map.lookup n manualFlags)
    definitelyFalse (Var _) = False
    definitelyFalse (Lit b) = not b
    definitelyFalse (CNot c) = definitelyTrue c
    definitelyFalse (COr c1 c2) = definitelyFalse c1 && definitelyFalse c2
    definitelyFalse (CAnd c1 c2) = definitelyFalse c1 || definitelyFalse c2

    definitelyTrue (Var (PackageFlag n)) = fromMaybe False (Map.lookup n manualFlags)
    definitelyTrue (Var _) = False
    definitelyTrue (Lit b) = b
    definitelyTrue (CNot c) = definitelyFalse c
    definitelyTrue (COr c1 c2) = definitelyTrue c1 || definitelyTrue c2
    definitelyTrue (CAnd c1 c2) = definitelyTrue c1 && definitelyTrue c2

    manualFlags =
      Map.fromList
        [ (flagName flag, flagDefault flag)
        | flag <- genPackageFlags pkg
        , flagManual flag
        ]

    allConditionalBuildInfo :: [([Condition ConfVar], BuildInfo)]
    allConditionalBuildInfo =
      concatMap
        (collectCondTreePaths libBuildInfo)
        (maybeToList (condLibrary pkg))
        ++ concatMap
          (collectCondTreePaths libBuildInfo . snd)
          (condSubLibraries pkg)
        ++ concatMap
          (collectCondTreePaths buildInfo . snd)
          (condExecutables pkg)
        ++ concatMap
          (collectCondTreePaths testBuildInfo . snd)
          (condTestSuites pkg)
        ++ concatMap
          (collectCondTreePaths benchmarkBuildInfo . snd)
          (condBenchmarks pkg)

    -- get all the leaf BuildInfo, paired up with the path (in the tree sense)
    -- of if-conditions that guard it
    collectCondTreePaths
      :: (a -> b)
      -> CondTree v c a
      -> [([Condition v], b)]
    collectCondTreePaths mapData = go []
      where
        go conditions condNode =
          -- the data at this level in the tree:
          (reverse conditions, mapData (condTreeData condNode))
            : concat
              [ go (condition : conditions) ifThen
              | (CondBranch condition ifThen _) <- condTreeComponents condNode
              ]
            ++ concat
              [ go (condition : conditions) elseThen
              | (CondBranch condition _ (Just elseThen)) <- condTreeComponents condNode
              ]

-- ------------------------------------------------------------

-- * Checks involving files in the package

-- ------------------------------------------------------------

-- | Sanity check things that requires IO. It looks at the files in the
-- package and expects to find the package unpacked in at the given file path.
checkPackageFiles :: Verbosity -> PackageDescription -> FilePath -> IO [PackageCheck]
checkPackageFiles verbosity pkg root = do
  contentChecks <- checkPackageContent checkFilesIO pkg
  preDistributionChecks <- checkPackageFilesPreDistribution verbosity pkg root
  -- Sort because different platforms will provide files from
  -- `getDirectoryContents` in different orders, and we'd like to be
  -- stable for test output.
  return (sort contentChecks ++ sort preDistributionChecks)
  where
    checkFilesIO =
      CheckPackageContentOps
        { doesFileExist = System.doesFileExist . relative
        , doesDirectoryExist = System.doesDirectoryExist . relative
        , getDirectoryContents = System.Directory.getDirectoryContents . relative
        , getFileContents = BS.readFile . relative
        }
    relative path = root </> path

-- | A record of operations needed to check the contents of packages.
-- Used by 'checkPackageContent'.
data CheckPackageContentOps m = CheckPackageContentOps
  { doesFileExist :: FilePath -> m Bool
  , doesDirectoryExist :: FilePath -> m Bool
  , getDirectoryContents :: FilePath -> m [FilePath]
  , getFileContents :: FilePath -> m BS.ByteString
  }

-- | Sanity check things that requires looking at files in the package.
-- This is a generalised version of 'checkPackageFiles' that can work in any
-- monad for which you can provide 'CheckPackageContentOps' operations.
--
-- The point of this extra generality is to allow doing checks in some virtual
-- file system, for example a tarball in memory.
checkPackageContent
  :: (Monad m, Applicative m)
  => CheckPackageContentOps m
  -> PackageDescription
  -> m [PackageCheck]
checkPackageContent ops pkg = do
  cabalBomError <- checkCabalFileBOM ops
  cabalNameError <- checkCabalFileName ops pkg
  licenseErrors <- checkLicensesExist ops pkg
  setupError <- checkSetupExists ops pkg
  configureError <- checkConfigureExists ops pkg
  localPathErrors <- checkLocalPathsExist ops pkg
  vcsLocation <- checkMissingVcsInfo ops pkg

  return $
    licenseErrors
      ++ catMaybes [cabalBomError, cabalNameError, setupError, configureError]
      ++ localPathErrors
      ++ vcsLocation

checkCabalFileBOM
  :: Monad m
  => CheckPackageContentOps m
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
    Left _ -> return Nothing
    Right pdfile ->
      (flip check pc . BS.isPrefixOf bomUtf8)
        `liftM` getFileContents ops pdfile
      where
        pc = PackageDistInexcusable (BOMStart pdfile)
  where
    bomUtf8 :: BS.ByteString
    bomUtf8 = BS.pack [0xef, 0xbb, 0xbf] -- U+FEFF encoded as UTF8

checkCabalFileName
  :: Monad m
  => CheckPackageContentOps m
  -> PackageDescription
  -> m (Maybe PackageCheck)
checkCabalFileName ops pkg = do
  -- findPackageDesc already takes care to detect missing/multiple
  -- .cabal files; we don't include this check in 'findPackageDesc' in
  -- order not to short-cut other checks which call 'findPackageDesc'
  epdfile <- findPackageDesc ops
  case epdfile of
    -- see "MASSIVE HACK" note in 'checkCabalFileBOM'
    Left _ -> return Nothing
    Right pdfile
      | takeFileName pdfile == expectedCabalname -> return Nothing
      | otherwise ->
          return $
            Just $
              PackageDistInexcusable
                (NotPackageName pdfile expectedCabalname)
  where
    pkgname = unPackageName . packageName $ pkg
    expectedCabalname = pkgname <.> "cabal"

-- | Find a package description file in the given directory.  Looks for
--  @.cabal@ files.  Like 'Distribution.Simple.Utils.findPackageDesc',
--  but generalized over monads.
findPackageDesc
  :: Monad m
  => CheckPackageContentOps m
  -> m (Either PackageCheck FilePath)
  -- ^ <pkgname>.cabal
findPackageDesc ops =
  do
    let dir = "."
    files <- getDirectoryContents ops dir
    -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
    -- file we filter to exclude dirs and null base file names:
    cabalFiles <-
      filterM
        (doesFileExist ops)
        [ dir </> file
        | file <- files
        , let (name, ext) = splitExtension file
        , not (null name) && ext == ".cabal"
        ]
    case cabalFiles of
      [] -> return (Left $ PackageBuildImpossible NoDesc)
      [cabalFile] -> return (Right cabalFile)
      multiple ->
        return
          ( Left $
              PackageBuildImpossible
                (MultiDesc multiple)
          )

checkLicensesExist
  :: (Monad m, Applicative m)
  => CheckPackageContentOps m
  -> PackageDescription
  -> m [PackageCheck]
checkLicensesExist ops pkg = do
  exists <- traverse (doesFileExist ops . getSymbolicPath) (licenseFiles pkg)
  return
    [ PackageBuildWarning (UnknownFile fieldname file)
    | (file, False) <- zip (licenseFiles pkg) exists
    ]
  where
    fieldname
      | length (licenseFiles pkg) == 1 = "license-file"
      | otherwise = "license-files"

checkSetupExists
  :: Monad m
  => CheckPackageContentOps m
  -> PackageDescription
  -> m (Maybe PackageCheck)
checkSetupExists ops pkg = do
  let simpleBuild = buildType pkg == Simple
  hsexists <- doesFileExist ops "Setup.hs"
  lhsexists <- doesFileExist ops "Setup.lhs"
  return $
    check (not simpleBuild && not hsexists && not lhsexists) $
      PackageDistInexcusable MissingSetupFile

checkConfigureExists
  :: Monad m
  => CheckPackageContentOps m
  -> PackageDescription
  -> m (Maybe PackageCheck)
checkConfigureExists ops pd
  | buildType pd == Configure = do
      exists <- doesFileExist ops "configure"
      return $
        check (not exists) $
          PackageBuildWarning MissingConfigureScript
  | otherwise = return Nothing

checkLocalPathsExist
  :: Monad m
  => CheckPackageContentOps m
  -> PackageDescription
  -> m [PackageCheck]
checkLocalPathsExist ops pkg = do
  let dirs =
        [ (dir, kind)
        | bi <- allBuildInfo pkg
        , (dir, kind) <-
            [(dir, "extra-lib-dirs") | dir <- extraLibDirs bi]
              ++ [(dir, "extra-lib-dirs-static") | dir <- extraLibDirsStatic bi]
              ++ [ (dir, "extra-framework-dirs")
                 | dir <- extraFrameworkDirs bi
                 ]
              ++ [(dir, "include-dirs") | dir <- includeDirs bi]
              ++ [(getSymbolicPath dir, "hs-source-dirs") | dir <- hsSourceDirs bi]
        , isRelativeOnAnyPlatform dir
        ]
  missing <- filterM (liftM not . doesDirectoryExist ops . fst) dirs
  return
    [ PackageBuildWarning (UnknownDirectory kind dir)
    | (dir, kind) <- missing
    ]

checkMissingVcsInfo
  :: (Monad m, Applicative m)
  => CheckPackageContentOps m
  -> PackageDescription
  -> m [PackageCheck]
checkMissingVcsInfo ops pkg | null (sourceRepos pkg) = do
  vcsInUse <- liftM or $ traverse (doesDirectoryExist ops) repoDirnames
  if vcsInUse
    then return [PackageDistSuspicious MissingSourceControl]
    else return []
  where
    repoDirnames =
      [ dirname | repo <- knownRepoTypes, dirname <- repoTypeDirname repo
      ]
checkMissingVcsInfo _ _ = return []

repoTypeDirname :: KnownRepoType -> [FilePath]
repoTypeDirname Darcs = ["_darcs"]
repoTypeDirname Git = [".git"]
repoTypeDirname SVN = [".svn"]
repoTypeDirname CVS = ["CVS"]
repoTypeDirname Mercurial = [".hg"]
repoTypeDirname GnuArch = [".arch-params"]
repoTypeDirname Bazaar = [".bzr"]
repoTypeDirname Monotone = ["_MTN"]
repoTypeDirname Pijul = [".pijul"]

-- ------------------------------------------------------------

-- * Checks involving files in the package

-- ------------------------------------------------------------

-- | Check the names of all files in a package for portability problems. This
-- should be done for example when creating or validating a package tarball.
checkPackageFileNames :: [FilePath] -> [PackageCheck]
checkPackageFileNames = checkPackageFileNamesWithGlob . zip (repeat True)

checkPackageFileNamesWithGlob :: [(Bool, FilePath)] -> [PackageCheck]
checkPackageFileNamesWithGlob files =
  catMaybes $
    checkWindowsPaths files
      : [ checkTarPath file
        | (_, file) <- files
        ]

checkWindowsPaths :: [(Bool, FilePath)] -> Maybe PackageCheck
checkWindowsPaths paths =
  case filter (not . FilePath.Windows.isValid . escape) paths of
    [] -> Nothing
    ps ->
      Just $
        PackageDistInexcusable (InvalidOnWin $ map snd ps)
  where
    -- force a relative name to catch invalid file names like "f:oo" which
    -- otherwise parse as file "oo" in the current directory on the 'f' drive.
    escape (isGlob, path) =
      (".\\" ++)
      -- glob paths will be expanded before being dereferenced, so asterisks
      -- shouldn't count against them.
      $
        map (\c -> if c == '*' && isGlob then 'x' else c) path

-- | Check a file name is valid for the portable POSIX tar format.
--
-- The POSIX tar format has a restriction on the length of file names. It is
-- unfortunately not a simple restriction like a maximum length. The exact
-- restriction is that either the whole path be 100 characters or less, or it
-- be possible to split the path on a directory separator such that the first
-- part is 155 characters or less and the second part 100 characters or less.
checkTarPath :: FilePath -> Maybe PackageCheck
checkTarPath path
  | length path > 255 = Just longPath
  | otherwise = case pack nameMax (reverse (splitPath path)) of
      Left err -> Just err
      Right [] -> Nothing
      Right (h : rest) -> case pack prefixMax remainder of
        Left err -> Just err
        Right [] -> Nothing
        Right (_ : _) -> Just noSplit
        where
          -- drop the '/' between the name and prefix:
          remainder = safeInit h : rest
  where
    nameMax, prefixMax :: Int
    nameMax = 100
    prefixMax = 155

    pack _ [] = Left emptyName
    pack maxLen (c : cs)
      | n > maxLen = Left longName
      | otherwise = Right (pack' maxLen n cs)
      where
        n = length c

    pack' maxLen n (c : cs)
      | n' <= maxLen = pack' maxLen n' cs
      where
        n' = n + length c
    pack' _ _ cs = cs

    longPath = PackageDistInexcusable (FilePathTooLong path)
    longName = PackageDistInexcusable (FilePathNameTooLong path)
    noSplit = PackageDistInexcusable (FilePathSplitTooLong path)
    emptyName = PackageDistInexcusable FilePathEmpty

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
checkGlobFiles
  :: Verbosity
  -> PackageDescription
  -> FilePath
  -> IO [PackageCheck]
checkGlobFiles verbosity pkg root = do
  -- Get the desirable doc files from package’s directory
  rootContents <- System.Directory.getDirectoryContents root
  docFiles0 <-
    filterM
      System.doesFileExist
      [ file
      | file <- rootContents
      , isDesirableExtraDocFile desirableDocFiles file
      ]
  -- Check the globs
  (warnings, unlisted) <- foldrM checkGlob ([], docFiles0) allGlobs

  return $
    if null unlisted
      then -- No missing desirable file
        warnings
      else -- Some missing desirable files

        warnings
          ++ let unlisted' = (root </>) <$> unlisted
              in [ PackageDistSuspiciousWarn
                    (MissingExpectedDocFiles extraDocFilesSupport unlisted')
                 ]
  where
    -- `extra-doc-files` is supported only from version 1.18
    extraDocFilesSupport = specVersion pkg >= CabalSpecV1_18
    adjustedDataDir = if null (dataDir pkg) then root else root </> dataDir pkg
    -- Cabal fields with globs
    allGlobs :: [(String, Bool, FilePath, FilePath)]
    allGlobs =
      concat
        [ (,,,) "extra-source-files" (not extraDocFilesSupport) root
            <$> extraSrcFiles pkg
        , (,,,) "extra-doc-files" True root <$> extraDocFiles pkg
        , (,,,) "data-files" False adjustedDataDir <$> dataFiles pkg
        ]

    -- For each field with globs (see allGlobs), look for:
    -- • errors (missing directory, no match)
    -- • omitted documentation files (changelog)
    checkGlob
      :: (String, Bool, FilePath, FilePath)
      -> ([PackageCheck], [FilePath])
      -> IO ([PackageCheck], [FilePath])
    checkGlob (field, isDocField, dir, glob) acc@(warnings, docFiles1) =
      -- Note: we just skip over parse errors here; they're reported elsewhere.
      case parseFileGlob (specVersion pkg) glob of
        Left _ -> return acc
        Right parsedGlob -> do
          results <- runDirFileGlob verbosity (root </> dir) parsedGlob
          let acc0 = (warnings, True, docFiles1, [])
          return $ case foldr checkGlobResult acc0 results of
            (individualWarn, noMatchesWarn, docFiles1', wrongPaths) ->
              let wrongFieldWarnings =
                    [ PackageDistSuspiciousWarn
                      ( WrongFieldForExpectedDocFiles
                          extraDocFilesSupport
                          field
                          wrongPaths
                      )
                    | not (null wrongPaths)
                    ]
               in ( if noMatchesWarn
                      then
                        [PackageDistSuspiciousWarn (GlobNoMatch field glob)]
                          ++ individualWarn
                          ++ wrongFieldWarnings
                      else individualWarn ++ wrongFieldWarnings
                  , docFiles1'
                  )
          where
            checkGlobResult
              :: GlobResult FilePath
              -> ([PackageCheck], Bool, [FilePath], [FilePath])
              -> ([PackageCheck], Bool, [FilePath], [FilePath])
            checkGlobResult result (ws, noMatchesWarn, docFiles2, wrongPaths) =
              let noMatchesWarn' =
                    noMatchesWarn
                      && not (suppressesNoMatchesWarning result)
               in case getWarning field glob result of
                    -- No match: add warning and do no further check
                    Left w ->
                      ( w : ws
                      , noMatchesWarn'
                      , docFiles2
                      , wrongPaths
                      )
                    -- Match: check doc files
                    Right path ->
                      let path' = makeRelative root (normalise path)
                          (docFiles2', wrongPaths') =
                            checkDoc
                              isDocField
                              path'
                              docFiles2
                              wrongPaths
                       in ( ws
                          , noMatchesWarn'
                          , docFiles2'
                          , wrongPaths'
                          )

    -- Check whether a path is a desirable doc: if so, check if it is in the
    -- field "extra-doc-files".
    checkDoc
      :: Bool -- Is it "extra-doc-files" ?
      -> FilePath -- Path to test
      -> [FilePath] -- Pending doc files to check
      -> [FilePath] -- Previous wrong paths
      -> ([FilePath], [FilePath]) -- Updated paths
    checkDoc isDocField path docFiles wrongFieldPaths =
      if path `elem` docFiles
        then -- Found desirable doc file

          ( delete path docFiles
          , if isDocField then wrongFieldPaths else path : wrongFieldPaths
          )
        else -- Not a desirable doc file

          ( docFiles
          , wrongFieldPaths
          )

    -- Predicate for desirable documentation file on Hackage server
    isDesirableExtraDocFile :: ([FilePath], [FilePath]) -> FilePath -> Bool
    isDesirableExtraDocFile (basenames, extensions) path =
      basename `elem` basenames && ext `elem` extensions
      where
        (basename, ext) = splitExtension (map toLower path)

    -- Changelog patterns (basenames & extensions)
    -- Source: hackage-server/src/Distribution/Server/Packages/ChangeLog.hs
    desirableChangeLog =
      [ "news"
      , "changelog"
      , "change_log"
      , "changes"
      ]
    desirableChangeLogExtensions = ["", ".txt", ".md", ".markdown", ".rst"]
    -- [TODO] Check readme. Observations:
    --        • Readme is not necessary if package description is good.
    --        • Some readmes exists only for repository browsing.
    --        • There is currently no reliable way to check what a good
    --          description is; there will be complains if the criterion is
    --          based on the length or number of words (can of worms).
    -- -- Readme patterns
    -- -- Source: hackage-server/src/Distribution/Server/Packages/Readme.hs
    -- desirableReadme = ["readme"]
    desirableDocFiles = (desirableChangeLog, desirableChangeLogExtensions)

    -- If there's a missing directory in play, since our globs don't
    -- (currently) support disjunction, that will always mean there are no
    -- matches. The no matches error in this case is strictly less informative
    -- than the missing directory error, so sit on it.
    suppressesNoMatchesWarning (GlobMatch _) = True
    suppressesNoMatchesWarning (GlobWarnMultiDot _) = False
    suppressesNoMatchesWarning (GlobMissingDirectory _) = True

    getWarning
      :: String
      -> FilePath
      -> GlobResult FilePath
      -> Either PackageCheck FilePath
    getWarning _ _ (GlobMatch path) =
      Right path
    -- Before Cabal 2.4, the extensions of globs had to match the file
    -- exactly. This has been relaxed in 2.4 to allow matching only the
    -- suffix. This warning detects when pre-2.4 package descriptions are
    -- omitting files purely because of the stricter check.
    getWarning field glob (GlobWarnMultiDot file) =
      Left (PackageDistSuspiciousWarn (GlobExactMatch field glob file))
    getWarning field glob (GlobMissingDirectory dir) =
      Left (PackageDistSuspiciousWarn (GlobNoDir field glob dir))

-- | Check that setup dependencies, have proper bounds.
-- In particular, @base@ and @Cabal@ upper bounds are mandatory.
checkSetupVersions :: GenericPackageDescription -> [PackageCheck]
checkSetupVersions pkg =
  [ emitError nameStr
  | (name, vr) <- Map.toList deps
  , not (hasUpperBound vr)
  , let nameStr = unPackageName name
  , nameStr `elem` criticalPkgs
  ]
  where
    criticalPkgs = ["Cabal", "base"]
    deps = toDependencyVersionsMap (foldMap setupDepends . setupBuildInfo) pkg
    emitError nm =
      PackageDistInexcusable (UpperBoundSetup nm)

checkDuplicateModules :: GenericPackageDescription -> [PackageCheck]
checkDuplicateModules pkg =
  concatMap checkLib (maybe id (:) (condLibrary pkg) . map snd $ condSubLibraries pkg)
    ++ concatMap checkExe (map snd $ condExecutables pkg)
    ++ concatMap checkTest (map snd $ condTestSuites pkg)
    ++ concatMap checkBench (map snd $ condBenchmarks pkg)
  where
    -- the duplicate modules check is has not been thoroughly vetted for backpack
    checkLib = checkDups "library" (\l -> explicitLibModules l ++ map moduleReexportName (reexportedModules l))
    checkExe = checkDups "executable" exeModules
    checkTest = checkDups "test suite" testModules
    checkBench = checkDups "benchmark" benchmarkModules
    checkDups s getModules t =
      let sumPair (x, x') (y, y') = (x + x' :: Int, y + y' :: Int)
          mergePair (x, x') (y, y') = (x + x', max y y')
          maxPair (x, x') (y, y') = (max x x', max y y')
          libMap =
            foldCondTree
              Map.empty
              (\(_, v) -> Map.fromListWith sumPair . map (\x -> (x, (1, 1))) $ getModules v)
              (Map.unionWith mergePair) -- if a module may occur in nonexclusive branches count it twice strictly and once loosely.
              (Map.unionWith maxPair) -- a module occurs the max of times it might appear in exclusive branches
              t
          dupLibsStrict = Map.keys $ Map.filter ((> 1) . fst) libMap
          dupLibsLax = Map.keys $ Map.filter ((> 1) . snd) libMap
       in if not (null dupLibsLax)
            then
              [ PackageBuildImpossible
                  (DuplicateModule s dupLibsLax)
              ]
            else
              if not (null dupLibsStrict)
                then
                  [ PackageDistSuspicious
                      (PotentialDupModule s dupLibsStrict)
                  ]
                else []

-- ------------------------------------------------------------

-- * Utils

-- ------------------------------------------------------------

toDependencyVersionsMap :: (PackageDescription -> [Dependency]) -> GenericPackageDescription -> Map PackageName VersionRange
toDependencyVersionsMap selectDependencies pkg = case typicalPkg pkg of
  Right (pkgs', _) ->
    let
      self :: PackageName
      self = pkgName $ package pkgs'
     in
      Map.fromListWith intersectVersionRanges $
        [ (pname, vr)
        | Dependency pname vr _ <- selectDependencies pkgs'
        , pname /= self
        ]
  -- Just in case finalizePD fails for any reason,
  -- or if the package doesn't depend on the base package at all,
  -- no deps is no checks.
  _ -> Map.empty

quote :: String -> String
quote s = "'" ++ s ++ "'"

commaSep :: [String] -> String
commaSep = intercalate ", "

dups :: Ord a => [a] -> [a]
dups xs = [x | (x : _ : _) <- group (sort xs)]

fileExtensionSupportedLanguage :: FilePath -> Bool
fileExtensionSupportedLanguage path =
  isHaskell || isC
  where
    extension = takeExtension path
    isHaskell = extension `elem` [".hs", ".lhs"]
    isC = isJust (filenameCDialect extension)

-- | Whether a path is a good relative path.  We aren't worried about perfect
-- cross-platform compatibility here; this function just checks the paths in
-- the (local) @.cabal@ file, while only Hackage needs the portability.
--
-- >>> let test fp = putStrLn $ show (isGoodRelativeDirectoryPath fp) ++ "; " ++ show (isGoodRelativeFilePath fp)
--
-- Note that "foo./bar.hs" would be invalid on Windows.
--
-- >>> traverse_ test ["foo/bar/quu", "a/b.hs", "foo./bar.hs"]
-- Nothing; Nothing
-- Nothing; Nothing
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
-- >>> traverse_ test ["", "/tmp/src", "foo//bar", "foo/.", "foo/./bar", "foo/../bar"]
-- Just "empty path"; Just "empty path"
-- Just "posix absolute path"; Just "posix absolute path"
-- Just "empty path segment"; Just "empty path segment"
-- Just "trailing same directory segment: ."; Just "trailing same directory segment: ."
-- Just "same directory segment: ."; Just "same directory segment: ."
-- Just "parent directory segment: .."; Just "parent directory segment: .."
--
-- For the last case, 'isGoodRelativeGlob' doesn't warn:
--
-- >>> traverse_ (print . isGoodRelativeGlob) ["foo/../bar"]
-- Just "parent directory segment: .."
isGoodRelativeFilePath :: FilePath -> Maybe String
isGoodRelativeFilePath = state0
  where
    -- initial state
    state0 [] = Just "empty path"
    state0 (c : cs)
      | c == '.' = state1 cs
      | c == '/' = Just "posix absolute path"
      | otherwise = state5 cs

    -- after initial .
    state1 [] = Just "trailing dot segment"
    state1 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = state2 cs
      | otherwise = state5 cs

    -- after ./ or after / between segments
    state2 [] = Just "trailing slash"
    state2 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = Just "empty path segment"
      | otherwise = state5 cs

    -- after non-first segment's .
    state3 [] = Just "trailing same directory segment: ."
    state3 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = Just "same directory segment: ."
      | otherwise = state5 cs

    -- after ..
    state4 [] = Just "trailing parent directory segment: .."
    state4 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = Just "parent directory segment: .."
      | otherwise = state5 cs

    -- in a segment which is ok.
    state5 [] = Nothing
    state5 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = state2 cs
      | otherwise = state5 cs

-- | See 'isGoodRelativeFilePath'.
--
-- This is barebones function. We check whether the glob is a valid file
-- by replacing stars @*@ with @x@ses.
isGoodRelativeGlob :: FilePath -> Maybe String
isGoodRelativeGlob = isGoodRelativeFilePath . map f
  where
    f '*' = 'x'
    f c = c

-- | See 'isGoodRelativeFilePath'.
isGoodRelativeDirectoryPath :: FilePath -> Maybe String
isGoodRelativeDirectoryPath = state0
  where
    -- initial state
    state0 [] = Just "empty path"
    state0 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = Just "posix absolute path"
      | otherwise = state4 cs

    -- after initial ./ or after / between segments
    state1 [] = Nothing
    state1 (c : cs)
      | c == '.' = state2 cs
      | c == '/' = Just "empty path segment"
      | otherwise = state4 cs

    -- after non-first setgment's .
    state2 [] = Just "trailing same directory segment: ."
    state2 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = Just "same directory segment: ."
      | otherwise = state4 cs

    -- after ..
    state3 [] = Just "trailing parent directory segment: .."
    state3 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = Just "parent directory segment: .."
      | otherwise = state4 cs

    -- in a segment which is ok.
    state4 [] = Nothing
    state4 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = state1 cs
      | otherwise = state4 cs

    -- after initial .
    state5 [] = Nothing -- "."
    state5 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = state1 cs
      | otherwise = state4 cs

-- [Note: Good relative paths]
--
-- Using @kleene@ we can define an extended regex:
--
-- @
-- import Algebra.Lattice
-- import Kleene
-- import Kleene.ERE (ERE (..), intersections)
--
-- data C = CDot | CSlash | CChar
--   deriving (Eq, Ord, Enum, Bounded, Show)
--
-- reservedR :: ERE C
-- reservedR = notChar CSlash
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
--     | otherwise           -> 5
-- 1 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 2
--     | otherwise           -> 5
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | otherwise           -> 5
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | otherwise           -> 5
-- 4 -> \x -> if
--     | x <= CDot           -> 5
--     | otherwise           -> 5
-- 5+ -> \x -> if
--     | x <= CDot           -> 5
--     | x <= CSlash         -> 2
--     | otherwise           -> 5
-- @
--
-- and @dirPathR@:
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 5
--     | otherwise           -> 4
-- 1+ -> \x -> if
--     | x <= CDot           -> 2
--     | otherwise           -> 4
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | otherwise           -> 4
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | otherwise           -> 4
-- 4+ -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 1
--     | otherwise           -> 4
-- 5+ -> \x -> if
--     | x <= CDot           -> 3
--     | x <= CSlash         -> 1
--     | otherwise           -> 4
-- @

--
-- TODO: What we really want to do is test if there exists any
-- configuration in which the base version is unbounded above.
-- However that's a bit tricky because there are many possible
-- configurations. As a cheap easy and safe approximation we will
-- pick a single "typical" configuration and check if that has an
-- open upper bound. To get a typical configuration we finalise
-- using no package index and the current platform.
typicalPkg
  :: GenericPackageDescription
  -> Either [Dependency] (PackageDescription, FlagAssignment)
typicalPkg =
  finalizePD
    mempty
    defaultComponentRequestedSpec
    (const True)
    buildPlatform
    ( unknownCompilerInfo
        (CompilerId buildCompilerFlavor nullVersion)
        NoAbiTag
    )
    []

addConditionalExp :: String -> String
addConditionalExp expl =
  expl
    ++ " Alternatively, if you want to use this, make it conditional based "
    ++ "on a Cabal configuration flag (with 'manual: True' and 'default: "
    ++ "False') and enable that flag during development."
