{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=redundant-constraints #-}

-- | 'GenericPackageDescription' Field descriptions
module Distribution.PackageDescription.FieldGrammar
  ( -- * Package description
    packageDescriptionFieldGrammar
  , CompatDataDir (..)
  , CompatLicenseFile (..)

    -- * Library
  , libraryFieldGrammar

    -- * Foreign library
  , foreignLibFieldGrammar

    -- * Executable
  , executableFieldGrammar

    -- * Test suite
  , TestSuiteStanza
  , TestSuiteStanzaWith (..)
  , testSuiteFieldGrammar
  , validateTestSuite
  , unvalidateTestSuite

    -- ** Lenses
  , testStanzaTestType
  , testStanzaMainIs
  , testStanzaTestModule
  , testStanzaBuildInfo

    -- * Benchmark
  , BenchmarkStanza
  , BenchmarkStanzaWith (..)
  , benchmarkFieldGrammar
  , validateBenchmark
  , unvalidateBenchmark

    -- * Field grammars
  , formatDependencyList
  , formatExposedModules
  , formatExtraSourceFiles
  , formatHsSourceDirs
  , formatMixinList
  , formatOtherExtensions
  , formatOtherModules

    -- ** Lenses
  , benchmarkStanzaBenchmarkType
  , benchmarkStanzaMainIs
  , benchmarkStanzaBenchmarkModule
  , benchmarkStanzaBuildInfo

    -- * Flag
  , flagFieldGrammar

    -- * Source repository
  , sourceRepoFieldGrammar

    -- * Setup build info
  , setupBInfoFieldGrammar

    -- * Component build info
  , buildInfoFieldGrammar
  , MiniBuildInfo (..)
  , miniBuildInfoFieldGrammar
  , BuildInfoConstraint
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Language.Haskell.Extension
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compat.Newtype (Newtype, pack', unpack')
import Distribution.Compiler (CompilerFlavor (..), PerCompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec
import Distribution.Pretty (Pretty (..), prettyShow, showToken)
import Distribution.Utils.Path
import Distribution.Version (Version, VersionRange)

import Distribution.Types.Annotation

import Data.Kind

import qualified Data.ByteString.Char8 as BS8
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Types.Lens as L

-------------------------------------------------------------------------------
-- PackageDescription
-------------------------------------------------------------------------------

packageDescriptionFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , c (Identity BuildType)
     , c (Identity PackageName)
     , c (Identity Version)
     , c (List FSep TestedWith (CompilerFlavor, VersionRange))
     , c (List VCat (RelativePathNT DataDir File) (RelativePath DataDir File))
     , c (List VCat (RelativePathNT Pkg File) (RelativePath Pkg File))
     , c (List FSep (RelativePathNT Pkg File) (RelativePath Pkg File))
     , c CompatLicenseFile
     , c CompatDataDir
     )
  => g mod (PackageDescriptionWith mod) (PackageDescriptionWith mod)
packageDescriptionFieldGrammar =
  PackageDescription
    <$> optionalFieldDefAla' "cabal-version" SpecVersion L.specVersion CabalSpecV1_0
    <*> blurFieldGrammar L.package packageIdentifierGrammar
    <*> optionalFieldDefAla "license" SpecLicense L.licenseRaw (Left SPDX.NONE)
    <*> licenseFilesGrammar
    <*> freeTextFieldDefST "copyright" L.copyright
    <*> freeTextFieldDefST "maintainer" L.maintainer
    <*> freeTextFieldDefST "author" L.author
    <*> freeTextFieldDefST "stability" L.stability
    <*> monoidalFieldAla "tested-with" (alaList' FSep TestedWith) L.testedWith
    <*> freeTextFieldDefST "homepage" L.homepage
    <*> freeTextFieldDefST "package-url" L.pkgUrl
    <*> freeTextFieldDefST "bug-reports" L.bugReports
    <*> pure [] -- source-repos are stanza
    <*> freeTextFieldDefST "synopsis" L.synopsis
    <*> freeTextFieldDefST "description" L.description
    <*> freeTextFieldDefST "category" L.category
    <*> prefixedFields "x-" L.customFieldsPD
    <*> optionalField "build-type" L.buildTypeRaw
    <*> pure Nothing -- custom-setup
    -- components
    <*> pure Nothing -- lib
    <*> pure [] -- sub libs
    <*> pure [] -- executables
    <*> pure [] -- foreign libs
    <*> pure [] -- test suites
    <*> pure [] -- benchmarks
    --  * Files
    <*> monoidalFieldAla "data-files" (alaList' VCat RelativePathNT) L.dataFiles
    <*> optionalFieldDefAla "data-dir" CompatDataDir L.dataDir sameDirectory
      ^^^ fmap (\x -> if null (getSymbolicPath x) then sameDirectory else x) -- map empty directories to "."
    <*> monoidalFieldAla "extra-source-files" formatExtraSourceFiles L.extraSrcFiles
    <*> monoidalFieldAla "extra-tmp-files" (alaList' VCat RelativePathNT) L.extraTmpFiles
    <*> monoidalFieldAla "extra-doc-files" formatExtraSourceFiles L.extraDocFiles
    <*> monoidalFieldAla "extra-files" formatExtraSourceFiles L.extraFiles
      ^^^ availableSince CabalSpecV3_14 []
  where
    packageIdentifierGrammar =
      PackageIdentifier
        <$> uniqueField' @mod @c @g @_ @PackageName "name" (L.pkgName @mod)
        <*> uniqueField' @mod @c @g @_ @Version "version" L.pkgVersion

    licenseFilesGrammar =
      (++)
        -- TODO: neither field is deprecated
        -- should we pretty print license-file if there's single license file
        -- and license-files when more
        <$> monoidalFieldAla "license-file" CompatLicenseFile L.licenseFiles
        <*> monoidalFieldAla "license-files" (alaList' FSep RelativePathNT) L.licenseFiles
          ^^^ hiddenField

-------------------------------------------------------------------------------
-- Library
-------------------------------------------------------------------------------

libraryFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , BuildInfoConstraint mod c g
     , c (Identity LibraryVisibility)
     , c (List CommaVCat (Identity ModuleReexport) ModuleReexport)
     )
  => LibraryName
  -> g mod (LibraryWith mod) (LibraryWith mod)
libraryFieldGrammar n =
  Library
    <$> pure Nothing
    <*> pure n
    <*> monoidalFieldAla "exposed-modules" formatExposedModules L.exposedModules
    <*> monoidalFieldAla "reexported-modules" (alaList CommaVCat) L.reexportedModules
    <*> monoidalFieldAla "signatures" (alaList' VCat MQuoted) L.signatures
      ^^^ availableSince CabalSpecV2_0 []
    <*> booleanFieldDef' "exposed" L.libExposed True
    <*> visibilityField
    <*> blurFieldGrammar L.libBuildInfo (buildInfoFieldGrammar @mod)
  where
    visibilityField = case n of
      -- nameless/"main" libraries are public
      LMainLibName -> pure LibraryVisibilityPublic
      -- named libraries have the field
      LSubLibName _ ->
        optionalFieldDef "visibility" L.libVisibility LibraryVisibilityPrivate
          ^^^ availableSince CabalSpecV3_0 LibraryVisibilityPrivate

-- {-# SPECIALIZE libraryFieldGrammar :: LibraryName -> ParsecFieldGrammar' LibraryAnn #-}
-- {-# SPECIALIZE libraryFieldGrammar :: LibraryName -> PrettyFieldGrammar' LibraryAnn #-}

-------------------------------------------------------------------------------
-- Foreign library
-------------------------------------------------------------------------------

foreignLibFieldGrammar
  :: forall (mod :: ParsingPhase) c g
   . ( BuildInfoConstraint mod c g
     , FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , c (Identity ForeignLibType)
     , c (Identity LibVersionInfo)
     , c (Identity Version)
     , c (List FSep (Identity ForeignLibOption) ForeignLibOption)
     , c (ListWith Abst FSep (RelativePathNT Source File) (RelativePath Source File))
     )
  => UnqualComponentName
  -> g mod (ForeignLibWith mod) (ForeignLibWith mod)
foreignLibFieldGrammar n =
  ForeignLib n
    <$> optionalFieldDef "type" L.foreignLibType ForeignLibTypeUnknown
    <*> monoidalFieldAla "options" (alaList FSep) L.foreignLibOptions
    <*> blurFieldGrammar (L.foreignLibBuildInfo @mod) (buildInfoFieldGrammar @mod)
    <*> optionalField "lib-version-info" L.foreignLibVersionInfo
    <*> optionalField "lib-version-linux" L.foreignLibVersionLinux
    <*> monoidalFieldAla "mod-def-file" (alaList' FSep RelativePathNT) L.foreignLibModDefFile

-- {-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' ForeignLib #-}
-- {-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' ForeignLib #-}

-------------------------------------------------------------------------------
-- Executable
-------------------------------------------------------------------------------

executableFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , BuildInfoConstraint mod c g
     , c (Identity ExecutableScope)
     , c (RelativePathNT Source File)
     )
  => UnqualComponentName
  -> g mod (ExecutableWith mod) (ExecutableWith mod)
executableFieldGrammar n =
  Executable n
    -- main-is is optional as conditional blocks don't have it
    <$> optionalFieldDefAla "main-is" RelativePathNT L.modulePath (unsafeMakeSymbolicPath "")
    <*> optionalFieldDef "scope" L.exeScope ExecutablePublic
      ^^^ availableSince CabalSpecV2_0 ExecutablePublic
    <*> blurFieldGrammar (L.buildInfo @mod) (buildInfoFieldGrammar @mod)
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' Executable #-}
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' Executable #-}

-------------------------------------------------------------------------------
-- TestSuite
-------------------------------------------------------------------------------

type TestSuiteStanza = TestSuiteStanzaWith Abst

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanzaWith (mod :: ParsingPhase) = TestSuiteStanza
  { _testStanzaTestType :: Maybe TestType
  , _testStanzaMainIs :: Maybe (RelativePath Source File)
  , _testStanzaTestModule :: Maybe ModuleName
  , _testStanzaBuildInfo :: BuildInfoWith mod
  , _testStanzaCodeGenerators :: [String]
  }

instance L.HasBuildInfoWith mod (TestSuiteStanzaWith mod) where
  buildInfo f t = (\x -> t{_testStanzaBuildInfo = x}) <$> f (_testStanzaBuildInfo t)

testStanzaTestType :: Lens' (TestSuiteStanzaWith mod) (Maybe TestType)
testStanzaTestType f s = fmap (\x -> s{_testStanzaTestType = x}) (f (_testStanzaTestType s))
{-# INLINE testStanzaTestType #-}

testStanzaMainIs :: Lens' (TestSuiteStanzaWith mod) (Maybe (RelativePath Source File))
testStanzaMainIs f s = fmap (\x -> s{_testStanzaMainIs = x}) (f (_testStanzaMainIs s))
{-# INLINE testStanzaMainIs #-}

testStanzaTestModule :: Lens' (TestSuiteStanzaWith mod) (Maybe ModuleName)
testStanzaTestModule f s = fmap (\x -> s{_testStanzaTestModule = x}) (f (_testStanzaTestModule s))
{-# INLINE testStanzaTestModule #-}

testStanzaBuildInfo :: Lens' (TestSuiteStanzaWith mod) (BuildInfoWith mod)
testStanzaBuildInfo f s = fmap (\x -> s{_testStanzaBuildInfo = x}) (f (_testStanzaBuildInfo s))
{-# INLINE testStanzaBuildInfo #-}

testStanzaCodeGenerators :: Lens' (TestSuiteStanzaWith mod) [String]
testStanzaCodeGenerators f s = fmap (\x -> s{_testStanzaCodeGenerators = x}) (f (_testStanzaCodeGenerators s))
{-# INLINE testStanzaCodeGenerators #-}

testSuiteFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , c (Identity ModuleName)
     , c (Identity TestType)
     , c (List CommaFSep Token String)
     , c (RelativePathNT Source File)
     , BuildInfoConstraint mod c g
     )
  => g mod (TestSuiteStanzaWith mod) (TestSuiteStanzaWith mod)
testSuiteFieldGrammar =
  TestSuiteStanza
    <$> optionalField "type" testStanzaTestType
    <*> optionalFieldAla "main-is" RelativePathNT testStanzaMainIs
    <*> optionalField "test-module" testStanzaTestModule
    <*> blurFieldGrammar testStanzaBuildInfo (buildInfoFieldGrammar @mod)
    <*> monoidalFieldAla "code-generators" (alaList' CommaFSep Token) testStanzaCodeGenerators
      ^^^ availableSince CabalSpecV3_8 []

validateTestSuite
  :: Monoid (TestSuiteWith phase)
  => CabalSpecVersion
  -> Position
  -> TestSuiteStanzaWith phase
  -> ParseResult src (TestSuiteWith phase)
validateTestSuite cabalSpecVersion pos stanza = case testSuiteType of
  Nothing -> pure basicTestSuite
  Just tt@(TestTypeUnknown _ _) ->
    pure
      basicTestSuite
        { testInterface = TestSuiteUnsupported tt
        }
  Just tt
    | tt `notElem` knownTestTypes ->
        pure
          basicTestSuite
            { testInterface = TestSuiteUnsupported tt
            }
  Just tt@(TestTypeExe ver) -> case _testStanzaMainIs stanza of
    Nothing -> do
      parseFailure pos (missingField "main-is" tt)
      pure mempty
    Just file -> do
      when (isJust (_testStanzaTestModule stanza)) $
        parseWarning pos PWTExtraBenchmarkModule (extraField "test-module" tt)
      pure
        basicTestSuite
          { testInterface = TestSuiteExeV10 ver file
          }
  Just tt@(TestTypeLib ver) -> case _testStanzaTestModule stanza of
    Nothing -> do
      parseFailure pos (missingField "test-module" tt)
      pure mempty
    Just module_ -> do
      when (isJust (_testStanzaMainIs stanza)) $
        parseWarning pos PWTExtraMainIs (extraField "main-is" tt)
      pure
        basicTestSuite
          { testInterface = TestSuiteLibV09 ver module_
          }
  where
    testSuiteType =
      _testStanzaTestType stanza
        <|> do
          guard (cabalSpecVersion >= CabalSpecV3_8)

          testTypeExe <$ _testStanzaMainIs stanza
        <|> testTypeLib <$ _testStanzaTestModule stanza

    missingField name tt =
      "The '"
        ++ name
        ++ "' field is required for the "
        ++ prettyShow tt
        ++ " test suite type."

    extraField name tt =
      "The '"
        ++ name
        ++ "' field is not used for the '"
        ++ prettyShow tt
        ++ "' test suite type."
    basicTestSuite =
      emptyTestSuite
        { testBuildInfo = _testStanzaBuildInfo stanza
        , testCodeGenerators = _testStanzaCodeGenerators stanza
        }

unvalidateTestSuite :: TestSuite -> TestSuiteStanza
unvalidateTestSuite t =
  TestSuiteStanza
    { _testStanzaTestType = ty
    , _testStanzaMainIs = ma
    , _testStanzaTestModule = mo
    , _testStanzaBuildInfo = testBuildInfo t
    , _testStanzaCodeGenerators = testCodeGenerators t
    }
  where
    (ty, ma, mo) = case testInterface t of
      TestSuiteExeV10 ver file -> (Just $ TestTypeExe ver, Just file, Nothing)
      TestSuiteLibV09 ver modu -> (Just $ TestTypeLib ver, Nothing, Just modu)
      _ -> (Nothing, Nothing, Nothing)

-------------------------------------------------------------------------------
-- Benchmark
-------------------------------------------------------------------------------

type BenchmarkStanza = BenchmarkStanzaWith Abst

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanzaWith (mod :: ParsingPhase) = BenchmarkStanza
  { _benchmarkStanzaBenchmarkType :: Maybe BenchmarkType
  , _benchmarkStanzaMainIs :: Maybe (RelativePath Source File)
  , _benchmarkStanzaBenchmarkModule :: Maybe ModuleName
  , _benchmarkStanzaBuildInfo :: BuildInfoWith mod
  }

instance L.HasBuildInfoWith mod (BenchmarkStanzaWith mod) where
  buildInfo f b = (\x -> b{_benchmarkStanzaBuildInfo = x}) <$> f (_benchmarkStanzaBuildInfo b)

benchmarkStanzaBenchmarkType :: Lens' (BenchmarkStanzaWith mod) (Maybe BenchmarkType)
benchmarkStanzaBenchmarkType f s = fmap (\x -> s{_benchmarkStanzaBenchmarkType = x}) (f (_benchmarkStanzaBenchmarkType s))
{-# INLINE benchmarkStanzaBenchmarkType #-}

benchmarkStanzaMainIs :: Lens' (BenchmarkStanzaWith mod) (Maybe (RelativePath Source File))
benchmarkStanzaMainIs f s = fmap (\x -> s{_benchmarkStanzaMainIs = x}) (f (_benchmarkStanzaMainIs s))
{-# INLINE benchmarkStanzaMainIs #-}

benchmarkStanzaBenchmarkModule :: Lens' (BenchmarkStanzaWith mod) (Maybe ModuleName)
benchmarkStanzaBenchmarkModule f s = fmap (\x -> s{_benchmarkStanzaBenchmarkModule = x}) (f (_benchmarkStanzaBenchmarkModule s))
{-# INLINE benchmarkStanzaBenchmarkModule #-}

benchmarkStanzaBuildInfo :: Lens' (BenchmarkStanzaWith mod) (BuildInfoWith mod)
benchmarkStanzaBuildInfo f s = fmap (\x -> s{_benchmarkStanzaBuildInfo = x}) (f (_benchmarkStanzaBuildInfo s))
{-# INLINE benchmarkStanzaBuildInfo #-}

benchmarkFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , BuildInfoConstraint mod c g
     , c (Identity BenchmarkType)
     , c (Identity ModuleName)
     , c (RelativePathNT Source File)
     )
  => g mod (BenchmarkStanzaWith mod) (BenchmarkStanzaWith mod)
benchmarkFieldGrammar =
  BenchmarkStanza
    <$> optionalField "type" benchmarkStanzaBenchmarkType
    <*> optionalFieldAla "main-is" RelativePathNT benchmarkStanzaMainIs
    <*> optionalField "benchmark-module" benchmarkStanzaBenchmarkModule
    <*> blurFieldGrammar benchmarkStanzaBuildInfo buildInfoFieldGrammar

validateBenchmark
  :: Monoid (BenchmarkWith phase)
  => CabalSpecVersion
  -> Position
  -> BenchmarkStanzaWith phase
  -> ParseResult src (BenchmarkWith phase)
validateBenchmark cabalSpecVersion pos stanza = case benchmarkStanzaType of
  Nothing ->
    pure
      emptyBenchmark
        { benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }
  Just tt@(BenchmarkTypeUnknown _ _) ->
    pure
      emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }
  Just tt
    | tt `notElem` knownBenchmarkTypes ->
        pure
          emptyBenchmark
            { benchmarkInterface = BenchmarkUnsupported tt
            , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
            }
  Just tt@(BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
    Nothing -> do
      parseFailure pos (missingField "main-is" tt)
      pure mempty
    Just file -> do
      when (isJust (_benchmarkStanzaBenchmarkModule stanza)) $
        parseWarning pos PWTExtraBenchmarkModule (extraField "benchmark-module" tt)
      pure
        emptyBenchmark
          { benchmarkInterface = BenchmarkExeV10 ver file
          , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
          }
  where
    benchmarkStanzaType =
      _benchmarkStanzaBenchmarkType stanza <|> do
        guard (cabalSpecVersion >= CabalSpecV3_8)

        benchmarkTypeExe <$ _benchmarkStanzaMainIs stanza

    missingField name tt =
      "The '"
        ++ name
        ++ "' field is required for the "
        ++ prettyShow tt
        ++ " benchmark type."

    extraField name tt =
      "The '"
        ++ name
        ++ "' field is not used for the '"
        ++ prettyShow tt
        ++ "' benchmark type."

unvalidateBenchmark :: Benchmark -> BenchmarkStanza
unvalidateBenchmark b =
  BenchmarkStanza
    { _benchmarkStanzaBenchmarkType = ty
    , _benchmarkStanzaMainIs = ma
    , _benchmarkStanzaBenchmarkModule = mo
    , _benchmarkStanzaBuildInfo = benchmarkBuildInfo b
    }
  where
    (ty, ma, mo) = case benchmarkInterface b of
      BenchmarkExeV10 ver ma'
        | getSymbolicPath ma' == "" ->
            (Just $ BenchmarkTypeExe ver, Nothing, Nothing)
        | otherwise ->
            (Just $ BenchmarkTypeExe ver, Just ma', Nothing)
      _ -> (Nothing, Nothing, Nothing)

-------------------------------------------------------------------------------
-- Build info
-------------------------------------------------------------------------------

-- TODO(leana8959): redundant costraint here are not caught. reproduce this and find a solution.
type BuildInfoConstraint (mod :: ParsingPhase) (c :: Type -> Constraint) (g :: ParsingPhase -> Type -> Type -> Type) =
  ( Newtype [AttachPosition mod (Annotate mod LegacyExeDependency)] (ListWith mod CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
  , c (ListWith mod CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
  , Newtype [AttachPosition mod (Annotate mod ExeDependency)] (ListWith mod CommaFSep (Identity ExeDependency) ExeDependency)
  , c (ListWith mod CommaFSep (Identity ExeDependency) ExeDependency)
  , Newtype [AttachPosition mod (Annotate mod String)] (ListWith mod NoCommaFSep Token' String)
  , c (ListWith mod NoCommaFSep Token' String)
  , Newtype [AttachPosition mod (Annotate mod PkgconfigDependency)] (ListWith mod CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
  , c (ListWith mod CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
  , Newtype [AttachPosition mod (Annotate mod (RelativePath Framework File))] (ListWith mod FSep (RelativePathNT Framework File) (RelativePath Framework File))
  , c (ListWith mod FSep (RelativePathNT Framework File) (RelativePath Framework File))
  , Newtype [AttachPosition mod (Annotate mod (SymbolicPath Pkg (Dir Framework)))] (ListWith mod FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
  , c (ListWith mod FSep (SymbolicPathNT Pkg (Dir Framework)) (SymbolicPath Pkg (Dir Framework)))
  , Newtype [AttachPosition mod (Annotate mod (SymbolicPath Pkg File))] (ListWith mod VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
  , c (ListWith mod VCat (SymbolicPathNT Pkg File) (SymbolicPath Pkg File))
  , -- is a monoid with or without annotation, for hsSourceDirs compat
    Monoid (PreserveGrouping mod (AttachPositions mod [AttachPosition mod (Annotate mod (SymbolicPath Pkg (Dir Source)))]))
  , Newtype [AttachPosition mod (Annotate mod (SymbolicPath Pkg (Dir Source)))] (ListWith mod FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
  , c (ListWith mod FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
  , Newtype [AttachPosition mod (Annotate mod ModuleName)] (ListWith mod VCat (MQuoted ModuleName) ModuleName)
  , c (ListWith mod VCat (MQuoted ModuleName) ModuleName)
  , c (List VCat (MQuoted ModuleName) ModuleName)
  , c (MQuoted Language)
  , c (List FSep (MQuoted Language) Language)
  , c (List FSep (MQuoted Extension) Extension)
  , c (List VCat Token String)
  , c (List FSep (SymbolicPathNT Pkg (Dir Lib)) (SymbolicPath Pkg (Dir Lib)))
  , c (List FSep (SymbolicPathNT Pkg (Dir Include)) (SymbolicPath Pkg (Dir Include)))
  , c (List FSep (SymbolicPathNT Include File) (SymbolicPath Include File))
  , c (List FSep (RelativePathNT Include File) (RelativePath Include File))
  , c (List NoCommaFSep Token' String)
  , Newtype
      [AttachPosition mod (Annotate mod (DependencyWith mod))]
      (ListWith mod CommaVCat (Identity (DependencyWith mod)) (DependencyWith mod))
  , c (ListWith mod CommaVCat (Identity (DependencyWith mod)) (DependencyWith mod))
  , c (List CommaVCat (Identity Mixin) Mixin)
  , Monoid (MonoidalFieldAla mod [AttachPosition mod (Annotate mod String)])
  , Monoid (MonoidalFieldAla mod [AttachPosition mod (Annotate mod (SymbolicPath Pkg File))])
  , HsSourceDirsGrammarConstr mod c g
  )

buildInfoFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , BuildInfoConstraint mod c g
     )
  => g mod (BuildInfoWith mod) (BuildInfoWith mod)
buildInfoFieldGrammar = do
  buildable <- booleanFieldDef' "buildable" L.buildable True
  buildTools <-
    monoidalFieldAla' "build-tools" (alaListWith @mod @CommaFSep @LegacyExeDependency) L.buildTools
      ^^^ deprecatedSince
        CabalSpecV2_0
        "Please use 'build-tool-depends' field"
      ^^^ removedIn
        CabalSpecV3_0
        "Please use 'build-tool-depends' field."
  buildToolDepends <- monoidalFieldAla' "build-tool-depends" (alaListWith @mod @CommaFSep @ExeDependency) L.buildToolDepends
  cppOptions <- monoidalFieldAla' "cpp-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.cppOptions
  asmOptions <-
    monoidalFieldAla' "asm-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.asmOptions
      ^^^ availableSince CabalSpecV3_0 mempty
  cmmOptions <-
    monoidalFieldAla' "cmm-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.cmmOptions
      ^^^ availableSince CabalSpecV3_0 mempty
  ccOptions <- monoidalFieldAla' "cc-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.ccOptions
  cxxOptions <-
    monoidalFieldAla' "cxx-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.cxxOptions
      ^^^ availableSince CabalSpecV2_2 mempty
  jsppOptions <-
    monoidalFieldAla' "jspp-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.jsppOptions
      ^^^ availableSince CabalSpecV3_16 mempty
  ldOptions <- monoidalFieldAla' "ld-options" (alaListWith' @mod @NoCommaFSep @Token' @String) L.ldOptions
  hsc2hsOptions <-
    monoidalFieldAla' "hsc2hsOptions" (alaListWith' @mod @NoCommaFSep @Token' @String) L.hsc2hsOptions
      ^^^ availableSince CabalSpecV3_6 mempty
  pkgconfigDepends <- monoidalFieldAla' "pkgconfig-depends" (alaListWith @mod @CommaFSep @PkgconfigDependency) L.pkgconfigDepends
  frameworks <- monoidalFieldAla' "frameworks" (alaListWith' @mod @FSep @(RelativePathNT Framework File) @(RelativePath Framework File)) L.frameworks
  extraFrameworkDirs <- monoidalFieldAla' "extra-framework-dirs" (alaListWith' @mod @FSep @(SymbolicPathNT Pkg (Dir Framework)) @(SymbolicPath Pkg (Dir Framework))) L.extraFrameworkDirs
  asmSources <-
    monoidalFieldAla' "asm-sources" (alaListWith' @mod @VCat @(SymbolicPathNT Pkg File) @(SymbolicPath Pkg File)) L.asmSources
      ^^^ availableSince CabalSpecV3_0 mempty
  cmmSources <-
    monoidalFieldAla' "cmm-sources" (alaListWith' @mod @VCat @(SymbolicPathNT Pkg File) @(SymbolicPath Pkg File)) L.cmmSources
      ^^^ availableSince CabalSpecV3_0 mempty
  cSources <- monoidalFieldAla' "c-sources" (alaListWith' @mod @VCat @(SymbolicPathNT Pkg File) @(SymbolicPath Pkg File)) L.cSources
  cxxSources <-
    monoidalFieldAla' "cxx-sources" (alaListWith' @mod @VCat @(SymbolicPathNT Pkg File) @(SymbolicPath Pkg File)) L.cxxSources
      ^^^ availableSince CabalSpecV2_2 mempty
  jsSources <- monoidalFieldAla' "js-sources" (alaListWith' @mod @VCat @(SymbolicPathNT Pkg File) @(SymbolicPath Pkg File)) L.jsSources
  hsSourceDirs <- hsSourceDirsGrammar @mod
  otherModules <- monoidalFieldAla' "other-modules" (formatOtherModules @mod) L.otherModules

  -- This section uses legacy monoidalFieldAla and doesn't handle trivia
  virtualModules <-
    monoidalFieldAla "virtual-modules" (alaList' VCat MQuoted) L.virtualModules
      ^^^ availableSince CabalSpecV2_2 []
  autogenModules <-
    monoidalFieldAla "autogen-modules" (alaList' VCat MQuoted) L.autogenModules
      ^^^ availableSince CabalSpecV2_0 []
  defaultLanguage <-
    optionalFieldAla "default-language" MQuoted L.defaultLanguage
      ^^^ availableSince CabalSpecV1_10 Nothing
  otherLanguages <-
    monoidalFieldAla "other-languages" (alaList' FSep MQuoted) L.otherLanguages
      ^^^ availableSince CabalSpecV1_10 []
  defaultExtensions <-
    monoidalFieldAla "default-extensions" (alaList' FSep MQuoted) L.defaultExtensions
      ^^^ availableSince CabalSpecV1_10 []
  otherExtensions <-
    monoidalFieldAla "other-extensions" formatOtherExtensions L.otherExtensions
      ^^^ availableSinceWarn CabalSpecV1_10
  oldExtensions <-
    monoidalFieldAla "extensions" (alaList' FSep MQuoted) L.oldExtensions
      ^^^ deprecatedSince
        CabalSpecV1_12
        "Please use 'default-extensions' or 'other-extensions' fields."
      ^^^ removedIn
        CabalSpecV3_0
        "Please use 'default-extensions' or 'other-extensions' fields."
  extraLibs <- monoidalFieldAla "extra-libraries" (alaList' VCat Token) L.extraLibs
  extraLibsStatic <-
    monoidalFieldAla "extra-libraries-static" (alaList' VCat Token) L.extraLibsStatic
      ^^^ availableSince CabalSpecV3_8 []
  extraGHCiLibs <- monoidalFieldAla "extra-ghci-libraries" (alaList' VCat Token) L.extraGHCiLibs
  extraBundledLibs <- monoidalFieldAla "extra-bundled-libraries" (alaList' VCat Token) L.extraBundledLibs
  extraLibFlavours <- monoidalFieldAla "extra-library-flavours" (alaList' VCat Token) L.extraLibFlavours
  extraDynLibFlavours <-
    monoidalFieldAla "extra-dynamic-library-flavours" (alaList' VCat Token) L.extraDynLibFlavours
      ^^^ availableSince CabalSpecV3_0 []
  extraLibDirs <- monoidalFieldAla "extra-lib-dirs" (alaList' FSep SymbolicPathNT) L.extraLibDirs
  extraLibDirsStatic <-
    monoidalFieldAla "extra-lib-dirs-static" (alaList' FSep SymbolicPathNT) L.extraLibDirsStatic
      ^^^ availableSince CabalSpecV3_8 []
  includeDirs <- monoidalFieldAla "include-dirs" (alaList' FSep SymbolicPathNT) L.includeDirs
  includes <- monoidalFieldAla "includes" (alaList' FSep SymbolicPathNT) L.includes
  autogenIncludes <-
    monoidalFieldAla "autogen-includes" (alaList' FSep RelativePathNT) L.autogenIncludes
      ^^^ availableSince CabalSpecV3_0 []
  installIncludes <- monoidalFieldAla "install-includes" (alaList' FSep RelativePathNT) L.installIncludes
  options <- optionsFieldGrammar
  profOptions <- profOptionsFieldGrammar
  sharedOptions <- sharedOptionsFieldGrammar
  profSharedOptions <- profSharedOptionsFieldGrammar
  let staticOptions = mempty
  customFieldsBI <- prefixedFields "x-" L.customFieldsBI

  targetBuildDepends <- monoidalFieldAla' "build-depends" (formatDependencyList @mod) L.targetBuildDepends

  mixins <-
    monoidalFieldAla "mixins" formatMixinList L.mixins
      ^^^ availableSince CabalSpecV2_0 mempty
  pure (BuildInfo{..})
{-# SPECIALIZE buildInfoFieldGrammar :: ParsecFieldGrammarWith Conc BuildInfoAnn BuildInfoAnn #-}
{-# SPECIALIZE buildInfoFieldGrammar :: ParsecFieldGrammarWith Abst BuildInfo BuildInfo #-}
{-# SPECIALIZE buildInfoFieldGrammar :: PrettyFieldGrammarWith Conc BuildInfoAnn BuildInfoAnn #-}
{-# SPECIALIZE buildInfoFieldGrammar :: PrettyFieldGrammarWith Abst BuildInfo BuildInfo #-}

data MiniBuildInfo (m :: ParsingPhase) = MiniBuildInfo
  { miniTargetBuildDepends :: MonoidalFieldAla m [AttachPosition m (Annotate m (DependencyWith m))]
  }

deriving instance Show (MiniBuildInfo Conc)
deriving instance Show (MiniBuildInfo Abst)

miniTargetBuildDependsLens
  :: forall mod f
   . Functor f
  => (MonoidalFieldAla mod [AttachPosition mod (Annotate mod (DependencyWith mod))] -> f (MonoidalFieldAla mod [AttachPosition mod (Annotate mod (DependencyWith mod))]))
  -> MiniBuildInfo mod
  -> f (MiniBuildInfo mod)
miniTargetBuildDependsLens f s = fmap (\x -> s{miniTargetBuildDepends = x}) (f (miniTargetBuildDepends s))

miniBuildInfoFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , Newtype
        [AttachPosition mod (Annotate mod (DependencyWith mod))]
        (ListWith mod CommaVCat (Identity (DependencyWith mod)) (DependencyWith mod))
     , c (ListWith mod CommaVCat (Identity (DependencyWith mod)) (DependencyWith mod))
     )
  => g mod (MiniBuildInfo mod) (MiniBuildInfo mod)
miniBuildInfoFieldGrammar =
  MiniBuildInfo
    <$> monoidalFieldAla' "build-depends" (formatDependencyList @mod) miniTargetBuildDependsLens

type HsSourceDirsGrammarConstr (ph :: ParsingPhase) (c :: Type -> Constraint) (g :: ParsingPhase -> Type -> Type -> Type) =
  ( -- is a monoid with or without annotation
    Monoid
      (MonoidalFieldAla ph [AttachPosition ph (Annotate ph (SymbolicPath Pkg (Dir Source)))])
  , Newtype
      [AttachPosition ph (Annotate ph (SymbolicPath Pkg (Dir Source)))]
      (ListWith ph FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
  , c (ListWith ph FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source)))
  )

hsSourceDirsGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     , HsSourceDirsGrammarConstr mod c g
     )
  => g mod (BuildInfoWith mod) (MonoidalFieldAla mod [AttachPosition mod (Annotate mod (SymbolicPath Pkg (Dir Source)))])
hsSourceDirsGrammar =
  (<>)
    <$> monoidalFieldAla' "hs-source-dirs" (alaListWith' @mod @FSep @(SymbolicPathNT Pkg (Dir Source)) @(SymbolicPath Pkg (Dir Source))) L.hsSourceDirs
    <*> monoidalFieldAla' "hs-source-dir" (alaListWith' @mod @FSep @(SymbolicPathNT Pkg (Dir Source)) @(SymbolicPath Pkg (Dir Source))) wrongLens
      --- https://github.com/haskell/cabal/commit/49e3cdae3bdf21b017ccd42e66670ca402e22b44
      ^^^ deprecatedSince CabalSpecV1_2 "Please use 'hs-source-dirs'"
      ^^^ removedIn CabalSpecV3_0 "Please use 'hs-source-dirs' field."
  where
    wrongLens f bi = (\fps -> set (L.hsSourceDirs @mod) fps bi) <$> f mempty

-- {-# SPECIALIZE hsSourceDirsGrammar :: ParsecFieldGrammar BuildInfoAnn [SymbolicPath Pkg (Dir Source)] #-}
-- {-# SPECIALIZE hsSourceDirsGrammar :: PrettyFieldGrammar BuildInfoAnn [SymbolicPath Pkg (Dir Source)] #-}

optionsFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , c (List NoCommaFSep Token' String)
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     )
  => g mod (BuildInfoWith mod) (PerCompilerFlavor [String])
optionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-options" (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-options" (alaList' NoCommaFSep Token') (extract GHCJS)
    -- NOTE: Hugs, NHC and JHC are not supported anymore, but these
    -- fields are kept around so that we can still parse legacy .cabal
    -- files that have them.
    <* knownField "jhc-options"
    <* knownField "hugs-options"
    <* knownField "nhc98-options"
  where
    extract flavor = L.options @mod . lookupLens flavor

-- {-# SPECIALIZE optionsFieldGrammar :: ParsecFieldGrammar BuildInfoAnn (PerCompilerFlavor [String]) #-}
-- {-# SPECIALIZE optionsFieldGrammar :: PrettyFieldGrammar BuildInfoAnn (PerCompilerFlavor [String]) #-}

profOptionsFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , c (List NoCommaFSep Token' String)
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     )
  => g mod (BuildInfoWith mod) (PerCompilerFlavor [String])
profOptionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-prof-options" (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-prof-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract flavor = L.profOptions @mod . lookupLens flavor

-- {-# SPECIALIZE profOptionsFieldGrammar :: ParsecFieldGrammar BuildInfoAnn (PerCompilerFlavor [String]) #-}
-- {-# SPECIALIZE profOptionsFieldGrammar :: PrettyFieldGrammar BuildInfoAnn (PerCompilerFlavor [String]) #-}

sharedOptionsFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , c (List NoCommaFSep Token' String)
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     )
  => g mod (BuildInfoWith mod) (PerCompilerFlavor [String])
sharedOptionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-shared-options" (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-shared-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract flavor = L.sharedOptions @mod . lookupLens flavor

profSharedOptionsFieldGrammar
  :: forall mod c g
   . ( FieldGrammarWith mod c g
     , c (List NoCommaFSep Token' String)
     , L.HasBuildInfoWith mod (BuildInfoWith mod)
     )
  => g mod (BuildInfoWith mod) (PerCompilerFlavor [String])
profSharedOptionsFieldGrammar =
  PerCompilerFlavor
    <$> monoidalFieldAla "ghc-prof-shared-options" (alaList' NoCommaFSep Token') (extract GHC)
      ^^^ availableSince CabalSpecV3_14 []
    <*> monoidalFieldAla "ghcjs-prof-shared-options" (alaList' NoCommaFSep Token') (extract GHCJS)
      ^^^ availableSince CabalSpecV3_14 []
  where
    extract flavor = L.profSharedOptions @mod . lookupLens flavor

lookupLens :: (Functor f, Monoid v) => CompilerFlavor -> LensLike' f (PerCompilerFlavor v) v
lookupLens k f p@(PerCompilerFlavor ghc ghcjs)
  | k == GHC = (`PerCompilerFlavor` ghcjs) <$> f ghc
  | k == GHCJS = (ghc `PerCompilerFlavor`) <$> f ghcjs
  | otherwise = p <$ f mempty

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagFieldGrammar
  :: (FieldGrammarWith mod c g)
  => FlagName
  -> g mod PackageFlag PackageFlag
flagFieldGrammar name =
  MkPackageFlag name
    <$> freeTextFieldDef "description" L.flagDescription
    <*> booleanFieldDef "default" L.flagDefault True
    <*> booleanFieldDef "manual" L.flagManual False

-- {-# SPECIALIZE flagFieldGrammar :: FlagName -> ParsecFieldGrammar' PackageFlag #-}
-- {-# SPECIALIZE flagFieldGrammar :: FlagName -> PrettyFieldGrammar' PackageFlag #-}

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

sourceRepoFieldGrammar
  :: (FieldGrammarWith mod c g, c (Identity RepoType))
  => RepoKind
  -> g mod SourceRepo SourceRepo
sourceRepoFieldGrammar kind =
  SourceRepo kind
    <$> optionalField "type" L.repoType
    <*> freeTextField "location" L.repoLocation
    <*> optionalFieldAla "module" Token L.repoModule
    <*> optionalFieldAla "branch" Token L.repoBranch
    <*> optionalFieldAla "tag" Token L.repoTag
    <*> optionalFieldAla "subdir" FilePathNT L.repoSubdir

-- {-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> ParsecFieldGrammar' SourceRepo #-}
-- {-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> PrettyFieldGrammar' SourceRepo #-}

-------------------------------------------------------------------------------
-- SetupBuildInfo
-------------------------------------------------------------------------------

setupBInfoFieldGrammar
  :: (FieldGrammarWith mod c g, Functor (g mod SetupBuildInfo), c (List CommaVCat (Identity Dependency) Dependency))
  => Bool
  -> g mod SetupBuildInfo SetupBuildInfo
setupBInfoFieldGrammar def =
  flip SetupBuildInfo def
    <$> monoidalFieldAla "setup-depends" (alaList CommaVCat) L.setupDepends

-- {-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> ParsecFieldGrammar' SetupBuildInfo #-}
-- {-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> PrettyFieldGrammar' SetupBuildInfo #-}

-------------------------------------------------------------------------------
-- Define how field values should be formatted for 'pretty'.
-------------------------------------------------------------------------------

formatDependencyList :: [AttachPosition mod (Annotate mod (DependencyWith mod))] -> ListWith mod CommaVCat (Identity (DependencyWith mod)) (DependencyWith mod)
formatDependencyList = List

formatMixinList :: [Mixin] -> List CommaVCat (Identity Mixin) Mixin
formatMixinList = alaList CommaVCat

formatExtraSourceFiles :: [RelativePath Pkg File] -> List VCat (RelativePathNT Pkg File) (RelativePath Pkg File)
formatExtraSourceFiles = alaList' VCat RelativePathNT

formatExposedModules :: [ModuleName] -> List VCat (MQuoted ModuleName) ModuleName
formatExposedModules = alaList' VCat MQuoted

formatHsSourceDirs :: [SymbolicPath Pkg (Dir Source)] -> List FSep (SymbolicPathNT Pkg (Dir Source)) (SymbolicPath Pkg (Dir Source))
formatHsSourceDirs = alaList' FSep SymbolicPathNT

formatOtherExtensions :: [Extension] -> List FSep (MQuoted Extension) Extension
formatOtherExtensions = alaList' FSep MQuoted

formatOtherModules :: [AttachPosition mod (Annotate mod ModuleName)] -> ListWith mod VCat (MQuoted ModuleName) ModuleName
formatOtherModules = List

-------------------------------------------------------------------------------
-- newtypes
-------------------------------------------------------------------------------

-- | Newtype for data directory (absolute or relative).
--
-- Accepts empty file path, but issues a warning;
-- there are simply too many (~1200) package definition files
--
-- @
-- data-dir: ""
-- @
--
-- across Hackage to outrule them completely.
-- I suspect some of them are generated (e.g. formatted) by machine.
newtype CompatDataDir = CompatDataDir {getCompatDataDir :: SymbolicPath Pkg (Dir DataDir)}

instance Newtype (SymbolicPath Pkg (Dir DataDir)) CompatDataDir

instance Parsec CompatDataDir where
  parsec = do
    token <- parsecToken
    when (null token) $
      parsecWarning PWTEmptyFilePath "empty FilePath"
    return (CompatDataDir $ makeSymbolicPath token)

instance Pretty CompatDataDir where
  pretty = showToken . getSymbolicPath . getCompatDataDir

newtype CompatLicenseFile = CompatLicenseFile {getCompatLicenseFile :: [RelativePath Pkg File]}

instance Newtype [RelativePath Pkg File] CompatLicenseFile

-- TODO
instance Parsec CompatLicenseFile where
  parsec = emptyToken <|> CompatLicenseFile . unpack' (alaList FSep) <$> parsec
    where
      emptyToken = P.try $ do
        token <- parsecToken
        if null token
          then return (CompatLicenseFile [])
          else P.unexpected "non-empty-token"

instance Pretty CompatLicenseFile where
  pretty = pretty . pack' (alaList FSep) . getCompatLicenseFile

-------------------------------------------------------------------------------
-- vim syntax definitions
-------------------------------------------------------------------------------

-- | '_syntaxFieldNames' and '_syntaxExtensions'
-- are for generating VIM syntax file definitions.
_syntaxFieldNames :: IO ()
_syntaxFieldNames =
  sequence_
    [ BS8.putStrLn $ " \\ " <> n
    | n <-
        nub $
          sort $
            mconcat
              [ fieldGrammarKnownFieldList (packageDescriptionFieldGrammar @Abst)
              , fieldGrammarKnownFieldList $ (libraryFieldGrammar @Abst) LMainLibName
              , fieldGrammarKnownFieldList $ (executableFieldGrammar @Abst) "exe"
              , fieldGrammarKnownFieldList $ (foreignLibFieldGrammar @Abst) "flib"
              , fieldGrammarKnownFieldList (testSuiteFieldGrammar @Abst)
              , fieldGrammarKnownFieldList (benchmarkFieldGrammar @Abst)
              , fieldGrammarKnownFieldList $ (flagFieldGrammar @Abst) (error "flagname")
              , fieldGrammarKnownFieldList $ (sourceRepoFieldGrammar @Abst) (error "repokind")
              , fieldGrammarKnownFieldList $ (setupBInfoFieldGrammar @Abst) True
              ]
    ]

_syntaxExtensions :: IO ()
_syntaxExtensions =
  sequence_
    [ putStrLn $ "  \\ " <> e
    | e <-
        ["Safe", "Trustworthy", "Unsafe"]
          ++ es
          ++ map ("No" ++) es
    ]
  where
    es =
      nub $
        sort
          [ prettyShow e
          | e <- [minBound .. maxBound]
          , e `notElem` [Safe, Unsafe, Trustworthy]
          ]
