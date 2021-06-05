{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | 'GenericPackageDescription' Field descriptions
module Distribution.PackageDescription.FieldGrammar (
    -- * Package description
    packageDescriptionFieldGrammar,
    -- * Library
    libraryFieldGrammar,
    -- * Foreign library
    foreignLibFieldGrammar,
    -- * Executable
    executableFieldGrammar,
    -- * Test suite
    TestSuiteStanza (..),
    testSuiteFieldGrammar,
    validateTestSuite,
    unvalidateTestSuite,
    -- ** Lenses
    testStanzaTestType,
    testStanzaMainIs,
    testStanzaTestModule,
    testStanzaBuildInfo,
    -- * Benchmark
    BenchmarkStanza (..),
    benchmarkFieldGrammar,
    validateBenchmark,
    unvalidateBenchmark,
    -- * Field grammars
    formatDependencyList,
    formatExposedModules,
    formatExtraSourceFiles,
    formatHsSourceDirs,
    formatMixinList,
    formatOtherExtensions,
    formatOtherModules,
    -- ** Lenses
    benchmarkStanzaBenchmarkType,
    benchmarkStanzaMainIs,
    benchmarkStanzaBenchmarkModule,
    benchmarkStanzaBuildInfo,
    -- * Flag
    flagFieldGrammar,
    -- * Source repository
    sourceRepoFieldGrammar,
    -- * Setup build info
    setupBInfoFieldGrammar,
    -- * Component build info
    buildInfoFieldGrammar,
    ) where


import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Language.Haskell.Extension
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compat.Newtype     (Newtype, pack', unpack')
import Distribution.Compiler           (CompilerFlavor (..), PerCompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.ModuleName         (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec
import Distribution.Pretty             (Pretty (..), prettyShow, showToken)
import Distribution.Utils.Path
import Distribution.Version            (Version, VersionRange)

import qualified Data.ByteString.Char8           as BS8
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX               as SPDX
import qualified Distribution.Types.Lens         as L

-------------------------------------------------------------------------------
-- PackageDescription
-------------------------------------------------------------------------------

packageDescriptionFieldGrammar
    :: ( FieldGrammar c g, Applicative (g PackageDescription), Applicative (g PackageIdentifier)
       , c (Identity BuildType)
       , c (Identity PackageName)
       , c (Identity Version)
       , c (List FSep FilePathNT String)
       , c (List FSep CompatFilePath String)
       , c (List FSep (Identity (SymbolicPath PackageDir LicenseFile)) (SymbolicPath PackageDir LicenseFile))
       , c (List FSep TestedWith (CompilerFlavor, VersionRange))
       , c (List VCat FilePathNT String)
       , c FilePathNT
       , c CompatLicenseFile
       , c CompatFilePath
       , c SpecLicense
       , c SpecVersion
       )
    => g PackageDescription PackageDescription
packageDescriptionFieldGrammar = PackageDescription
    <$> optionalFieldDefAla "cabal-version" SpecVersion                L.specVersion CabalSpecV1_0
    <*> blurFieldGrammar L.package packageIdentifierGrammar
    <*> optionalFieldDefAla "license"       SpecLicense                L.licenseRaw (Left SPDX.NONE)
    <*> licenseFilesGrammar
    <*> freeTextFieldDefST  "copyright"                                L.copyright
    <*> freeTextFieldDefST  "maintainer"                               L.maintainer
    <*> freeTextFieldDefST  "author"                                   L.author
    <*> freeTextFieldDefST  "stability"                                L.stability
    <*> monoidalFieldAla    "tested-with"   (alaList' FSep TestedWith) L.testedWith
    <*> freeTextFieldDefST  "homepage"                                 L.homepage
    <*> freeTextFieldDefST  "package-url"                              L.pkgUrl
    <*> freeTextFieldDefST   "bug-reports"                              L.bugReports
    <*> pure [] -- source-repos are stanza
    <*> freeTextFieldDefST  "synopsis"                                 L.synopsis
    <*> freeTextFieldDefST  "description"                              L.description
    <*> freeTextFieldDefST  "category"                                 L.category
    <*> prefixedFields      "x-"                                       L.customFieldsPD
    <*> optionalField       "build-type"                               L.buildTypeRaw
    <*> pure Nothing -- custom-setup
    -- components
    <*> pure Nothing  -- lib
    <*> pure []       -- sub libs
    <*> pure []       -- executables
    <*> pure []       -- foreign libs
    <*> pure []       -- test suites
    <*> pure []       -- benchmarks
    --  * Files
    <*> monoidalFieldAla    "data-files"         (alaList' VCat FilePathNT) L.dataFiles
    <*> optionalFieldDefAla "data-dir"           CompatFilePath             L.dataDir "."
        ^^^ fmap (\x -> if null x then "." else x) -- map empty directories to "."
    <*> monoidalFieldAla    "extra-source-files" formatExtraSourceFiles     L.extraSrcFiles
    <*> monoidalFieldAla    "extra-tmp-files"    (alaList' VCat FilePathNT) L.extraTmpFiles
    <*> monoidalFieldAla    "extra-doc-files"    (alaList' VCat FilePathNT) L.extraDocFiles
  where
    packageIdentifierGrammar = PackageIdentifier
        <$> uniqueField "name"    L.pkgName
        <*> uniqueField "version" L.pkgVersion

    licenseFilesGrammar = (++)
        -- TODO: neither field is deprecated
        -- should we pretty print license-file if there's single license file
        -- and license-files when more
        <$> monoidalFieldAla    "license-file"   CompatLicenseFile L.licenseFiles
        <*> monoidalFieldAla    "license-files"  (alaList FSep)    L.licenseFiles
            ^^^ hiddenField

-------------------------------------------------------------------------------
-- Library
-------------------------------------------------------------------------------

libraryFieldGrammar
    :: ( FieldGrammar c g, Applicative (g Library), Applicative (g BuildInfo)
       , c (Identity LibraryVisibility)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List CommaVCat (Identity ModuleReexport) ModuleReexport)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep FilePathNT String)
       , c (List FSep Token String)
       , c (List NoCommaFSep Token' String)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat FilePathNT String)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       , c (List VCat Token String)
       , c (MQuoted Language)
       )
    => LibraryName
    -> g Library Library
libraryFieldGrammar n = Library n
    <$> monoidalFieldAla  "exposed-modules"    formatExposedModules    L.exposedModules
    <*> monoidalFieldAla  "reexported-modules" (alaList  CommaVCat)    L.reexportedModules
    <*> monoidalFieldAla  "signatures"         (alaList' VCat MQuoted) L.signatures
        ^^^ availableSince CabalSpecV2_0 []
    <*> booleanFieldDef   "exposed"                                    L.libExposed True
    <*> visibilityField
    <*> blurFieldGrammar L.libBuildInfo buildInfoFieldGrammar
  where
    visibilityField = case n of
        -- nameless/"main" libraries are public
        LMainLibName -> pure LibraryVisibilityPublic
        -- named libraries have the field
        LSubLibName _ ->
            optionalFieldDef "visibility" L.libVisibility LibraryVisibilityPrivate
            ^^^ availableSince CabalSpecV3_0 LibraryVisibilityPrivate

{-# SPECIALIZE libraryFieldGrammar :: LibraryName -> ParsecFieldGrammar' Library #-}
{-# SPECIALIZE libraryFieldGrammar :: LibraryName -> PrettyFieldGrammar' Library #-}

-------------------------------------------------------------------------------
-- Foreign library
-------------------------------------------------------------------------------

foreignLibFieldGrammar
    :: ( FieldGrammar c g, Applicative (g ForeignLib), Applicative (g BuildInfo)
       , c (Identity ForeignLibType)
       , c (Identity LibVersionInfo)
       , c (Identity Version)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (Identity ForeignLibOption) ForeignLibOption)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep FilePathNT String)
       , c (List FSep Token String)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       , c (List NoCommaFSep Token' String)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat FilePathNT String), c (List VCat Token String)
       , c (MQuoted Language)
       )
    => UnqualComponentName -> g ForeignLib ForeignLib
foreignLibFieldGrammar n = ForeignLib n
    <$> optionalFieldDef "type"                                         L.foreignLibType ForeignLibTypeUnknown
    <*> monoidalFieldAla "options"           (alaList FSep)             L.foreignLibOptions
    <*> blurFieldGrammar L.foreignLibBuildInfo buildInfoFieldGrammar
    <*> optionalField    "lib-version-info"                             L.foreignLibVersionInfo
    <*> optionalField    "lib-version-linux"                            L.foreignLibVersionLinux
    <*> monoidalFieldAla "mod-def-file"      (alaList' FSep FilePathNT) L.foreignLibModDefFile
{-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' ForeignLib #-}
{-# SPECIALIZE foreignLibFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' ForeignLib #-}

-------------------------------------------------------------------------------
-- Executable
-------------------------------------------------------------------------------

executableFieldGrammar
    :: ( FieldGrammar c g, Applicative (g Executable), Applicative (g BuildInfo)
       , c (Identity ExecutableScope)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep FilePathNT String)
       , c (List FSep Token String)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       , c (List NoCommaFSep Token' String)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat FilePathNT String)
       , c (List VCat Token String)
       , c (MQuoted Language)
       )
    => UnqualComponentName -> g Executable Executable
executableFieldGrammar n = Executable n
    -- main-is is optional as conditional blocks don't have it
    <$> optionalFieldDefAla "main-is" FilePathNT L.modulePath ""
    <*> optionalFieldDef    "scope"              L.exeScope ExecutablePublic
        ^^^ availableSince CabalSpecV2_0 ExecutablePublic
    <*> blurFieldGrammar L.buildInfo buildInfoFieldGrammar
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> ParsecFieldGrammar' Executable #-}
{-# SPECIALIZE executableFieldGrammar :: UnqualComponentName -> PrettyFieldGrammar' Executable #-}

-------------------------------------------------------------------------------
-- TestSuite
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza
    { _testStanzaTestType   :: Maybe TestType
    , _testStanzaMainIs     :: Maybe FilePath
    , _testStanzaTestModule :: Maybe ModuleName
    , _testStanzaBuildInfo  :: BuildInfo
    }

instance L.HasBuildInfo TestSuiteStanza where
    buildInfo = testStanzaBuildInfo

testStanzaTestType :: Lens' TestSuiteStanza (Maybe TestType)
testStanzaTestType f s = fmap (\x -> s { _testStanzaTestType = x }) (f (_testStanzaTestType s))
{-# INLINE testStanzaTestType #-}

testStanzaMainIs :: Lens' TestSuiteStanza (Maybe FilePath)
testStanzaMainIs f s = fmap (\x -> s { _testStanzaMainIs = x }) (f (_testStanzaMainIs s))
{-# INLINE testStanzaMainIs #-}

testStanzaTestModule :: Lens' TestSuiteStanza (Maybe ModuleName)
testStanzaTestModule f s = fmap (\x -> s { _testStanzaTestModule = x }) (f (_testStanzaTestModule s))
{-# INLINE testStanzaTestModule #-}

testStanzaBuildInfo :: Lens' TestSuiteStanza BuildInfo
testStanzaBuildInfo f s = fmap (\x -> s { _testStanzaBuildInfo = x }) (f (_testStanzaBuildInfo s))
{-# INLINE testStanzaBuildInfo #-}

testSuiteFieldGrammar
    :: ( FieldGrammar c g, Applicative (g TestSuiteStanza), Applicative (g BuildInfo)
       , c (Identity ModuleName)
       , c (Identity TestType)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep FilePathNT String)
       , c (List FSep Token String)
       , c (List NoCommaFSep Token' String)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat FilePathNT String)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       , c (List VCat Token String)
       , c (MQuoted Language)
       )
    => g TestSuiteStanza TestSuiteStanza
testSuiteFieldGrammar = TestSuiteStanza
    <$> optionalField    "type"                   testStanzaTestType
    <*> optionalFieldAla "main-is"     FilePathNT testStanzaMainIs
    <*> optionalField    "test-module"            testStanzaTestModule
    <*> blurFieldGrammar testStanzaBuildInfo buildInfoFieldGrammar

validateTestSuite :: Position -> TestSuiteStanza -> ParseResult TestSuite
validateTestSuite pos stanza = case _testStanzaTestType stanza of
    Nothing -> return $
        emptyTestSuite { testBuildInfo = _testStanzaBuildInfo stanza }

    Just tt@(TestTypeUnknown _ _) ->
        pure emptyTestSuite
            { testInterface = TestSuiteUnsupported tt
            , testBuildInfo = _testStanzaBuildInfo stanza
            }

    Just tt | tt `notElem` knownTestTypes ->
        pure emptyTestSuite
            { testInterface = TestSuiteUnsupported tt
            , testBuildInfo = _testStanzaBuildInfo stanza
            }

    Just tt@(TestTypeExe ver) -> case _testStanzaMainIs stanza of
        Nothing   -> do
            parseFailure pos (missingField "main-is" tt)
            pure emptyTestSuite
        Just file -> do
            when (isJust (_testStanzaTestModule stanza)) $
                parseWarning pos PWTExtraBenchmarkModule (extraField "test-module" tt)
            pure emptyTestSuite
                { testInterface = TestSuiteExeV10 ver file
                , testBuildInfo = _testStanzaBuildInfo stanza
                }

    Just tt@(TestTypeLib ver) -> case _testStanzaTestModule stanza of
         Nothing      -> do
             parseFailure pos (missingField "test-module" tt)
             pure emptyTestSuite
         Just module_ -> do
            when (isJust (_testStanzaMainIs stanza)) $
                parseWarning pos PWTExtraMainIs (extraField "main-is" tt)
            pure emptyTestSuite
                { testInterface = TestSuiteLibV09 ver module_
                , testBuildInfo = _testStanzaBuildInfo stanza
                }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ prettyShow tt ++ " test suite type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ prettyShow tt ++ "' test suite type."

unvalidateTestSuite :: TestSuite -> TestSuiteStanza
unvalidateTestSuite t = TestSuiteStanza
    { _testStanzaTestType   = ty
    , _testStanzaMainIs     = ma
    , _testStanzaTestModule = mo
    , _testStanzaBuildInfo  = testBuildInfo t
    }
  where
    (ty, ma, mo) = case testInterface t of
        TestSuiteExeV10 ver file -> (Just $ TestTypeExe ver, Just file, Nothing)
        TestSuiteLibV09 ver modu -> (Just $ TestTypeLib ver, Nothing, Just modu)
        _                        -> (Nothing, Nothing, Nothing)

-------------------------------------------------------------------------------
-- Benchmark
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza
    { _benchmarkStanzaBenchmarkType   :: Maybe BenchmarkType
    , _benchmarkStanzaMainIs          :: Maybe FilePath
    , _benchmarkStanzaBenchmarkModule :: Maybe ModuleName
    , _benchmarkStanzaBuildInfo       :: BuildInfo
    }

instance L.HasBuildInfo BenchmarkStanza where
    buildInfo = benchmarkStanzaBuildInfo

benchmarkStanzaBenchmarkType :: Lens' BenchmarkStanza (Maybe BenchmarkType)
benchmarkStanzaBenchmarkType f s = fmap (\x -> s { _benchmarkStanzaBenchmarkType = x }) (f (_benchmarkStanzaBenchmarkType s))
{-# INLINE benchmarkStanzaBenchmarkType #-}

benchmarkStanzaMainIs :: Lens' BenchmarkStanza (Maybe FilePath)
benchmarkStanzaMainIs f s = fmap (\x -> s { _benchmarkStanzaMainIs = x }) (f (_benchmarkStanzaMainIs s))
{-# INLINE benchmarkStanzaMainIs #-}

benchmarkStanzaBenchmarkModule :: Lens' BenchmarkStanza (Maybe ModuleName)
benchmarkStanzaBenchmarkModule f s = fmap (\x -> s { _benchmarkStanzaBenchmarkModule = x }) (f (_benchmarkStanzaBenchmarkModule s))
{-# INLINE benchmarkStanzaBenchmarkModule #-}

benchmarkStanzaBuildInfo :: Lens' BenchmarkStanza BuildInfo
benchmarkStanzaBuildInfo f s = fmap (\x -> s { _benchmarkStanzaBuildInfo = x }) (f (_benchmarkStanzaBuildInfo s))
{-# INLINE benchmarkStanzaBuildInfo #-}

benchmarkFieldGrammar
    :: ( FieldGrammar c g, Applicative (g BenchmarkStanza), Applicative (g BuildInfo)
       , c (Identity BenchmarkType)
       , c (Identity ModuleName)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep FilePathNT String)
       , c (List FSep Token String)
       , c (List NoCommaFSep Token' String)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat FilePathNT String)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       , c (List VCat Token String)
       , c (MQuoted Language)
       )
    => g BenchmarkStanza BenchmarkStanza
benchmarkFieldGrammar = BenchmarkStanza
    <$> optionalField    "type"                        benchmarkStanzaBenchmarkType
    <*> optionalFieldAla "main-is"          FilePathNT benchmarkStanzaMainIs
    <*> optionalField    "benchmark-module"            benchmarkStanzaBenchmarkModule
    <*> blurFieldGrammar benchmarkStanzaBuildInfo buildInfoFieldGrammar

validateBenchmark :: Position -> BenchmarkStanza -> ParseResult Benchmark
validateBenchmark pos stanza = case _benchmarkStanzaBenchmarkType stanza of
    Nothing -> pure emptyBenchmark
        { benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza }

    Just tt@(BenchmarkTypeUnknown _ _) -> pure emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }

    Just tt | tt `notElem` knownBenchmarkTypes -> pure emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }

    Just tt@(BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
        Nothing   -> do
            parseFailure pos (missingField "main-is" tt)
            pure emptyBenchmark
        Just file -> do
            when (isJust (_benchmarkStanzaBenchmarkModule stanza)) $
                parseWarning pos PWTExtraBenchmarkModule (extraField "benchmark-module" tt)
            pure emptyBenchmark
                { benchmarkInterface = BenchmarkExeV10 ver file
                , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
                }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ prettyShow tt ++ " benchmark type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ prettyShow tt ++ "' benchmark type."

unvalidateBenchmark :: Benchmark -> BenchmarkStanza
unvalidateBenchmark b = BenchmarkStanza
    { _benchmarkStanzaBenchmarkType   = ty
    , _benchmarkStanzaMainIs          = ma
    , _benchmarkStanzaBenchmarkModule = mo
    , _benchmarkStanzaBuildInfo       = benchmarkBuildInfo b
    }
  where
    (ty, ma, mo) = case benchmarkInterface b of
        BenchmarkExeV10 ver ""  -> (Just $ BenchmarkTypeExe ver, Nothing,  Nothing)
        BenchmarkExeV10 ver ma' -> (Just $ BenchmarkTypeExe ver, Just ma', Nothing)
        _                       -> (Nothing, Nothing,  Nothing)

-------------------------------------------------------------------------------
-- Build info
-------------------------------------------------------------------------------

buildInfoFieldGrammar
    :: ( FieldGrammar c g, Applicative (g BuildInfo)
       , c (List CommaFSep (Identity ExeDependency) ExeDependency)
       , c (List CommaFSep (Identity LegacyExeDependency) LegacyExeDependency)
       , c (List CommaFSep (Identity PkgconfigDependency) PkgconfigDependency)
       , c (List CommaVCat (Identity Dependency) Dependency)
       , c (List CommaVCat (Identity Mixin) Mixin)
       , c (List FSep (MQuoted Extension) Extension)
       , c (List FSep (MQuoted Language) Language)
       , c (List FSep FilePathNT String)
       , c (List FSep Token String)
       , c (List NoCommaFSep Token' String)
       , c (List VCat (MQuoted ModuleName) ModuleName)
       , c (List VCat FilePathNT String)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       , c (List VCat Token String)
       , c (MQuoted Language)
       )
    => g BuildInfo BuildInfo
buildInfoFieldGrammar = BuildInfo
    <$> booleanFieldDef  "buildable"                                          L.buildable True
    <*> monoidalFieldAla "build-tools"          (alaList  CommaFSep)          L.buildTools
        ^^^ deprecatedSince CabalSpecV2_0
            "Please use 'build-tool-depends' field"
        ^^^ removedIn CabalSpecV3_0
            "Please use 'build-tool-depends' field."
    <*> monoidalFieldAla "build-tool-depends"   (alaList  CommaFSep)          L.buildToolDepends
        -- {- ^^^ availableSince [2,0] [] -}
        -- here, we explicitly want to recognise build-tool-depends for all Cabal files
        -- as otherwise cabal new-build cannot really work.
        --
        -- I.e. we don't want trigger unknown field warning
    <*> monoidalFieldAla "cpp-options"          (alaList' NoCommaFSep Token') L.cppOptions
    <*> monoidalFieldAla "asm-options"          (alaList' NoCommaFSep Token') L.asmOptions
        ^^^ availableSince CabalSpecV3_0 []
    <*> monoidalFieldAla "cmm-options"          (alaList' NoCommaFSep Token') L.cmmOptions
        ^^^ availableSince CabalSpecV3_0 []
    <*> monoidalFieldAla "cc-options"           (alaList' NoCommaFSep Token') L.ccOptions
    <*> monoidalFieldAla "cxx-options"          (alaList' NoCommaFSep Token') L.cxxOptions
        ^^^ availableSince CabalSpecV2_2 []
    <*> monoidalFieldAla "ld-options"           (alaList' NoCommaFSep Token') L.ldOptions
    <*> monoidalFieldAla "hsc2hs-options"       (alaList' NoCommaFSep Token') L.hsc2hsOptions
        ^^^ availableSince CabalSpecV3_6 []
    <*> monoidalFieldAla "pkgconfig-depends"    (alaList  CommaFSep)          L.pkgconfigDepends
    <*> monoidalFieldAla "frameworks"           (alaList' FSep Token)         L.frameworks
    <*> monoidalFieldAla "extra-framework-dirs" (alaList' FSep FilePathNT)    L.extraFrameworkDirs
    <*> monoidalFieldAla "asm-sources"          (alaList' VCat FilePathNT)    L.asmSources
        ^^^ availableSince CabalSpecV3_0 []
    <*> monoidalFieldAla "cmm-sources"          (alaList' VCat FilePathNT)    L.cmmSources
        ^^^ availableSince CabalSpecV3_0 []
    <*> monoidalFieldAla "c-sources"            (alaList' VCat FilePathNT)    L.cSources
    <*> monoidalFieldAla "cxx-sources"          (alaList' VCat FilePathNT)    L.cxxSources
        ^^^ availableSince CabalSpecV2_2 []
    <*> monoidalFieldAla "js-sources"           (alaList' VCat FilePathNT)    L.jsSources
    <*> hsSourceDirsGrammar
    <*> monoidalFieldAla "other-modules"        formatOtherModules            L.otherModules
    <*> monoidalFieldAla "virtual-modules"      (alaList' VCat MQuoted)       L.virtualModules
        ^^^ availableSince CabalSpecV2_2 []
    <*> monoidalFieldAla "autogen-modules"      (alaList' VCat MQuoted)       L.autogenModules
        ^^^ availableSince CabalSpecV2_0 []
    <*> optionalFieldAla "default-language"     MQuoted                       L.defaultLanguage
        ^^^ availableSince CabalSpecV1_10 Nothing
    <*> monoidalFieldAla "other-languages"      (alaList' FSep MQuoted)       L.otherLanguages
        ^^^ availableSince CabalSpecV1_10 []
    <*> monoidalFieldAla "default-extensions"   (alaList' FSep MQuoted)       L.defaultExtensions
        ^^^ availableSince CabalSpecV1_10 []
    <*> monoidalFieldAla "other-extensions"     formatOtherExtensions         L.otherExtensions
        ^^^ availableSinceWarn CabalSpecV1_10
    <*> monoidalFieldAla "extensions"           (alaList' FSep MQuoted)       L.oldExtensions
        ^^^ deprecatedSince CabalSpecV1_12
            "Please use 'default-extensions' or 'other-extensions' fields."
        ^^^ removedIn CabalSpecV3_0
            "Please use 'default-extensions' or 'other-extensions' fields."
    <*> monoidalFieldAla "extra-libraries"      (alaList' VCat Token)         L.extraLibs
    <*> monoidalFieldAla "extra-ghci-libraries" (alaList' VCat Token)         L.extraGHCiLibs
    <*> monoidalFieldAla "extra-bundled-libraries" (alaList' VCat Token)      L.extraBundledLibs
    <*> monoidalFieldAla "extra-library-flavours" (alaList' VCat Token)       L.extraLibFlavours
    <*> monoidalFieldAla "extra-dynamic-library-flavours" (alaList' VCat Token) L.extraDynLibFlavours
        ^^^ availableSince CabalSpecV3_0 []
    <*> monoidalFieldAla "extra-lib-dirs"       (alaList' FSep FilePathNT)    L.extraLibDirs
    <*> monoidalFieldAla "include-dirs"         (alaList' FSep FilePathNT)    L.includeDirs
    <*> monoidalFieldAla "includes"             (alaList' FSep FilePathNT)    L.includes
    <*> monoidalFieldAla "autogen-includes"     (alaList' FSep FilePathNT)    L.autogenIncludes
        ^^^ availableSince CabalSpecV3_0 []
    <*> monoidalFieldAla "install-includes"     (alaList' FSep FilePathNT)    L.installIncludes
    <*> optionsFieldGrammar
    <*> profOptionsFieldGrammar
    <*> sharedOptionsFieldGrammar
    <*> pure mempty -- static-options ???
    <*> prefixedFields   "x-"                                                 L.customFieldsBI
    <*> monoidalFieldAla "build-depends"        formatDependencyList          L.targetBuildDepends
    <*> monoidalFieldAla "mixins"               formatMixinList               L.mixins
        ^^^ availableSince CabalSpecV2_0 []
{-# SPECIALIZE buildInfoFieldGrammar :: ParsecFieldGrammar' BuildInfo #-}
{-# SPECIALIZE buildInfoFieldGrammar :: PrettyFieldGrammar' BuildInfo #-}

hsSourceDirsGrammar
    :: ( FieldGrammar c g, Applicative (g BuildInfo)
       , c (List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir))
       )
    => g BuildInfo [SymbolicPath PackageDir SourceDir]
hsSourceDirsGrammar = (++)
    <$> monoidalFieldAla "hs-source-dirs" formatHsSourceDirs L.hsSourceDirs
    <*> monoidalFieldAla "hs-source-dir"  (alaList FSep) wrongLens
        --- https://github.com/haskell/cabal/commit/49e3cdae3bdf21b017ccd42e66670ca402e22b44
        ^^^ deprecatedSince CabalSpecV1_2 "Please use 'hs-source-dirs'"
        ^^^ removedIn CabalSpecV3_0 "Please use 'hs-source-dirs' field."
  where
    -- TODO: make pretty printer aware of CabalSpecVersion
    wrongLens :: Functor f => LensLike' f BuildInfo [SymbolicPath PackageDir SourceDir]
    wrongLens f bi = (\fps -> set L.hsSourceDirs fps bi) <$> f []

optionsFieldGrammar
    :: (FieldGrammar c g, Applicative (g BuildInfo), c (List NoCommaFSep Token' String))
    => g BuildInfo (PerCompilerFlavor [String])
optionsFieldGrammar = PerCompilerFlavor
    <$> monoidalFieldAla "ghc-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-options" (alaList' NoCommaFSep Token') (extract GHCJS)
    -- NOTE: Hugs, NHC and JHC are not supported anymore, but these
    -- fields are kept around so that we can still parse legacy .cabal
    -- files that have them.
    <*  knownField "jhc-options"
    <*  knownField "hugs-options"
    <*  knownField "nhc98-options"
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.options . lookupLens flavor

profOptionsFieldGrammar
    :: (FieldGrammar c g, Applicative (g BuildInfo), c (List NoCommaFSep Token' String))
    => g BuildInfo (PerCompilerFlavor [String])
profOptionsFieldGrammar = PerCompilerFlavor
    <$> monoidalFieldAla "ghc-prof-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-prof-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.profOptions . lookupLens flavor

sharedOptionsFieldGrammar
    :: (FieldGrammar c g, Applicative (g BuildInfo), c (List NoCommaFSep Token' String))
    => g BuildInfo (PerCompilerFlavor [String])
sharedOptionsFieldGrammar = PerCompilerFlavor
    <$> monoidalFieldAla "ghc-shared-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-shared-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.sharedOptions . lookupLens flavor

lookupLens :: (Functor f, Monoid v) => CompilerFlavor -> LensLike' f (PerCompilerFlavor v) v
lookupLens k f p@(PerCompilerFlavor ghc ghcjs)
    | k == GHC   = (\n -> PerCompilerFlavor n ghcjs) <$> f ghc
    | k == GHCJS = (\n -> PerCompilerFlavor ghc n) <$> f ghcjs
    | otherwise  = p <$ f mempty

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagFieldGrammar
    :: (FieldGrammar c g, Applicative (g PackageFlag))
    =>  FlagName -> g PackageFlag PackageFlag
flagFieldGrammar name = MkPackageFlag name
    <$> freeTextFieldDef    "description"          L.flagDescription
    <*> booleanFieldDef     "default"              L.flagDefault     True
    <*> booleanFieldDef     "manual"               L.flagManual      False
{-# SPECIALIZE flagFieldGrammar :: FlagName -> ParsecFieldGrammar' PackageFlag #-}
{-# SPECIALIZE flagFieldGrammar :: FlagName -> PrettyFieldGrammar' PackageFlag #-}

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

sourceRepoFieldGrammar
    :: (FieldGrammar c g, Applicative (g SourceRepo), c (Identity RepoType), c Token, c FilePathNT)
    => RepoKind -> g SourceRepo SourceRepo
sourceRepoFieldGrammar kind = SourceRepo kind
    <$> optionalField    "type"                L.repoType
    <*> freeTextField    "location"            L.repoLocation
    <*> optionalFieldAla "module"   Token      L.repoModule
    <*> optionalFieldAla "branch"   Token      L.repoBranch
    <*> optionalFieldAla "tag"      Token      L.repoTag
    <*> optionalFieldAla "subdir"   FilePathNT L.repoSubdir
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> ParsecFieldGrammar' SourceRepo #-}
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> PrettyFieldGrammar' SourceRepo #-}

-------------------------------------------------------------------------------
-- SetupBuildInfo
-------------------------------------------------------------------------------

setupBInfoFieldGrammar
    :: (FieldGrammar c g, Functor (g SetupBuildInfo), c (List CommaVCat (Identity Dependency) Dependency))
    => Bool -> g SetupBuildInfo SetupBuildInfo
setupBInfoFieldGrammar def = flip SetupBuildInfo def
    <$> monoidalFieldAla "setup-depends" (alaList CommaVCat) L.setupDepends
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> ParsecFieldGrammar' SetupBuildInfo #-}
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> PrettyFieldGrammar' SetupBuildInfo #-}

-------------------------------------------------------------------------------
-- Define how field values should be formatted for 'pretty'.
-------------------------------------------------------------------------------

formatDependencyList :: [Dependency] -> List CommaVCat (Identity Dependency) Dependency
formatDependencyList = alaList CommaVCat

formatMixinList :: [Mixin] -> List CommaVCat (Identity Mixin) Mixin
formatMixinList = alaList CommaVCat

formatExtraSourceFiles :: [FilePath] -> List VCat FilePathNT FilePath
formatExtraSourceFiles = alaList' VCat FilePathNT

formatExposedModules :: [ModuleName] -> List VCat (MQuoted ModuleName) ModuleName
formatExposedModules = alaList' VCat MQuoted

formatHsSourceDirs :: [SymbolicPath PackageDir SourceDir] -> List FSep (Identity (SymbolicPath PackageDir SourceDir)) (SymbolicPath PackageDir SourceDir)
formatHsSourceDirs = alaList FSep

formatOtherExtensions :: [Extension] -> List FSep (MQuoted Extension) Extension
formatOtherExtensions = alaList' FSep MQuoted

formatOtherModules :: [ModuleName] -> List VCat (MQuoted ModuleName) ModuleName
formatOtherModules = alaList' VCat MQuoted

-------------------------------------------------------------------------------
-- newtypes
-------------------------------------------------------------------------------

-- | Compat FilePath accepts empty file path,
-- but issues a warning.
--
-- There are simply too many (~1200) package definition files
--
-- @
-- license-file: ""
-- @
--
-- and
--
-- @
-- data-dir: ""
-- @
--
-- across Hackage to outrule them completely.
-- I suspect some of them are generated (e.g. formatted) by machine.
--
newtype CompatFilePath = CompatFilePath { getCompatFilePath :: FilePath } -- TODO: Change to use SymPath

instance Newtype String CompatFilePath

instance Parsec CompatFilePath where
    parsec = do
        token <- parsecToken
        if null token
        then do
            parsecWarning PWTEmptyFilePath "empty FilePath"
            return (CompatFilePath "")
        else return (CompatFilePath token)

instance Pretty CompatFilePath where
    pretty = showToken . getCompatFilePath

newtype CompatLicenseFile = CompatLicenseFile { getCompatLicenseFile :: [SymbolicPath PackageDir LicenseFile] }

instance Newtype [SymbolicPath PackageDir LicenseFile] CompatLicenseFile

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
--
_syntaxFieldNames :: IO ()
_syntaxFieldNames = sequence_
    [ BS8.putStrLn $ " \\ " <> n
    | n <- nub $ sort $ mconcat
        [ fieldGrammarKnownFieldList packageDescriptionFieldGrammar
        , fieldGrammarKnownFieldList $ libraryFieldGrammar LMainLibName
        , fieldGrammarKnownFieldList $ executableFieldGrammar "exe"
        , fieldGrammarKnownFieldList $ foreignLibFieldGrammar "flib"
        , fieldGrammarKnownFieldList testSuiteFieldGrammar
        , fieldGrammarKnownFieldList benchmarkFieldGrammar
        , fieldGrammarKnownFieldList $ flagFieldGrammar (error "flagname")
        , fieldGrammarKnownFieldList $ sourceRepoFieldGrammar (error "repokind")
        , fieldGrammarKnownFieldList $ setupBInfoFieldGrammar True
        ]
    ]

_syntaxExtensions :: IO ()
_syntaxExtensions = sequence_
    [ putStrLn $ "  \\ " <> e
    | e <- ["Safe","Trustworthy","Unsafe"]
        ++ es
        ++ map ("No"++) es
    ]
  where
    es = nub $ sort
          [ prettyShow e
          | e <- [ minBound .. maxBound ]
          , e `notElem` [Safe,Unsafe,Trustworthy]
          ]
