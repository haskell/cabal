{-# LANGUAGE OverloadedStrings #-}
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
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compiler                  (CompilerFlavor (..), PerCompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.ModuleName                (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec
import Distribution.Parsec.Newtypes
import Distribution.Fields
import Distribution.Pretty                    (prettyShow)
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.LibraryVisibility
import Distribution.Types.UnqualComponentName
import Distribution.Version                   (anyVersion)

import qualified Distribution.SPDX as SPDX

import qualified Distribution.Types.Lens as L

-------------------------------------------------------------------------------
-- PackageDescription
-------------------------------------------------------------------------------

packageDescriptionFieldGrammar
    :: (FieldGrammar g, Applicative (g PackageDescription), Applicative (g PackageIdentifier))
    => g PackageDescription PackageDescription
packageDescriptionFieldGrammar = PackageDescription
    <$> optionalFieldDefAla "cabal-version" SpecVersion                L.specVersionRaw (Right anyVersion)
    <*> blurFieldGrammar L.package packageIdentifierGrammar
    <*> optionalFieldDefAla "license"       SpecLicense                L.licenseRaw (Left SPDX.NONE)
    <*> licenseFilesGrammar
    <*> freeTextFieldDef    "copyright"                                L.copyright
    <*> freeTextFieldDef    "maintainer"                               L.maintainer
    <*> freeTextFieldDef    "author"                                   L.author
    <*> freeTextFieldDef    "stability"                                L.stability
    <*> monoidalFieldAla    "tested-with"   (alaList' FSep TestedWith) L.testedWith
    <*> freeTextFieldDef    "homepage"                                 L.homepage
    <*> freeTextFieldDef    "package-url"                              L.pkgUrl
    <*> freeTextFieldDef    "bug-reports"                              L.bugReports
    <*> pure [] -- source-repos are stanza
    <*> freeTextFieldDef    "synopsis"                                 L.synopsis
    <*> freeTextFieldDef    "description"                              L.description
    <*> freeTextFieldDef    "category"                                 L.category
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
    <*> optionalFieldDefAla "data-dir"           FilePathNT                 L.dataDir ""
    <*> monoidalFieldAla    "extra-source-files" (alaList' VCat FilePathNT) L.extraSrcFiles
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
        <$> monoidalFieldAla    "license-file"  (alaList' FSep FilePathNT)  L.licenseFiles
        <*> monoidalFieldAla    "license-files"  (alaList' FSep FilePathNT) L.licenseFiles
            ^^^ hiddenField

-------------------------------------------------------------------------------
-- Library
-------------------------------------------------------------------------------

libraryFieldGrammar
    :: (FieldGrammar g, Applicative (g Library), Applicative (g BuildInfo))
    => LibraryName
    -> g Library Library
libraryFieldGrammar n = Library n
    <$> monoidalFieldAla  "exposed-modules"    (alaList' VCat MQuoted) L.exposedModules
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
    :: (FieldGrammar g, Applicative (g ForeignLib), Applicative (g BuildInfo))
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
    :: (FieldGrammar g, Applicative (g Executable), Applicative (g BuildInfo))
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
    :: (FieldGrammar g, Applicative (g TestSuiteStanza), Applicative (g BuildInfo))
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
    :: (FieldGrammar g, Applicative (g BenchmarkStanza), Applicative (g BuildInfo))
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
    :: (FieldGrammar g, Applicative (g BuildInfo))
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
    <*> monoidalFieldAla "cmm-options"          (alaList' NoCommaFSep Token') L.cmmOptions
    <*> monoidalFieldAla "cc-options"           (alaList' NoCommaFSep Token') L.ccOptions
    <*> monoidalFieldAla "cxx-options"          (alaList' NoCommaFSep Token') L.cxxOptions
        ^^^ availableSince CabalSpecV2_2 []
    <*> monoidalFieldAla "ld-options"           (alaList' NoCommaFSep Token') L.ldOptions
    <*> monoidalFieldAla "pkgconfig-depends"    (alaList  CommaFSep)          L.pkgconfigDepends
    <*> monoidalFieldAla "frameworks"           (alaList' FSep Token)         L.frameworks
    <*> monoidalFieldAla "extra-framework-dirs" (alaList' FSep FilePathNT)    L.extraFrameworkDirs
    <*> monoidalFieldAla "asm-sources"          (alaList' VCat FilePathNT)    L.asmSources
    <*> monoidalFieldAla "cmm-sources"          (alaList' VCat FilePathNT)    L.cmmSources
    <*> monoidalFieldAla "c-sources"            (alaList' VCat FilePathNT)    L.cSources
    <*> monoidalFieldAla "cxx-sources"          (alaList' VCat FilePathNT)    L.cxxSources
        ^^^ availableSince CabalSpecV2_2 []
    <*> monoidalFieldAla "js-sources"           (alaList' VCat FilePathNT)    L.jsSources
    <*> hsSourceDirsGrammar
    <*> monoidalFieldAla "other-modules"        (alaList' VCat MQuoted)       L.otherModules
    <*> monoidalFieldAla "virtual-modules"      (alaList' VCat MQuoted)       L.virtualModules
        ^^^ availableSince CabalSpecV2_2 []
    <*> monoidalFieldAla "autogen-modules"      (alaList' VCat MQuoted)       L.autogenModules
    <*> optionalFieldAla "default-language"     MQuoted                       L.defaultLanguage
    <*> monoidalFieldAla "other-languages"      (alaList' FSep MQuoted)       L.otherLanguages
    <*> monoidalFieldAla "default-extensions"   (alaList' FSep MQuoted)       L.defaultExtensions
    <*> monoidalFieldAla "other-extensions"     (alaList' FSep MQuoted)       L.otherExtensions
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
    <*> monoidalFieldAla "build-depends"        (alaList  CommaVCat)          L.targetBuildDepends
    <*> monoidalFieldAla "mixins"               (alaList  CommaVCat)          L.mixins
        ^^^ availableSince CabalSpecV2_0 []
{-# SPECIALIZE buildInfoFieldGrammar :: ParsecFieldGrammar' BuildInfo #-}
{-# SPECIALIZE buildInfoFieldGrammar :: PrettyFieldGrammar' BuildInfo #-}

hsSourceDirsGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo [FilePath]
hsSourceDirsGrammar = (++)
    <$> monoidalFieldAla "hs-source-dirs" (alaList' FSep FilePathNT) L.hsSourceDirs
    <*> monoidalFieldAla "hs-source-dir"  (alaList' FSep FilePathNT) wrongLens
        --- https://github.com/haskell/cabal/commit/49e3cdae3bdf21b017ccd42e66670ca402e22b44
        ^^^ deprecatedSince CabalSpecV1_2 "Please use 'hs-source-dirs'"
        ^^^ removedIn CabalSpecV3_0 "Please use 'hs-source-dirs' field."
  where
    -- TODO: make pretty printer aware of CabalSpecVersion
    wrongLens :: Functor f => LensLike' f BuildInfo [FilePath]
    wrongLens f bi = (\fps -> set L.hsSourceDirs fps bi) <$> f []

optionsFieldGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
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
    :: (FieldGrammar g, Applicative (g BuildInfo))
    => g BuildInfo (PerCompilerFlavor [String])
profOptionsFieldGrammar = PerCompilerFlavor
    <$> monoidalFieldAla "ghc-prof-options"   (alaList' NoCommaFSep Token') (extract GHC)
    <*> monoidalFieldAla "ghcjs-prof-options" (alaList' NoCommaFSep Token') (extract GHCJS)
  where
    extract :: CompilerFlavor -> ALens' BuildInfo [String]
    extract flavor = L.profOptions . lookupLens flavor

sharedOptionsFieldGrammar
    :: (FieldGrammar g, Applicative (g BuildInfo))
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
    :: (FieldGrammar g, Applicative (g Flag))
    =>  FlagName -> g Flag Flag
flagFieldGrammar name = MkFlag name
    <$> freeTextFieldDef    "description"          L.flagDescription
    <*> booleanFieldDef     "default"              L.flagDefault     True
    <*> booleanFieldDef     "manual"               L.flagManual      False
{-# SPECIALIZE flagFieldGrammar :: FlagName -> ParsecFieldGrammar' Flag #-}
{-# SPECIALIZE flagFieldGrammar :: FlagName -> PrettyFieldGrammar' Flag #-}

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

sourceRepoFieldGrammar
    :: (FieldGrammar g, Applicative (g SourceRepo))
    => RepoKind -> g SourceRepo SourceRepo
sourceRepoFieldGrammar kind = SourceRepo kind
    <$> optionalField    "type"                L.repoType
    <*> freeTextField    "location"            L.repoLocation
    <*> optionalFieldAla "module"   Token      L.repoModule
    <*> optionalFieldAla "branch"   Token      L.repoBranch
    <*> optionalFieldAla "tag"      Token      L.repoTag
    <*> optionalFieldAla "subdir"   FilePathNT L.repoSubdir
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind -> ParsecFieldGrammar' SourceRepo #-}
{-# SPECIALIZE sourceRepoFieldGrammar :: RepoKind ->PrettyFieldGrammar' SourceRepo #-}

-------------------------------------------------------------------------------
-- SetupBuildInfo
-------------------------------------------------------------------------------

setupBInfoFieldGrammar
    :: (FieldGrammar g, Functor (g SetupBuildInfo))
    => Bool -> g SetupBuildInfo SetupBuildInfo
setupBInfoFieldGrammar def = flip SetupBuildInfo def
    <$> monoidalFieldAla "setup-depends" (alaList CommaVCat) L.setupDepends
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool -> ParsecFieldGrammar' SetupBuildInfo #-}
{-# SPECIALIZE setupBInfoFieldGrammar :: Bool ->PrettyFieldGrammar' SetupBuildInfo #-}
