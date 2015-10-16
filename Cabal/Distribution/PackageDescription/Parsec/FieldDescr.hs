{-# LANGUAGE OverloadedStrings #-}
-- | 'GenericPackageDescription' Field descriptions
module Distribution.PackageDescription.Parsec.FieldDescr (
    -- * Package description
    pkgDescrFieldDescrs,
    storeXFieldsPD,
    -- * Library
    libFieldDescrs,
    storeXFieldsLib,
    -- * Executable
    executableFieldDescrs,
    storeXFieldsExe,
    -- * Test suite
    TestSuiteStanza (..),
    emptyTestStanza,
    testSuiteFieldDescrs,
    storeXFieldsTest,
    validateTestSuite,
    -- * Benchmark
    BenchmarkStanza (..),
    emptyBenchmarkStanza,
    benchmarkFieldDescrs,
    storeXFieldsBenchmark,
    validateBenchmark,
    -- * Flag
    flagFieldDescrs,
    -- * Source repository
    sourceRepoFieldDescrs,
    -- * Setup build info
    setupBInfoFieldDescrs,
    ) where

import           Distribution.Compat.Prelude
import           Prelude ()

import           Text.PrettyPrint                      (vcat)

import qualified Data.ByteString                       as BS
import           Data.List                             (dropWhileEnd)
import qualified Distribution.Compat.Parsec            as Parsec
import           Distribution.Compiler                 (CompilerFlavor (..))
import           Distribution.ModuleName               (ModuleName)
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Parsec.Class
import           Distribution.Parsec.Types.Common
import           Distribution.Parsec.Types.FieldDescr
import           Distribution.Parsec.Types.ParseResult
import           Distribution.PrettyUtils
import           Distribution.Simple.Utils             (fromUTF8BS)
import           Distribution.Text                     (disp, display)

-------------------------------------------------------------------------------
-- common FieldParsers
-------------------------------------------------------------------------------

-- | This is /almost/ @'many' 'Distribution.Compat.Parsec.anyChar'@, but it
--
-- * trims whitespace from ends of the lines,
--
-- * converts lines with only single dot into empty line.
--
freeTextFieldParser :: FieldParser String
freeTextFieldParser = dropDotLines <$ Parsec.spaces <*> many Parsec.anyChar
  where
    -- http://hackage.haskell.org/package/copilot-cbmc-0.1/copilot-cbmc.cabal
    dropDotLines "." = "."
    dropDotLines x = intercalate "\n" . map dotToEmpty . lines $ x
    dotToEmpty x | trim' x == "." = ""
    dotToEmpty x                  = trim x

    trim' = dropWhileEnd (`elem` (" \t" :: String))

-------------------------------------------------------------------------------
-- PackageDescription
-------------------------------------------------------------------------------

-- TODO: other-files isn't used in any cabla file on Hackage.
pkgDescrFieldDescrs :: [FieldDescr PackageDescription]
pkgDescrFieldDescrs =
    [ simpleField "name"
        disp                   parsec
        packageName            (\name pkg -> pkg{package=(package pkg){pkgName=name}})
    , simpleField "version"
        disp                   parsec
        packageVersion         (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
    , simpleField "cabal-version"
             (either disp disp)     (Left <$> parsec <|> Right <$> parsec)
             specVersionRaw         (\v pkg -> pkg{specVersionRaw=v})
    , simpleField "build-type"
             (maybe mempty disp)    (Just <$> parsec)
             buildType              (\t pkg -> pkg{buildType=t})
    , simpleField "license"
             disp                   (parsecMaybeQuoted parsec)
             license                (\l pkg -> pkg{license=l})
    , simpleField "license-file"
             showFilePath           parsecFilePath
             (\pkg -> case licenseFiles pkg of
                        [x] -> x
                        _   -> "")
             (\l pkg -> pkg{licenseFiles=licenseFiles pkg ++ [l]})
     -- We have both 'license-file' and 'license-files' fields.
     -- Rather than declaring license-file to be deprecated, we will continue
     -- to allow both. The 'license-file' will continue to only allow single
     -- tokens, while 'license-files' allows multiple. On pretty-printing, we
     -- will use 'license-file' if there's just one, and use 'license-files'
     -- otherwise.
   , listField "license-files"
             showFilePath          parsecFilePath
             (\pkg -> case licenseFiles pkg of
                        [_] -> []
                        xs  -> xs)
             (\ls pkg -> pkg{licenseFiles=ls})
   , simpleField "copyright"
             showFreeText           freeTextFieldParser
             copyright              (\val pkg -> pkg{copyright=val})
   , simpleField "maintainer"
             showFreeText           freeTextFieldParser
             maintainer             (\val pkg -> pkg{maintainer=val})
   , simpleField "stability"
             showFreeText           freeTextFieldParser
             stability              (\val pkg -> pkg{stability=val})
   , simpleField "homepage"
             showFreeText           freeTextFieldParser
             homepage               (\val pkg -> pkg{homepage=val})
   , simpleField "package-url"
             showFreeText           freeTextFieldParser
             pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
   , simpleField "bug-reports"
             showFreeText           freeTextFieldParser
             bugReports             (\val pkg -> pkg{bugReports=val})
   , simpleField "synopsis"
             showFreeText           freeTextFieldParser
             synopsis               (\val pkg -> pkg{synopsis=val})
   , simpleField "description"
             showFreeText           freeTextFieldParser
             description            (\val pkg -> pkg{description=val})
   , simpleField "category"
             showFreeText           freeTextFieldParser
             category               (\val pkg -> pkg{category=val})
   , simpleField "author"
             showFreeText           freeTextFieldParser
             author                 (\val pkg -> pkg{author=val})
   , listField "tested-with"
             showTestedWith         parsecTestedWith
             testedWith             (\val pkg -> pkg{testedWith=val})
   , listFieldWithSep vcat "data-files"
             showFilePath           parsecFilePath
             dataFiles              (\val pkg -> pkg{dataFiles=val})
   , simpleField "data-dir"
             showFilePath           parsecFilePath
             dataDir                (\val pkg -> pkg{dataDir=val})
   , listFieldWithSep vcat "extra-source-files"
             showFilePath           parsecFilePath
             extraSrcFiles          (\val pkg -> pkg{extraSrcFiles=val})
   , listFieldWithSep vcat "extra-tmp-files"
             showFilePath           parsecFilePath
             extraTmpFiles          (\val pkg -> pkg{extraTmpFiles=val})
   , listFieldWithSep vcat "extra-doc-files"
             showFilePath           parsecFilePath
             extraDocFiles          (\val pkg -> pkg{extraDocFiles=val})
   ]

-- | Store any fields beginning with "x-" in the customFields field of
--   a PackageDescription.  All other fields will generate a warning.
storeXFieldsPD :: UnknownFieldParser PackageDescription
storeXFieldsPD f val pkg | beginsWithX f =
    Just pkg { customFieldsPD = customFieldsPD pkg ++ [(fromUTF8BS f, trim val)] }
storeXFieldsPD _ _ _ = Nothing

-------------------------------------------------------------------------------
-- Library
-------------------------------------------------------------------------------

libFieldDescrs :: [FieldDescr Library]
libFieldDescrs =
    [ listFieldWithSep vcat "exposed-modules" disp (parsecMaybeQuoted parsec)
        exposedModules (\mods lib -> lib{exposedModules=mods})
    , commaListFieldWithSep vcat "reexported-modules" disp parsec
        reexportedModules (\mods lib -> lib{reexportedModules=mods})

{-
  , listFieldWithSep vcat "required-signatures" disp parseModuleNameQ
      requiredSignatures (\mods lib -> lib{requiredSignatures=mods})

-}
    , boolField "exposed"
        libExposed     (\val lib -> lib{libExposed=val})
    ] ++ map biToLib binfoFieldDescrs
  where
    biToLib = liftField libBuildInfo (\bi lib -> lib{libBuildInfo=bi})

storeXFieldsLib :: UnknownFieldParser Library
storeXFieldsLib f val l@Library { libBuildInfo = bi } | beginsWithX f =
    Just $ l {libBuildInfo =
                 bi{ customFieldsBI = customFieldsBI bi ++ [(fromUTF8BS f, trim val)]}}
storeXFieldsLib _ _ _ = Nothing

-------------------------------------------------------------------------------
-- Executable
-------------------------------------------------------------------------------

executableFieldDescrs :: [FieldDescr Executable]
executableFieldDescrs =
    [ -- note ordering: configuration must come first, for
      -- showPackageDescription.
      simpleField "executable"
        showToken          parsecToken
        exeName            (\xs    exe -> exe{exeName=xs})
    , simpleField "main-is"
        showFilePath       parsecFilePath
        modulePath         (\xs    exe -> exe{modulePath=xs})
    ]
    ++ map biToExe binfoFieldDescrs
  where
    biToExe = liftField buildInfo (\bi exe -> exe{buildInfo=bi})

storeXFieldsExe :: UnknownFieldParser Executable
storeXFieldsExe f val e@Executable { buildInfo = bi } | beginsWithX f =
    Just $ e {buildInfo = bi{ customFieldsBI = (fromUTF8BS f, trim val) : customFieldsBI bi}}
storeXFieldsExe _ _ _ = Nothing

-------------------------------------------------------------------------------
-- TestSuite
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza
    { testStanzaTestType   :: Maybe TestType
    , testStanzaMainIs     :: Maybe FilePath
    , testStanzaTestModule :: Maybe ModuleName
    , testStanzaBuildInfo  :: BuildInfo
    }

emptyTestStanza :: TestSuiteStanza
emptyTestStanza = TestSuiteStanza Nothing Nothing Nothing mempty

testSuiteFieldDescrs :: [FieldDescr TestSuiteStanza]
testSuiteFieldDescrs =
    [ simpleField "type"
        (maybe mempty disp)   (Just <$> parsec)
        testStanzaTestType    (\x suite -> suite { testStanzaTestType = x })
    , simpleField "main-is"
        (maybe mempty showFilePath) (Just <$> parsecFilePath)
        testStanzaMainIs      (\x suite -> suite { testStanzaMainIs = x })
    , simpleField "test-module"
        (maybe mempty disp)   (Just <$> parsecMaybeQuoted parsec)
        testStanzaTestModule  (\x suite -> suite { testStanzaTestModule = x })
    ]
    ++ map biToTest binfoFieldDescrs
  where
    biToTest = liftField
        testStanzaBuildInfo
        (\bi suite -> suite { testStanzaBuildInfo = bi })

storeXFieldsTest :: UnknownFieldParser TestSuiteStanza
storeXFieldsTest f val t@TestSuiteStanza { testStanzaBuildInfo = bi }
    | beginsWithX f =
        Just $ t {testStanzaBuildInfo = bi{ customFieldsBI = (fromUTF8BS f,val):customFieldsBI bi}}
storeXFieldsTest _ _ _ = Nothing

validateTestSuite :: Position -> TestSuiteStanza -> ParseResult TestSuite
validateTestSuite pos stanza = case testStanzaTestType stanza of
    Nothing -> return $
        emptyTestSuite { testBuildInfo = testStanzaBuildInfo stanza }

    Just tt@(TestTypeUnknown _ _) ->
        pure emptyTestSuite
            { testInterface = TestSuiteUnsupported tt
            , testBuildInfo = testStanzaBuildInfo stanza
            }

    Just tt | tt `notElem` knownTestTypes ->
        pure emptyTestSuite
            { testInterface = TestSuiteUnsupported tt
            , testBuildInfo = testStanzaBuildInfo stanza
            }

    Just tt@(TestTypeExe ver) -> case testStanzaMainIs stanza of
        Nothing   -> do
            parseFailure pos (missingField "main-is" tt)
            pure emptyTestSuite
        Just file -> do
            when (isJust (testStanzaTestModule stanza)) $
                parseWarning pos PWTExtraBenchmarkModule (extraField "test-module" tt)
            pure emptyTestSuite
                { testInterface = TestSuiteExeV10 ver file
                , testBuildInfo = testStanzaBuildInfo stanza
                }

    Just tt@(TestTypeLib ver) -> case testStanzaTestModule stanza of
         Nothing      -> do
             parseFailure pos (missingField "test-module" tt)
             pure emptyTestSuite
         Just module_ -> do
            when (isJust (testStanzaMainIs stanza)) $
                parseWarning pos PWTExtraMainIs (extraField "main-is" tt)
            pure emptyTestSuite
                { testInterface = TestSuiteLibV09 ver module_
                , testBuildInfo = testStanzaBuildInfo stanza
                }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " test suite type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' test suite type."

-------------------------------------------------------------------------------
-- Benchmark
-------------------------------------------------------------------------------

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza
    { benchmarkStanzaBenchmarkType   :: Maybe BenchmarkType
    , benchmarkStanzaMainIs          :: Maybe FilePath
    , benchmarkStanzaBenchmarkModule :: Maybe ModuleName
    , benchmarkStanzaBuildInfo        :: BuildInfo
    }

emptyBenchmarkStanza :: BenchmarkStanza
emptyBenchmarkStanza = BenchmarkStanza Nothing Nothing Nothing mempty

benchmarkFieldDescrs :: [FieldDescr BenchmarkStanza]
benchmarkFieldDescrs =
    [ simpleField "type"
        (maybe mempty disp)    (Just <$> parsec)
        benchmarkStanzaBenchmarkType
        (\x suite -> suite { benchmarkStanzaBenchmarkType = x })
    , simpleField "main-is"
        (maybe mempty showFilePath)  (Just <$> parsecFilePath)
        benchmarkStanzaMainIs
        (\x suite -> suite { benchmarkStanzaMainIs = x })
    ]
    ++ map biToBenchmark binfoFieldDescrs
  where
    biToBenchmark = liftField benchmarkStanzaBuildInfo
                    (\bi suite -> suite { benchmarkStanzaBuildInfo = bi })

storeXFieldsBenchmark :: UnknownFieldParser BenchmarkStanza
storeXFieldsBenchmark f val t@BenchmarkStanza { benchmarkStanzaBuildInfo = bi } | beginsWithX f =
    Just $ t {benchmarkStanzaBuildInfo =
                       bi{ customFieldsBI = (fromUTF8BS f, trim val):customFieldsBI bi}}
storeXFieldsBenchmark _ _ _ = Nothing

validateBenchmark :: Position -> BenchmarkStanza -> ParseResult Benchmark
validateBenchmark pos stanza = case benchmarkStanzaBenchmarkType stanza of
    Nothing -> pure emptyBenchmark
        { benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza }

    Just tt@(BenchmarkTypeUnknown _ _) -> pure emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
        }

    Just tt | tt `notElem` knownBenchmarkTypes -> pure emptyBenchmark
        { benchmarkInterface = BenchmarkUnsupported tt
        , benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
        }

    Just tt@(BenchmarkTypeExe ver) -> case benchmarkStanzaMainIs stanza of
        Nothing   -> do
            parseFailure pos (missingField "main-is" tt)
            pure emptyBenchmark
        Just file -> do
            when (isJust (benchmarkStanzaBenchmarkModule stanza)) $
                parseWarning pos PWTExtraBenchmarkModule (extraField "benchmark-module" tt)
            pure emptyBenchmark
                { benchmarkInterface = BenchmarkExeV10 ver file
                , benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
                }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " benchmark type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' benchmark type."

-------------------------------------------------------------------------------
-- BuildInfo
-------------------------------------------------------------------------------

binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ boolField "buildable"
           buildable          (\val binfo -> binfo{buildable=val})
 , commaListField  "build-tools"
           disp               parsecBuildTool
           buildTools         (\xs  binfo -> binfo{buildTools=xs})
 , commaListFieldWithSep vcat "build-depends"
       disp               parsec
       targetBuildDepends (\xs binfo -> binfo{targetBuildDepends=xs})
 , spaceListField "cpp-options"
           showToken          parsecToken'
           cppOptions          (\val binfo -> binfo{cppOptions=val})
 , spaceListField "cc-options"
           showToken          parsecToken'
           ccOptions          (\val binfo -> binfo{ccOptions=val})
 , spaceListField "ld-options"
           showToken          parsecToken'
           ldOptions          (\val binfo -> binfo{ldOptions=val})
 , commaListField  "pkgconfig-depends"
           disp               parsecPkgconfigDependency
           pkgconfigDepends   (\xs  binfo -> binfo{pkgconfigDepends=xs})
 , listField "frameworks"
           showToken          parsecToken
           frameworks         (\val binfo -> binfo{frameworks=val})
 , listField "extra-framework-dirs"
           showToken          parsecFilePath
           extraFrameworkDirs (\val binfo -> binfo{extraFrameworkDirs=val})
 , listFieldWithSep vcat "c-sources"
           showFilePath       parsecFilePath
           cSources           (\paths binfo -> binfo{cSources=paths})
 , listFieldWithSep vcat "js-sources"
           showFilePath       parsecFilePath
           jsSources          (\paths binfo -> binfo{jsSources=paths})
   , simpleField "default-language"
       (maybe mempty disp) (Parsec.optionMaybe $ parsecMaybeQuoted parsec)
        defaultLanguage    (\lang  binfo -> binfo{defaultLanguage=lang})
 , listField   "other-languages"
           disp              (parsecMaybeQuoted parsec)
           otherLanguages     (\langs binfo -> binfo{otherLanguages=langs})
   , listField   "default-extensions"
       disp               (parsecMaybeQuoted parsec)
       defaultExtensions  (\exts  binfo -> binfo{defaultExtensions=exts})
   , listField   "other-extensions"
       disp               (parsecMaybeQuoted parsec)
       otherExtensions    (\exts  binfo -> binfo{otherExtensions=exts})
   , listField   "extensions"
       -- TODO: this is deprecated field, isn't it?
       disp               (parsecMaybeQuoted parsec)
       oldExtensions      (\exts  binfo -> binfo{oldExtensions=exts})
 , listFieldWithSep vcat "extra-libraries"
           showToken          parsecToken
           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listFieldWithSep vcat "extra-ghci-libraries"
           showToken          parsecToken
           extraGHCiLibs      (\xs    binfo -> binfo{extraGHCiLibs=xs})
 , listField   "extra-lib-dirs"
           showFilePath       parsecFilePath
           extraLibDirs       (\xs    binfo -> binfo{extraLibDirs=xs})
 , listFieldWithSep vcat "includes"
           showFilePath       parsecFilePath
           includes           (\paths binfo -> binfo{includes=paths})
 , listFieldWithSep vcat "install-includes"
           showFilePath       parsecFilePath
           installIncludes    (\paths binfo -> binfo{installIncludes=paths})
 , listField   "include-dirs"
           showFilePath       parsecFilePath
           includeDirs        (\paths binfo -> binfo{includeDirs=paths})
   , listField   "hs-source-dirs"
       showFilePath        parsecFilePath
       hsSourceDirs       (\paths binfo -> binfo{hsSourceDirs=paths})
   , deprecatedField "hs-source-dirs" $ listField  "hs-source-dir"
       showFilePath        parsecFilePath
       (const [])          (\paths binfo -> binfo{hsSourceDirs=paths})
   , listFieldWithSep vcat "other-modules"
       disp               (parsecMaybeQuoted parsec)
       otherModules       (\val binfo -> binfo{otherModules=val})
 , optsField   "ghc-prof-options" GHC
           profOptions        (\val binfo -> binfo{profOptions=val})
 , optsField   "ghcjs-prof-options" GHCJS
           profOptions        (\val binfo -> binfo{profOptions=val})
 , optsField   "ghc-shared-options" GHC
           sharedOptions      (\val binfo -> binfo{sharedOptions=val})
 , optsField   "ghcjs-shared-options" GHCJS
           sharedOptions      (\val binfo -> binfo{sharedOptions=val})
   , optsField   "ghc-options"  GHC
        options            (\path  binfo -> binfo{options=path})
 , optsField   "ghcjs-options" GHCJS
           options            (\path  binfo -> binfo{options=path})
 , optsField   "jhc-options"  JHC
           options            (\path  binfo -> binfo{options=path})
 -- NOTE: Hugs and NHC are not supported anymore, but these fields are kept
 -- around for backwards compatibility.
 --
 -- TODO: deprecate?
 , optsField   "hugs-options" Hugs
           options            (const id)
 , optsField   "nhc98-options" NHC
           options            (const id)
   ]

{-
storeXFieldsBI :: UnknownFieldParser BuildInfo
--storeXFieldsBI (f@('x':'-':_),val) bi = Just bi{ customFieldsBI = (f,val):customFieldsBI bi }
storeXFieldsBI _ _ = Nothing
-}

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagFieldDescrs :: [FieldDescr Flag]
flagFieldDescrs =
    [ simpleField "description"
        showFreeText     freeTextFieldParser
        flagDescription  (\val fl -> fl{ flagDescription = val })
    , boolField "default"
        flagDefault      (\val fl -> fl{ flagDefault = val })
    , boolField "manual"
        flagManual       (\val fl -> fl{ flagManual = val })
    ]

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

sourceRepoFieldDescrs :: [FieldDescr SourceRepo]
sourceRepoFieldDescrs =
    [ simpleField "type"
        (maybe mempty disp)         (Just <$> parsec)
        repoType                    (\val repo -> repo { repoType = val })
    , simpleField "location"
        (maybe mempty showFreeText) (Just <$> freeTextFieldParser)
        repoLocation                (\val repo -> repo { repoLocation = val })
    , simpleField "module"
        (maybe mempty showToken)    (Just <$> parsecToken)
        repoModule                  (\val repo -> repo { repoModule = val })
    , simpleField "branch"
        (maybe mempty showToken)    (Just <$> parsecToken)
        repoBranch                  (\val repo -> repo { repoBranch = val })
    , simpleField "tag"
        (maybe mempty showToken)    (Just <$> parsecToken)
        repoTag                     (\val repo -> repo { repoTag = val })
    , simpleField "subdir"
        (maybe mempty showFilePath) (Just <$> parsecFilePath)
        repoSubdir                  (\val repo -> repo { repoSubdir = val })
    ]

-------------------------------------------------------------------------------
-- SetupBuildInfo
-------------------------------------------------------------------------------

setupBInfoFieldDescrs :: [FieldDescr SetupBuildInfo]
setupBInfoFieldDescrs =
    [ commaListFieldWithSep vcat "setup-depends"
        disp         parsec
        setupDepends (\xs binfo -> binfo{setupDepends=xs})
    ]


-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

beginsWithX :: FieldName -> Bool
beginsWithX bs = case BS.uncons bs of
    Just (x, _)
        | x == fromIntegral (ord 'x') -> True
    _                                 -> False

-- | Mark the field as deprecated.
deprecatedField
    :: FieldName   -- ^ alternative field
    -> FieldDescr a
    -> FieldDescr a
deprecatedField newFieldName fd = FieldDescr
    { fieldName   = oldFieldName
    , fieldPretty = const mempty  -- we don't print deprecated field
    , fieldParser = \x -> do
        parsecWarning PWTDeprecatedField $
            "The field " <> show oldFieldName <>
            " is deprecated, please use " <> show newFieldName
        fieldParser fd x
    }
  where
    oldFieldName = fieldName fd

-- Used to trim x-fields
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
