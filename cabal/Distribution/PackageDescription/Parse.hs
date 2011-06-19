-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Parse
-- Copyright   :  Isaac Jones 2003-2005
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defined parsers and partial pretty printers for the @.cabal@ format.
-- Some of the complexity in this module is due to the fact that we have to be
-- backwards compatible with old @.cabal@ files, so there's code to translate
-- into the newer structure.

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

module Distribution.PackageDescription.Parse (
        -- * Package descriptions
        readPackageDescription,
        writePackageDescription,
        parsePackageDescription,
        showPackageDescription,

        -- ** Parsing
        ParseResult(..),
        FieldDescr(..),
        LineNo,

        -- ** Supplementary build information
        readHookedBuildInfo,
        parseHookedBuildInfo,
        writeHookedBuildInfo,
        showHookedBuildInfo,

        pkgDescrFieldDescrs,
        libFieldDescrs,
        executableFieldDescrs,
        binfoFieldDescrs,
        sourceRepoFieldDescrs,
        testSuiteFieldDescrs,
        flagFieldDescrs
  ) where

import Data.Char  (isSpace)
import Data.Maybe (listToMaybe, isJust)
import Data.Monoid ( Monoid(..) )
import Data.List  (nub, unfoldr, partition, (\\))
import Control.Monad (liftM, foldM, when, unless)
import System.Directory (doesFileExist)

import Distribution.Text
         ( Text(disp, parse), display, simpleParse )
import Distribution.Compat.ReadP
         ((+++), option)
import Text.PrettyPrint.HughesPJ

import Distribution.ParseUtils hiding (parseFields)
import Distribution.PackageDescription
import Distribution.Package
         ( PackageIdentifier(..), Dependency(..), packageName, packageVersion )
import Distribution.ModuleName ( ModuleName )
import Distribution.Version
        ( Version(Version), orLaterVersion
        , LowerBound(..), asVersionIntervals )
import Distribution.Verbosity (Verbosity)
import Distribution.Compiler  (CompilerFlavor(..))
import Distribution.PackageDescription.Configuration (parseCondition, freeVars)
import Distribution.Simple.Utils
         ( die, dieWithLocation, warn, intercalate, lowercase, cabalVersion
         , withFileContents, withUTF8FileContents
         , writeFileAtomic, writeUTF8File )


-- -----------------------------------------------------------------------------
-- The PackageDescription type

pkgDescrFieldDescrs :: [FieldDescr PackageDescription]
pkgDescrFieldDescrs =
    [ simpleField "name"
           disp                   parse
           packageName            (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
           disp                   parse
           packageVersion         (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , simpleField "cabal-version"
           (either disp disp)     (liftM Left parse +++ liftM Right parse)
           specVersionRaw         (\v pkg -> pkg{specVersionRaw=v})
 , simpleField "build-type"
           (maybe empty disp)     (fmap Just parse)
           buildType              (\t pkg -> pkg{buildType=t})
 , simpleField "license"
           disp                   parseLicenseQ
           license                (\l pkg -> pkg{license=l})
 , simpleField "license-file"
           showFilePath           parseFilePathQ
           licenseFile            (\l pkg -> pkg{licenseFile=l})
 , simpleField "copyright"
           showFreeText           parseFreeText
           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
           showFreeText           parseFreeText
           maintainer             (\val pkg -> pkg{maintainer=val})
 , commaListField  "build-depends"
           disp                   parse
           buildDepends           (\xs    pkg -> pkg{buildDepends=xs})
 , simpleField "stability"
           showFreeText           parseFreeText
           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
           showFreeText           parseFreeText
           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
           showFreeText           parseFreeText
           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "bug-reports"
           showFreeText           parseFreeText
           bugReports             (\val pkg -> pkg{bugReports=val})
 , simpleField "synopsis"
           showFreeText           parseFreeText
           synopsis               (\val pkg -> pkg{synopsis=val})
 , simpleField "description"
           showFreeText           parseFreeText
           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
           showFreeText           parseFreeText
           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
           showFreeText           parseFreeText
           author                 (\val pkg -> pkg{author=val})
 , listField "tested-with"
           showTestedWith         parseTestedWithQ
           testedWith             (\val pkg -> pkg{testedWith=val})
 , listField "data-files"
           showFilePath           parseFilePathQ
           dataFiles              (\val pkg -> pkg{dataFiles=val})
 , simpleField "data-dir"
           showFilePath           parseFilePathQ
           dataDir                (\val pkg -> pkg{dataDir=val})
 , listField "extra-source-files"
           showFilePath    parseFilePathQ
           extraSrcFiles          (\val pkg -> pkg{extraSrcFiles=val})
 , listField "extra-tmp-files"
           showFilePath       parseFilePathQ
           extraTmpFiles          (\val pkg -> pkg{extraTmpFiles=val})
 ]

-- | Store any fields beginning with "x-" in the customFields field of
--   a PackageDescription.  All other fields will generate a warning.
storeXFieldsPD :: UnrecFieldParser PackageDescription
storeXFieldsPD (f@('x':'-':_),val) pkg = Just pkg{ customFieldsPD =
                                                        (customFieldsPD pkg) ++ [(f,val)]}
storeXFieldsPD _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Library type

libFieldDescrs :: [FieldDescr Library]
libFieldDescrs =
  [ listField "exposed-modules" disp parseModuleNameQ
      exposedModules (\mods lib -> lib{exposedModules=mods})

  , boolField "exposed"
      libExposed     (\val lib -> lib{libExposed=val})
  ] ++ map biToLib binfoFieldDescrs
  where biToLib = liftField libBuildInfo (\bi lib -> lib{libBuildInfo=bi})

storeXFieldsLib :: UnrecFieldParser Library
storeXFieldsLib (f@('x':'-':_), val) l@(Library { libBuildInfo = bi }) =
    Just $ l {libBuildInfo = bi{ customFieldsBI = (customFieldsBI bi) ++ [(f,val)]}}
storeXFieldsLib _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Executable type


executableFieldDescrs :: [FieldDescr Executable]
executableFieldDescrs =
  [ -- note ordering: configuration must come first, for
    -- showPackageDescription.
    simpleField "executable"
                           showToken          parseTokenQ
                           exeName            (\xs    exe -> exe{exeName=xs})
  , simpleField "main-is"
                           showFilePath       parseFilePathQ
                           modulePath         (\xs    exe -> exe{modulePath=xs})
  ]
  ++ map biToExe binfoFieldDescrs
  where biToExe = liftField buildInfo (\bi exe -> exe{buildInfo=bi})

storeXFieldsExe :: UnrecFieldParser Executable
storeXFieldsExe (f@('x':'-':_), val) e@(Executable { buildInfo = bi }) =
    Just $ e {buildInfo = bi{ customFieldsBI = (f,val):(customFieldsBI bi)}}
storeXFieldsExe _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The TestSuite type

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza {
       testStanzaTestType   :: Maybe TestType,
       testStanzaMainIs     :: Maybe FilePath,
       testStanzaTestModule :: Maybe ModuleName,
       testStanzaBuildInfo  :: BuildInfo
     }

emptyTestStanza :: TestSuiteStanza
emptyTestStanza = TestSuiteStanza Nothing Nothing Nothing mempty

testSuiteFieldDescrs :: [FieldDescr TestSuiteStanza]
testSuiteFieldDescrs =
    [ simpleField "type"
        (maybe empty disp)    (fmap Just parse)
        testStanzaTestType    (\x suite -> suite { testStanzaTestType = x })
    , simpleField "main-is"
        (maybe empty showFilePath)  (fmap Just parseFilePathQ)
        testStanzaMainIs      (\x suite -> suite { testStanzaMainIs = x })
    , simpleField "test-module"
        (maybe empty disp)    (fmap Just parseModuleNameQ)
        testStanzaTestModule  (\x suite -> suite { testStanzaTestModule = x })
    ]
    ++ map biToTest binfoFieldDescrs
  where
    biToTest = liftField testStanzaBuildInfo
                         (\bi suite -> suite { testStanzaBuildInfo = bi })

storeXFieldsTest :: UnrecFieldParser TestSuiteStanza
storeXFieldsTest (f@('x':'-':_), val) t@(TestSuiteStanza { testStanzaBuildInfo = bi }) =
    Just $ t {testStanzaBuildInfo = bi{ customFieldsBI = (f,val):(customFieldsBI bi)}}
storeXFieldsTest _ _ = Nothing

validateTestSuite :: LineNo -> TestSuiteStanza -> ParseResult TestSuite
validateTestSuite line stanza =
    case testStanzaTestType stanza of
      Nothing -> return $
        emptyTestSuite { testBuildInfo = testStanzaBuildInfo stanza }

      Just tt@(TestTypeUnknown _ _) ->
        return emptyTestSuite {
          testInterface = TestSuiteUnsupported tt,
          testBuildInfo = testStanzaBuildInfo stanza
        }

      Just tt | tt `notElem` knownTestTypes ->
        return emptyTestSuite {
          testInterface = TestSuiteUnsupported tt,
          testBuildInfo = testStanzaBuildInfo stanza
        }

      Just tt@(TestTypeExe ver) ->
        case testStanzaMainIs stanza of
          Nothing   -> syntaxError line (missingField "main-is" tt)
          Just file -> do
            when (isJust (testStanzaTestModule stanza)) $
              warning (extraField "test-module" tt)
            return emptyTestSuite {
              testInterface = TestSuiteExeV10 ver file,
              testBuildInfo = testStanzaBuildInfo stanza
            }

      Just tt@(TestTypeLib ver) ->
        case testStanzaTestModule stanza of
          Nothing      -> syntaxError line (missingField "test-module" tt)
          Just module_ -> do
            when (isJust (testStanzaMainIs stanza)) $
              warning (extraField "main-is" tt)
            return emptyTestSuite {
              testInterface = TestSuiteLibV09 ver module_,
              testBuildInfo = testStanzaBuildInfo stanza
            }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " test suite type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' test suite type."


-- ---------------------------------------------------------------------------
-- The BuildInfo type


binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ boolField "buildable"
           buildable          (\val binfo -> binfo{buildable=val})
 , commaListField  "build-tools"
           disp               parseBuildTool
           buildTools         (\xs  binfo -> binfo{buildTools=xs})
 , spaceListField "cpp-options"
           showToken          parseTokenQ'
           cppOptions          (\val binfo -> binfo{cppOptions=val})
 , spaceListField "cc-options"
           showToken          parseTokenQ'
           ccOptions          (\val binfo -> binfo{ccOptions=val})
 , spaceListField "ld-options"
           showToken          parseTokenQ'
           ldOptions          (\val binfo -> binfo{ldOptions=val})
 , commaListField  "pkgconfig-depends"
           disp               parsePkgconfigDependency
           pkgconfigDepends   (\xs  binfo -> binfo{pkgconfigDepends=xs})
 , listField "frameworks"
           showToken          parseTokenQ
           frameworks         (\val binfo -> binfo{frameworks=val})
 , listField   "c-sources"
           showFilePath       parseFilePathQ
           cSources           (\paths binfo -> binfo{cSources=paths})

 , simpleField "default-language"
           (maybe empty disp) (option Nothing (fmap Just parseLanguageQ))
           defaultLanguage    (\lang  binfo -> binfo{defaultLanguage=lang})
 , listField   "other-languages"
           disp               parseLanguageQ
           otherLanguages     (\langs binfo -> binfo{otherLanguages=langs})
 , listField   "default-extensions"
           disp               parseExtensionQ
           defaultExtensions  (\exts  binfo -> binfo{defaultExtensions=exts})
 , listField   "other-extensions"
           disp               parseExtensionQ
           otherExtensions    (\exts  binfo -> binfo{otherExtensions=exts})
 , listField   "extensions"
           disp               parseExtensionQ
           oldExtensions      (\exts  binfo -> binfo{oldExtensions=exts})

 , listField   "extra-libraries"
           showToken          parseTokenQ
           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listField   "extra-lib-dirs"
           showFilePath       parseFilePathQ
           extraLibDirs       (\xs    binfo -> binfo{extraLibDirs=xs})
 , listField   "includes"
           showFilePath       parseFilePathQ
           includes           (\paths binfo -> binfo{includes=paths})
 , listField   "install-includes"
           showFilePath       parseFilePathQ
           installIncludes    (\paths binfo -> binfo{installIncludes=paths})
 , listField   "include-dirs"
           showFilePath       parseFilePathQ
           includeDirs        (\paths binfo -> binfo{includeDirs=paths})
 , listField   "hs-source-dirs"
           showFilePath       parseFilePathQ
           hsSourceDirs       (\paths binfo -> binfo{hsSourceDirs=paths})
 , listField   "other-modules"
           disp               parseModuleNameQ
           otherModules       (\val binfo -> binfo{otherModules=val})
 , listField   "ghc-prof-options"
           text               parseTokenQ
           ghcProfOptions        (\val binfo -> binfo{ghcProfOptions=val})
 , listField   "ghc-shared-options"
           text               parseTokenQ
           ghcSharedOptions      (\val binfo -> binfo{ghcSharedOptions=val})
 , optsField   "ghc-options"  GHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "hugs-options" Hugs
           options            (\path  binfo -> binfo{options=path})
 , optsField   "nhc98-options"  NHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "jhc-options"  JHC
           options            (\path  binfo -> binfo{options=path})
 ]

storeXFieldsBI :: UnrecFieldParser BuildInfo
storeXFieldsBI (f@('x':'-':_),val) bi = Just bi{ customFieldsBI = (f,val):(customFieldsBI bi) }
storeXFieldsBI _ _ = Nothing

------------------------------------------------------------------------------

flagFieldDescrs :: [FieldDescr Flag]
flagFieldDescrs =
    [ simpleField "description"
        showFreeText     parseFreeText
        flagDescription  (\val fl -> fl{ flagDescription = val })
    , boolField "default"
        flagDefault      (\val fl -> fl{ flagDefault = val })
    , boolField "manual"
        flagManual       (\val fl -> fl{ flagManual = val })
    ]

------------------------------------------------------------------------------

sourceRepoFieldDescrs :: [FieldDescr SourceRepo]
sourceRepoFieldDescrs =
    [ simpleField "type"
        (maybe empty disp)         (fmap Just parse)
        repoType                   (\val repo -> repo { repoType = val })
    , simpleField "location"
        (maybe empty showFreeText) (fmap Just parseFreeText)
        repoLocation               (\val repo -> repo { repoLocation = val })
    , simpleField "module"
        (maybe empty showToken)    (fmap Just parseTokenQ)
        repoModule                 (\val repo -> repo { repoModule = val })
    , simpleField "branch"
        (maybe empty showToken)    (fmap Just parseTokenQ)
        repoBranch                 (\val repo -> repo { repoBranch = val })
    , simpleField "tag"
        (maybe empty showToken)    (fmap Just parseTokenQ)
        repoTag                    (\val repo -> repo { repoTag = val })
    , simpleField "subdir"
        (maybe empty showFilePath) (fmap Just parseFilePathQ)
        repoSubdir                 (\val repo -> repo { repoSubdir = val })
    ]

-- ---------------------------------------------------------------
-- Parsing

-- | Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
readAndParseFile :: (FilePath -> (String -> IO a) -> IO a)
                 -> (String -> ParseResult a)
                 -> Verbosity
                 -> FilePath -> IO a
readAndParseFile withFileContents' parser verbosity fpath = do
  exists <- doesFileExist fpath
  when (not exists) (die $ "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue.")
  withFileContents' fpath $ \str -> case parser str of
    ParseFailed e -> do
        let (line, message) = locatedErrorMsg e
        dieWithLocation fpath line message
    ParseOk warnings x -> do
        mapM_ (warn verbosity . showPWarning fpath) $ reverse warnings
        return x

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo =
    readAndParseFile withFileContents parseHookedBuildInfo

-- |Parse the given package file.
readPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readPackageDescription =
    readAndParseFile withUTF8FileContents parsePackageDescription

stanzas :: [Field] -> [[Field]]
stanzas [] = []
stanzas (f:fields) = (f:this) : stanzas rest
  where
    (this, rest) = break isStanzaHeader fields

isStanzaHeader :: Field -> Bool
isStanzaHeader (F _ f _) = f == "executable"
isStanzaHeader _ = False

------------------------------------------------------------------------------


mapSimpleFields :: (Field -> ParseResult Field) -> [Field]
                -> ParseResult [Field]
mapSimpleFields f fs = mapM walk fs
  where
    walk fld@(F _ _ _) = f fld
    walk (IfBlock l c fs1 fs2) = do
      fs1' <- mapM walk fs1
      fs2' <- mapM walk fs2
      return (IfBlock l c fs1' fs2')
    walk (Section ln n l fs1) = do
      fs1' <-  mapM walk fs1
      return (Section ln n l fs1')

-- prop_isMapM fs = mapSimpleFields return fs == return fs


-- names of fields that represents dependencies, thus consrca
constraintFieldNames :: [String]
constraintFieldNames = ["build-depends"]

-- Possible refactoring would be to have modifiers be explicit about what
-- they add and define an accessor that specifies what the dependencies
-- are.  This way we would completely reuse the parsing knowledge from the
-- field descriptor.
parseConstraint :: Field -> ParseResult [Dependency]
parseConstraint (F l n v)
    | n == "build-depends" = runP l n (parseCommaList parse) v
parseConstraint f = bug $ "Constraint was expected (got: " ++ show f ++ ")"

{-
headerFieldNames :: [String]
headerFieldNames = filter (\n -> not (n `elem` constraintFieldNames))
                 . map fieldName $ pkgDescrFieldDescrs
-}

libFieldNames :: [String]
libFieldNames = map fieldName libFieldDescrs
                ++ buildInfoNames ++ constraintFieldNames

-- exeFieldNames :: [String]
-- exeFieldNames = map fieldName executableFieldDescrs
--                 ++ buildInfoNames

buildInfoNames :: [String]
buildInfoNames = map fieldName binfoFieldDescrs
                ++ map fst deprecatedFieldsBuildInfo

-- A minimal implementation of the StateT monad transformer to avoid depending
-- on the 'mtl' package.
newtype StT s m a = StT { runStT :: s -> m (a,s) }

instance Monad m => Monad (StT s m) where
    return a = StT (\s -> return (a,s))
    StT f >>= g = StT $ \s -> do
                        (a,s') <- f s
                        runStT (g a) s'

get :: Monad m => StT s m s
get = StT $ \s -> return (s, s)

modify :: Monad m => (s -> s) -> StT s m ()
modify f = StT $ \s -> return ((),f s)

lift :: Monad m => m a -> StT s m a
lift m = StT $ \s -> m >>= \a -> return (a,s)

evalStT :: Monad m => StT s m a -> s -> m a
evalStT st s = runStT st s >>= return . fst

-- Our monad for parsing a list/tree of fields.
--
-- The state represents the remaining fields to be processed.
type PM a = StT [Field] ParseResult a



-- return look-ahead field or nothing if we're at the end of the file
peekField :: PM (Maybe Field)
peekField = get >>= return . listToMaybe

-- Unconditionally discard the first field in our state.  Will error when it
-- reaches end of file.  (Yes, that's evil.)
skipField :: PM ()
skipField = modify tail

--FIXME: this should take a ByteString, not a String. We have to be able to
-- decode UTF8 and handle the BOM.

-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.
parsePackageDescription :: String -> ParseResult GenericPackageDescription
parsePackageDescription file = do

    -- This function is quite complex because it needs to be able to parse
    -- both pre-Cabal-1.2 and post-Cabal-1.2 files.  Additionally, it contains
    -- a lot of parser-related noise since we do not want to depend on Parsec.
    --
    -- If we detect an pre-1.2 file we implicitly convert it to post-1.2
    -- style.  See 'sectionizeFields' below for details about the conversion.

    fields0 <- readFields file `catchParseError` \err ->
                 let tabs = findIndentTabs file in
                 case err of
                   -- In case of a TabsError report them all at once.
                   TabsError tabLineNo -> reportTabsError
                   -- but only report the ones including and following
                   -- the one that caused the actual error
                                            [ t | t@(lineNo',_) <- tabs
                                                , lineNo' >= tabLineNo ]
                   _ -> parseFail err

    let cabalVersionNeeded =
          head $ [ minVersionBound versionRange
                 | Just versionRange <- [ simpleParse v
                                        | F _ "cabal-version" v <- fields0 ] ]
              ++ [Version [0] []]
        minVersionBound versionRange =
          case asVersionIntervals versionRange of
            []                            -> Version [0] []
            ((LowerBound version _, _):_) -> version

    handleFutureVersionParseFailure cabalVersionNeeded $ do

      let sf = sectionizeFields fields0  -- ensure 1.2 format

        -- figure out and warn about deprecated stuff (warnings are collected
        -- inside our parsing monad)
      fields <- mapSimpleFields deprecField sf

        -- Our parsing monad takes the not-yet-parsed fields as its state.
        -- After each successful parse we remove the field from the state
        -- ('skipField') and move on to the next one.
        --
        -- Things are complicated a bit, because fields take a tree-like
        -- structure -- they can be sections or "if"/"else" conditionals.

      flip evalStT fields $ do

          -- The header consists of all simple fields up to the first section
          -- (flag, library, executable).
        header_fields <- getHeader []

          -- Parses just the header fields and stores them in a
          -- 'PackageDescription'.  Note that our final result is a
          -- 'GenericPackageDescription'; for pragmatic reasons we just store
          -- the partially filled-out 'PackageDescription' inside the
          -- 'GenericPackageDescription'.
        pkg <- lift $ parseFields pkgDescrFieldDescrs
                                  storeXFieldsPD
                                  emptyPackageDescription
                                  header_fields

          -- 'getBody' assumes that the remaining fields only consist of
          -- flags, lib and exe sections.
        (repos, flags, mlib, exes, tests) <- getBody
        warnIfRest  -- warn if getBody did not parse up to the last field.
          -- warn about using old/new syntax with wrong cabal-version:
        maybeWarnCabalVersion (not $ oldSyntax fields0) pkg
        checkForUndefinedFlags flags mlib exes tests
        return $ GenericPackageDescription
                   pkg { sourceRepos = repos }
                   flags mlib exes tests

  where
    oldSyntax flds = all isSimpleField flds
    reportTabsError tabs =
        syntaxError (fst (head tabs)) $
          "Do not use tabs for indentation (use spaces instead)\n"
          ++ "  Tabs were used at (line,column): " ++ show tabs

    maybeWarnCabalVersion newsyntax pkg
      | newsyntax && specVersion pkg < Version [1,2] []
      = lift $ warning $
             "A package using section syntax must specify at least\n"
          ++ "'cabal-version: >= 1.2'."

    maybeWarnCabalVersion newsyntax pkg
      | not newsyntax && specVersion pkg >= Version [1,2] []
      = lift $ warning $
             "A package using 'cabal-version: "
          ++ displaySpecVersion (specVersionRaw pkg)
          ++ "' must use section syntax. See the Cabal user guide for details."
      where
        displaySpecVersion (Left version)       = display version
        displaySpecVersion (Right versionRange) =
          case asVersionIntervals versionRange of
            [] {- impossible -}           -> display versionRange
            ((LowerBound version _, _):_) -> display (orLaterVersion version)

    maybeWarnCabalVersion _ _ = return ()


    handleFutureVersionParseFailure cabalVersionNeeded parseBody =
      (unless versionOk (warning message) >> parseBody)
        `catchParseError` \parseError -> case parseError of
        TabsError _   -> parseFail parseError
        _ | versionOk -> parseFail parseError
          | otherwise -> fail message
      where versionOk = cabalVersionNeeded <= cabalVersion
            message   = "This package requires at least Cabal version "
                     ++ display cabalVersionNeeded

    -- "Sectionize" an old-style Cabal file.  A sectionized file has:
    --
    --  * all global fields at the beginning, followed by
    --
    --  * all flag declarations, followed by
    --
    --  * an optional library section, and an arbitrary number of executable
    --    sections (in any order).
    --
    -- The current implementatition just gathers all library-specific fields
    -- in a library section and wraps all executable stanzas in an executable
    -- section.
    sectionizeFields :: [Field] -> [Field]
    sectionizeFields fs
      | oldSyntax fs =
          let
            -- "build-depends" is a local field now.  To be backwards
            -- compatible, we still allow it as a global field in old-style
            -- package description files and translate it to a local field by
            -- adding it to every non-empty section
            (hdr0, exes0) = break ((=="executable") . fName) fs
            (hdr, libfs0) = partition (not . (`elem` libFieldNames) . fName) hdr0

            (deps, libfs) = partition ((== "build-depends") . fName)
                                       libfs0

            exes = unfoldr toExe exes0
            toExe [] = Nothing
            toExe (F l e n : r)
              | e == "executable" =
                  let (efs, r') = break ((=="executable") . fName) r
                  in Just (Section l "executable" n (deps ++ efs), r')
            toExe _ = bug "unexpeced input to 'toExe'"
          in
            hdr ++
           (if null libfs then []
            else [Section (lineNo (head libfs)) "library" "" (deps ++ libfs)])
            ++ exes
      | otherwise = fs

    isSimpleField (F _ _ _) = True
    isSimpleField _ = False

    -- warn if there's something at the end of the file
    warnIfRest :: PM ()
    warnIfRest = do
      s <- get
      case s of
        [] -> return ()
        _ -> lift $ warning "Ignoring trailing declarations."  -- add line no.

    -- all simple fields at the beginning of the file are (considered) header
    -- fields
    getHeader :: [Field] -> PM [Field]
    getHeader acc = peekField >>= \mf -> case mf of
        Just f@(F _ _ _) -> skipField >> getHeader (f:acc)
        _ -> return (reverse acc)

    --
    -- body ::= { repo | flag | library | executable | test }+   -- at most one lib
    --
    -- The body consists of an optional sequence of declarations of flags and
    -- an arbitrary number of executables and at most one library.
    getBody :: PM ([SourceRepo], [Flag]
                  ,Maybe (CondTree ConfVar [Dependency] Library)
                  ,[(String, CondTree ConfVar [Dependency] Executable)]
                  ,[(String, CondTree ConfVar [Dependency] TestSuite)])
    getBody = peekField >>= \mf -> case mf of
      Just (Section line_no sec_type sec_label sec_fields)
        | sec_type == "executable" -> do
            when (null sec_label) $ lift $ syntaxError line_no
              "'executable' needs one argument (the executable's name)"
            exename <- lift $ runP line_no "executable" parseTokenQ sec_label
            flds <- collectFields parseExeFields sec_fields
            skipField
            (repos, flags, lib, exes, tests) <- getBody
            return (repos, flags, lib, (exename, flds): exes, tests)

        | sec_type == "test-suite" -> do
            when (null sec_label) $ lift $ syntaxError line_no
                "'test-suite' needs one argument (the test suite's name)"
            testname <- lift $ runP line_no "test" parseTokenQ sec_label
            flds <- collectFields (parseTestFields line_no) sec_fields

            -- Check that a valid test suite type has been chosen. A type
            -- field may be given inside a conditional block, so we must
            -- check for that before complaining that a type field has not
            -- been given. The test suite must always have a valid type, so
            -- we need to check both the 'then' and 'else' blocks, though
            -- the blocks need not have the same type.
            let checkTestType ts ct =
                    let ts' = mappend ts $ condTreeData ct
                        -- If a conditional has only a 'then' block and no
                        -- 'else' block, then it cannot have a valid type
                        -- in every branch, unless the type is specified at
                        -- a higher level in the tree.
                        checkComponent (_, _, Nothing) = False
                        -- If a conditional has a 'then' block and an 'else'
                        -- block, both must specify a test type, unless the
                        -- type is specified higher in the tree.
                        checkComponent (_, t, Just e) =
                            checkTestType ts' t && checkTestType ts' e
                        -- Does the current node specify a test type?
                        hasTestType = testInterface ts'
                            /= testInterface emptyTestSuite
                        components = condTreeComponents ct
                    -- If the current level of the tree specifies a type,
                    -- then we are done. If not, then one of the conditional
                    -- branches below the current node must specify a type.
                    -- Each node may have multiple immediate children; we
                    -- only one need one to specify a type because the
                    -- configure step uses 'mappend' to join together the
                    -- results of flag resolution.
                    in hasTestType || (any checkComponent components)
            if checkTestType emptyTestSuite flds
                then do
                    skipField
                    (repos, flags, lib, exes, tests) <- getBody
                    return (repos, flags, lib, exes, (testname, flds) : tests)
                else lift $ syntaxError line_no $
                         "Test suite \"" ++ testname
                      ++ "\" is missing required field \"type\" or the field "
                      ++ "is not present in all conditional branches. The "
                      ++ "available test types are: "
                      ++ intercalate ", " (map display knownTestTypes)

        | sec_type == "library" -> do
            when (not (null sec_label)) $ lift $
              syntaxError line_no "'library' expects no argument"
            flds <- collectFields parseLibFields sec_fields
            skipField
            (repos, flags, lib, exes, tests) <- getBody
            when (isJust lib) $ lift $ syntaxError line_no
              "There can only be one library section in a package description."
            return (repos, flags, Just flds, exes, tests)

        | sec_type == "flag" -> do
            when (null sec_label) $ lift $
              syntaxError line_no "'flag' needs one argument (the flag's name)"
            flag <- lift $ parseFields
                    flagFieldDescrs
                    warnUnrec
                    (MkFlag (FlagName (lowercase sec_label)) "" True False)
                    sec_fields
            skipField
            (repos, flags, lib, exes, tests) <- getBody
            return (repos, flag:flags, lib, exes, tests)

        | sec_type == "source-repository" -> do
            when (null sec_label) $ lift $ syntaxError line_no $
                 "'source-repository' needs one argument, "
              ++ "the repo kind which is usually 'head' or 'this'"
            kind <- case simpleParse sec_label of
              Just kind -> return kind
              Nothing   -> lift $ syntaxError line_no $
                             "could not parse repo kind: " ++ sec_label
            repo <- lift $ parseFields
                    sourceRepoFieldDescrs
                    warnUnrec
                    (SourceRepo {
                      repoKind     = kind,
                      repoType     = Nothing,
                      repoLocation = Nothing,
                      repoModule   = Nothing,
                      repoBranch   = Nothing,
                      repoTag      = Nothing,
                      repoSubdir   = Nothing
                    })
                    sec_fields
            skipField
            (repos, flags, lib, exes, tests) <- getBody
            return (repo:repos, flags, lib, exes, tests)

        | otherwise -> do
            lift $ warning $ "Ignoring unknown section type: " ++ sec_type
            skipField
            getBody
      Just f -> do
            _ <- lift $ syntaxError (lineNo f) $
              "Construct not supported at this position: " ++ show f
            skipField
            getBody
      Nothing -> return ([], [], Nothing, [], [])

    -- Extracts all fields in a block and returns a 'CondTree'.
    --
    -- We have to recurse down into conditionals and we treat fields that
    -- describe dependencies specially.
    collectFields :: ([Field] -> PM a) -> [Field]
                  -> PM (CondTree ConfVar [Dependency] a)
    collectFields parser allflds = do

        let simplFlds = [ F l n v | F l n v <- allflds ]
            condFlds = [ f | f@(IfBlock _ _ _ _) <- allflds ]

        let (depFlds, dataFlds) = partition isConstraint simplFlds

        a <- parser dataFlds
        deps <- liftM concat . mapM (lift . parseConstraint) $ depFlds

        ifs <- mapM processIfs condFlds

        return (CondNode a deps ifs)
      where
        isConstraint (F _ n _) = n `elem` constraintFieldNames
        isConstraint _ = False

        processIfs (IfBlock l c t e) = do
            cnd <- lift $ runP l "if" parseCondition c
            t' <- collectFields parser t
            e' <- case e of
                   [] -> return Nothing
                   es -> do fs <- collectFields parser es
                            return (Just fs)
            return (cnd, t', e')
        processIfs _ = bug "processIfs called with wrong field type"

    parseLibFields :: [Field] -> PM Library
    parseLibFields = lift . parseFields libFieldDescrs storeXFieldsLib emptyLibrary

    -- Note: we don't parse the "executable" field here, hence the tail hack.
    parseExeFields :: [Field] -> PM Executable
    parseExeFields = lift . parseFields (tail executableFieldDescrs) storeXFieldsExe emptyExecutable

    parseTestFields :: LineNo -> [Field] -> PM TestSuite
    parseTestFields line fields = do
        x <- lift $ parseFields testSuiteFieldDescrs storeXFieldsTest
                                emptyTestStanza fields
        lift $ validateTestSuite line x

    checkForUndefinedFlags ::
        [Flag] ->
        Maybe (CondTree ConfVar [Dependency] Library) ->
        [(String, CondTree ConfVar [Dependency] Executable)] ->
        [(String, CondTree ConfVar [Dependency] TestSuite)] ->
        PM ()
    checkForUndefinedFlags flags mlib exes tests = do
        let definedFlags = map flagName flags
        maybe (return ()) (checkCondTreeFlags definedFlags) mlib
        mapM_ (checkCondTreeFlags definedFlags . snd) exes
        mapM_ (checkCondTreeFlags definedFlags . snd) tests

    checkCondTreeFlags :: [FlagName] -> CondTree ConfVar c a -> PM ()
    checkCondTreeFlags definedFlags ct = do
        let fv = nub $ freeVars ct
        when (not . all (`elem` definedFlags) $ fv) $
            fail $ "These flags are used without having been defined: "
                ++ intercalate ", " [ n | FlagName n <- fv \\ definedFlags ]


-- | Parse a list of fields, given a list of field descriptions,
--   a structure to accumulate the parsed fields, and a function
--   that can decide what to do with fields which don't match any
--   of the field descriptions.
parseFields :: [FieldDescr a]      -- ^ descriptions of fields we know how to
                                   --   parse
            -> UnrecFieldParser a  -- ^ possibly do something with
                                   --   unrecognized fields
            -> a                   -- ^ accumulator
            -> [Field]             -- ^ fields to be parsed
            -> ParseResult a
parseFields descrs unrec ini fields =
    do (a, unknowns) <- foldM (parseField descrs unrec) (ini, []) fields
       when (not (null unknowns)) $ do
         warning $ render $
           text "Unknown fields:" <+>
                commaSep (map (\(l,u) -> u ++ " (line " ++ show l ++ ")")
                              (reverse unknowns))
           $+$
           text "Fields allowed in this section:" $$
             nest 4 (commaSep $ map fieldName descrs)
       return a
  where
    commaSep = fsep . punctuate comma . map text

parseField :: [FieldDescr a]     -- ^ list of parseable fields
           -> UnrecFieldParser a -- ^ possibly do something with
                                 --   unrecognized fields
           -> (a,[(Int,String)]) -- ^ accumulated result and warnings
           -> Field              -- ^ the field to be parsed
           -> ParseResult (a, [(Int,String)])
parseField ((FieldDescr name _ parser):fields) unrec (a, us) (F line f val)
  | name == f = parser line val a >>= \a' -> return (a',us)
  | otherwise = parseField fields unrec (a,us) (F line f val)
parseField [] unrec (a,us) (F l f val) = return $
  case unrec (f,val) a of        -- no fields matched, see if the 'unrec'
    Just a' -> (a',us)           -- function wants to do anything with it
    Nothing -> (a, ((l,f):us))
parseField _ _ _ _ = bug "'parseField' called on a non-field"

deprecatedFields :: [(String,String)]
deprecatedFields =
    deprecatedFieldsPkgDescr ++ deprecatedFieldsBuildInfo

deprecatedFieldsPkgDescr :: [(String,String)]
deprecatedFieldsPkgDescr = [ ("other-files", "extra-source-files") ]

deprecatedFieldsBuildInfo :: [(String,String)]
deprecatedFieldsBuildInfo = [ ("hs-source-dir","hs-source-dirs") ]

-- Handle deprecated fields
deprecField :: Field -> ParseResult Field
deprecField (F line fld val) = do
  fld' <- case lookup fld deprecatedFields of
            Nothing -> return fld
            Just newName -> do
              warning $ "The field \"" ++ fld
                      ++ "\" is deprecated, please use \"" ++ newName ++ "\""
              return newName
  return (F line fld' val)
deprecField _ = bug "'deprecField' called on a non-field"


parseHookedBuildInfo :: String -> ParseResult HookedBuildInfo
parseHookedBuildInfo inp = do
  fields <- readFields inp
  let ss@(mLibFields:exes) = stanzas fields
  mLib <- parseLib mLibFields
  biExes <- mapM parseExe (maybe ss (const exes) mLib)
  return (mLib, biExes)
  where
    parseLib :: [Field] -> ParseResult (Maybe BuildInfo)
    parseLib (bi@((F _ inFieldName _):_))
        | lowercase inFieldName /= "executable" = liftM Just (parseBI bi)
    parseLib _ = return Nothing

    parseExe :: [Field] -> ParseResult (String, BuildInfo)
    parseExe ((F line inFieldName mName):bi)
        | lowercase inFieldName == "executable"
            = do bis <- parseBI bi
                 return (mName, bis)
        | otherwise = syntaxError line "expecting 'executable' at top of stanza"
    parseExe (_:_) = bug "`parseExe' called on a non-field"
    parseExe [] = syntaxError 0 "error in parsing buildinfo file. Expected executable stanza"

    parseBI st = parseFields binfoFieldDescrs storeXFieldsBI emptyBuildInfo st

-- ---------------------------------------------------------------------------
-- Pretty printing

writePackageDescription :: FilePath -> PackageDescription -> IO ()
writePackageDescription fpath pkg = writeUTF8File fpath (showPackageDescription pkg)

--TODO: make this use section syntax
-- add equivalent for GenericPackageDescription
showPackageDescription :: PackageDescription -> String
showPackageDescription pkg = render $
     ppPackage pkg
  $$ ppCustomFields (customFieldsPD pkg)
  $$ (case library pkg of
        Nothing  -> empty
        Just lib -> ppLibrary lib)
  $$ vcat [ space $$ ppExecutable exe | exe <- executables pkg ]
  where
    ppPackage    = ppFields pkgDescrFieldDescrs
    ppLibrary    = ppFields libFieldDescrs
    ppExecutable = ppFields executableFieldDescrs

ppCustomFields :: [(String,String)] -> Doc
ppCustomFields flds = vcat (map ppCustomField flds)

ppCustomField :: (String,String) -> Doc
ppCustomField (name,val) = text name <> colon <+> showFreeText val

writeHookedBuildInfo :: FilePath -> HookedBuildInfo -> IO ()
writeHookedBuildInfo fpath = writeFileAtomic fpath . showHookedBuildInfo

showHookedBuildInfo :: HookedBuildInfo -> String
showHookedBuildInfo (mb_lib_bi, ex_bis) = render $
     (case mb_lib_bi of
        Nothing -> empty
        Just bi -> ppBuildInfo bi)
  $$ vcat [    space
            $$ text "executable:" <+> text name
            $$ ppBuildInfo bi
          | (name, bi) <- ex_bis ]
  where
    ppBuildInfo bi = ppFields binfoFieldDescrs bi
                  $$ ppCustomFields (customFieldsBI bi)

-- replace all tabs used as indentation with whitespace, also return where
-- tabs were found
findIndentTabs :: String -> [(Int,Int)]
findIndentTabs = concatMap checkLine
               . zip [1..]
               . lines
    where
      checkLine (lineno, l) =
          let (indent, _content) = span isSpace l
              tabCols = map fst . filter ((== '\t') . snd) . zip [0..]
              addLineNo = map (\col -> (lineno,col))
          in addLineNo (tabCols indent)

--test_findIndentTabs = findIndentTabs $ unlines $
--    [ "foo", "  bar", " \t baz", "\t  biz\t", "\t\t \t mib" ]

bug :: String -> a
bug msg = error $ msg ++ ". Consider this a bug."
