{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.Parsec
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defined parsers and partial pretty printers for the @.cabal@ format.

module Distribution.PackageDescription.Parsec (
    -- * Package descriptions
    readGenericPackageDescription,
    parseGenericPackageDescription,
    parseGenericPackageDescriptionMaybe,

    -- ** Parsing
    ParseResult,
    runParseResult,

    -- * New-style spec-version
    scanSpecVersion,

    -- ** Supplementary build information
    readHookedBuildInfo,
    parseHookedBuildInfo,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad                                (guard)
import Control.Monad.State.Strict                   (StateT, execStateT)
import Control.Monad.Trans.Class                    (lift)
import Data.List                                    (partition)
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Parsec             (NamelessField (..))
import Distribution.PackageDescription
import Distribution.PackageDescription.FieldGrammar
import Distribution.PackageDescription.Quirks       (patchQuirks)
import Distribution.Parsec.Class                    (parsec, simpleParsec)
import Distribution.Parsec.Common
import Distribution.Parsec.ConfVar                  (parseConditionConfVar)
import Distribution.Parsec.Field                    (FieldName, getName)
import Distribution.Parsec.FieldLineStream          (fieldLineStreamFromBS)
import Distribution.Parsec.LexerMonad               (LexWarning, toPWarnings)
import Distribution.Parsec.Newtypes                 (CommaFSep, List, SpecVersion (..), Token)
import Distribution.Parsec.Parser
import Distribution.Parsec.ParseResult
import Distribution.Pretty                          (prettyShow)
import Distribution.Simple.Utils                    (die', fromUTF8BS, warn)
import Distribution.Text                            (display)
import Distribution.Types.CondTree
import Distribution.Types.Dependency                (Dependency)
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType            (knownForeignLibTypes)
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)
import Distribution.Types.PackageDescription        (specVersion')
import Distribution.Types.UnqualComponentName       (UnqualComponentName, mkUnqualComponentName)
import Distribution.Utils.Generic                   (breakMaybe, unfoldrM, validateUTF8)
import Distribution.Verbosity                       (Verbosity)
import Distribution.Version
       (LowerBound (..), Version, asVersionIntervals, mkVersion, orLaterVersion, version0,
       versionNumbers)
import System.Directory                             (doesFileExist)

import qualified Data.ByteString                                   as BS
import qualified Data.ByteString.Char8                             as BS8
import qualified Distribution.Compat.Map.Strict                    as Map
import qualified Distribution.Compat.Newtype                       as Newtype
import qualified Distribution.Types.BuildInfo.Lens                 as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens        as L
import qualified Text.Parsec                                       as P

-- ---------------------------------------------------------------
-- Parsing

-- | Helper combinator to do parsing plumbing for files.
--
-- Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
--
-- Argument order is chosen to encourage partial application.
readAndParseFile
    :: (BS.ByteString -> ParseResult a)  -- ^ File contents to final value parser
    -> Verbosity                         -- ^ Verbosity level
    -> FilePath                          -- ^ File to read
    -> IO a
readAndParseFile parser verbosity fpath = do
    exists <- doesFileExist fpath
    unless exists $
      die' verbosity $
        "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
    bs <- BS.readFile fpath
    let (warnings, result) = runParseResult (parser bs)
    traverse_ (warn verbosity . showPWarning fpath) warnings
    case result of
        Right x -> return x
        Left (_, errors) -> do
            traverse_ (warn verbosity . showPError fpath) errors
            die' verbosity $ "Failed parsing \"" ++ fpath ++ "\"."

-- | Parse the given package file.
readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readAndParseFile parseGenericPackageDescription

------------------------------------------------------------------------------
-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.
--
parseGenericPackageDescription :: BS.ByteString -> ParseResult GenericPackageDescription
parseGenericPackageDescription bs = do
    -- set scanned version
    setCabalSpecVersion ver
    -- if we get too new version, fail right away
    case ver of
        Just v | v > mkVersion [2,2] -> parseFailure zeroPos
            "Unsupported cabal-version. See https://github.com/haskell/cabal/issues/4899."
        _ -> pure ()

    case readFields' bs' of
        Right (fs, lexWarnings) -> do
            when patched $
                parseWarning zeroPos PWTQuirkyCabalFile "Legacy cabal file"
            -- UTF8 is validated in a prepass step, afterwards parsing is lenient.
            parseGenericPackageDescription' ver lexWarnings (validateUTF8 bs') fs
        -- TODO: better marshalling of errors
        Left perr -> parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = Position (P.sourceLine ppos) (P.sourceColumn ppos)
  where
    (patched, bs') = patchQuirks bs
    ver = scanSpecVersion bs'

-- | 'Maybe' variant of 'parseGenericPackageDescription'
parseGenericPackageDescriptionMaybe :: BS.ByteString -> Maybe GenericPackageDescription
parseGenericPackageDescriptionMaybe =
    either (const Nothing) Just . snd . runParseResult . parseGenericPackageDescription

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)

-- Monad in which sections are parsed
type SectionParser = StateT SectionS ParseResult

-- | State of section parser
data SectionS = SectionS
    { _stateGpd           :: !GenericPackageDescription
    , _stateCommonStanzas :: !(Map String CondTreeBuildInfo)
    }

stateGpd :: Lens' SectionS GenericPackageDescription
stateGpd f (SectionS gpd cs) = (\x -> SectionS x cs) <$> f gpd
{-# INLINE stateGpd #-}

stateCommonStanzas :: Lens' SectionS (Map String CondTreeBuildInfo)
stateCommonStanzas f (SectionS gpd cs) = SectionS gpd <$> f cs
{-# INLINE stateCommonStanzas #-}

-- Note [Accumulating parser]
--
-- This parser has two "states":
-- * first we parse fields of PackageDescription
-- * then we parse sections (libraries, executables, etc)
parseGenericPackageDescription'
    :: Maybe Version
    -> [LexWarning]
    -> Maybe Int
    -> [Field Position]
    -> ParseResult GenericPackageDescription
parseGenericPackageDescription' cabalVerM lexWarnings utf8WarnPos fs = do
    parseWarnings (toPWarnings lexWarnings)
    for_ utf8WarnPos $ \pos ->
        parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
    let (syntax, fs') = sectionizeFields fs
    let (fields, sectionFields) = takeFields fs'

    -- cabal-version
    cabalVer <- case cabalVerM of
        Just v  -> return v
        Nothing -> case Map.lookup "cabal-version" fields >>= safeLast of
            Nothing                        -> return version0
            Just (MkNamelessField pos fls) -> do
                v <- specVersion' . Newtype.unpack' SpecVersion <$> runFieldParser pos parsec cabalSpecLatest fls
                when (v >= mkVersion [2,1]) $ parseFailure pos $
                    "cabal-version should be at the beginning of the file starting with spec version 2.2. " ++
                    "See https://github.com/haskell/cabal/issues/4899"

                return v

    let specVer
          | cabalVer >= mkVersion [2,1]  = CabalSpecV2_2
          | cabalVer >= mkVersion [1,25] = CabalSpecV2_0
          | cabalVer >= mkVersion [1,23] = CabalSpecV1_24
          | otherwise = CabalSpecOld

    -- reset cabal version
    setCabalSpecVersion (Just cabalVer)

    -- Package description
    pd <- parseFieldGrammar specVer fields packageDescriptionFieldGrammar

    -- Check that scanned and parsed versions match.
    unless (cabalVer == specVersion pd) $ parseFailure zeroPos $
        "Scanned and parsed cabal-versions don't match " ++
        prettyShow cabalVer ++ " /= " ++ prettyShow (specVersion pd)

    maybeWarnCabalVersion syntax pd

    -- Sections
    let gpd = emptyGenericPackageDescription & L.packageDescription .~ pd

    view stateGpd <$> execStateT (goSections specVer sectionFields) (SectionS gpd Map.empty)
  where
    safeLast :: [a] -> Maybe a
    safeLast = listToMaybe . reverse

    newSyntaxVersion :: Version
    newSyntaxVersion = mkVersion [1, 2]

    maybeWarnCabalVersion :: Syntax -> PackageDescription -> ParseResult ()
    maybeWarnCabalVersion syntax pkg
      | syntax == NewSyntax && specVersion pkg < newSyntaxVersion
      = parseWarning zeroPos PWTNewSyntax $
             "A package using section syntax must specify at least\n"
          ++ "'cabal-version: >= 1.2'."

    maybeWarnCabalVersion syntax pkg
      | syntax == OldSyntax && specVersion pkg >= newSyntaxVersion
      = parseWarning zeroPos PWTOldSyntax $
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

goSections :: CabalSpecVersion -> [Field Position] -> SectionParser ()
goSections specVer = traverse_ process
  where
    process (Field (Name pos name) _) =
        lift $ parseWarning pos PWTTrailingFields $
            "Ignoring trailing fields after sections: " ++ show name
    process (Section name args secFields) =
        parseSection name args secFields

    snoc x xs = xs ++ [x]

    hasCommonStanzas = specHasCommonStanzas specVer

    -- we need signature, because this is polymorphic, but not-closed
    parseCondTree'
        :: FromBuildInfo a
        => ParsecFieldGrammar' a       -- ^ grammar
        -> Map String CondTreeBuildInfo  -- ^ common stanzas
        -> [Field Position]
        -> ParseResult (CondTree ConfVar [Dependency] a)
    parseCondTree' = parseCondTreeWithCommonStanzas specVer

    parseSection :: Name Position -> [SectionArg Position] -> [Field Position] -> SectionParser ()
    parseSection (Name pos name) args fields
        | hasCommonStanzas == NoCommonStanzas, name == "common" = lift $ do
          parseWarning pos PWTUnknownSection $ "Ignoring section: common. You should set cabal-version: 2.2 or larger to use common stanzas."

        | name == "common" = do
            commonStanzas <- use stateCommonStanzas
            name' <- lift $ parseCommonName pos args
            biTree <- lift $ parseCondTree' buildInfoFieldGrammar commonStanzas fields

            case Map.lookup name' commonStanzas of
                Nothing -> stateCommonStanzas .= Map.insert name' biTree commonStanzas
                Just _  -> lift $ parseFailure pos $
                    "Duplicate common stanza: " ++ name'

        | name == "library" && null args = do
            commonStanzas <- use stateCommonStanzas
            lib <- lift $ parseCondTree' (libraryFieldGrammar Nothing) commonStanzas fields
            -- TODO: check that library is defined once
            stateGpd . L.condLibrary ?= lib

        -- Sublibraries
        -- TODO: check cabal-version
        | name == "library" = do
            commonStanzas <- use stateCommonStanzas
            name' <- parseUnqualComponentName pos args
            lib   <- lift $ parseCondTree' (libraryFieldGrammar $ Just name') commonStanzas fields
            -- TODO check duplicate name here?
            stateGpd . L.condSubLibraries %= snoc (name', lib)

        -- TODO: check cabal-version
        | name == "foreign-library" = do
            commonStanzas <- use stateCommonStanzas
            name' <- parseUnqualComponentName pos args
            flib  <- lift $ parseCondTree' (foreignLibFieldGrammar name')  commonStanzas fields

            let hasType ts = foreignLibType ts /= foreignLibType mempty
            unless (onAllBranches hasType flib) $ lift $ parseFailure pos $ concat
                [ "Foreign library " ++ show (display name')
                , " is missing required field \"type\" or the field "
                , "is not present in all conditional branches. The "
                , "available test types are: "
                , intercalate ", " (map display knownForeignLibTypes)
                ]

            -- TODO check duplicate name here?
            stateGpd . L.condForeignLibs %= snoc (name', flib)

        | name == "executable" = do
            commonStanzas <- use stateCommonStanzas
            name' <- parseUnqualComponentName pos args
            exe   <- lift $ parseCondTree' (executableFieldGrammar name') commonStanzas fields
            -- TODO check duplicate name here?
            stateGpd . L.condExecutables %= snoc (name', exe)

        | name == "test-suite" = do
            commonStanzas <- use stateCommonStanzas
            name'      <- parseUnqualComponentName pos args
            testStanza <- lift $ parseCondTree' testSuiteFieldGrammar commonStanzas fields
            testSuite  <- lift $ traverse (validateTestSuite pos) testStanza

            let hasType ts = testInterface ts /= testInterface mempty
            unless (onAllBranches hasType testSuite) $ lift $ parseFailure pos $ concat
                [ "Test suite " ++ show (display name')
                , " is missing required field \"type\" or the field "
                , "is not present in all conditional branches. The "
                , "available test types are: "
                , intercalate ", " (map display knownTestTypes)
                ]

            -- TODO check duplicate name here?
            stateGpd . L.condTestSuites %= snoc (name', testSuite)

        | name == "benchmark" = do
            commonStanzas <- use stateCommonStanzas
            name'       <- parseUnqualComponentName pos args
            benchStanza <- lift $ parseCondTree' benchmarkFieldGrammar commonStanzas fields
            bench       <- lift $ traverse (validateBenchmark pos) benchStanza

            let hasType ts = benchmarkInterface ts /= benchmarkInterface mempty
            unless (onAllBranches hasType bench) $ lift $ parseFailure pos $ concat
                [ "Benchmark " ++ show (display name')
                , " is missing required field \"type\" or the field "
                , "is not present in all conditional branches. The "
                , "available benchmark types are: "
                , intercalate ", " (map display knownBenchmarkTypes)
                ]

            -- TODO check duplicate name here?
            stateGpd . L.condBenchmarks %= snoc (name', bench)

        | name == "flag" = do
            name'  <- parseNameBS pos args
            name'' <- lift $ runFieldParser' pos parsec specVer (fieldLineStreamFromBS name') `recoverWith` mkFlagName ""
            flag   <- lift $ parseFields specVer fields (flagFieldGrammar name'')
            -- Check default flag
            stateGpd . L.genPackageFlags %= snoc flag

        | name == "custom-setup" && null args = do
            sbi <- lift $ parseFields specVer fields  (setupBInfoFieldGrammar False)
            stateGpd . L.packageDescription . L.setupBuildInfo ?= sbi

        | name == "source-repository" = do
            kind <- lift $ case args of
                [SecArgName spos secName] ->
                    runFieldParser' spos parsec specVer (fieldLineStreamFromBS secName) `recoverWith` RepoHead
                [] -> do
                    parseFailure pos "'source-repository' requires exactly one argument"
                    pure RepoHead
                _ -> do
                    parseFailure pos $ "Invalid source-repository kind " ++ show args
                    pure RepoHead

            sr <- lift $ parseFields specVer fields (sourceRepoFieldGrammar kind)
            stateGpd . L.packageDescription . L.sourceRepos %= snoc sr

        | otherwise = lift $
            parseWarning pos PWTUnknownSection $ "Ignoring section: " ++ show name

parseName :: Position -> [SectionArg Position] -> SectionParser String
parseName pos args = fromUTF8BS <$> parseNameBS pos args

parseNameBS :: Position -> [SectionArg Position] -> SectionParser BS.ByteString
-- TODO: use strict parser
parseNameBS pos args = case args of
    [SecArgName _pos secName] ->
         pure secName
    [SecArgStr _pos secName] ->
         pure secName
    [] -> do
         lift $ parseFailure pos "name required"
         pure ""
    _ -> do
         -- TODO: pretty print args
         lift $ parseFailure pos $ "Invalid name " ++ show args
         pure ""

parseCommonName :: Position -> [SectionArg Position] -> ParseResult String
parseCommonName pos args = case args of
    [SecArgName _pos secName] ->
         pure $ fromUTF8BS secName
    [SecArgStr _pos secName] ->
         pure $ fromUTF8BS secName
    [] -> do
         parseFailure pos $ "name required"
         pure ""
    _ -> do
         -- TODO: pretty print args
         parseFailure pos $ "Invalid name " ++ show args
         pure ""

-- TODO: avoid conversion to 'String'.
parseUnqualComponentName :: Position -> [SectionArg Position] -> SectionParser UnqualComponentName
parseUnqualComponentName pos args = mkUnqualComponentName <$> parseName pos args

-- | Parse a non-recursive list of fields.
parseFields
    :: CabalSpecVersion
    -> [Field Position] -- ^ fields to be parsed
    -> ParsecFieldGrammar' a
    -> ParseResult a
parseFields v fields grammar = do
    let (fs0, ss) = partitionFields fields
    traverse_ (traverse_ warnInvalidSubsection) ss
    parseFieldGrammar v fs0 grammar

warnInvalidSubsection :: Section Position -> ParseResult ()
warnInvalidSubsection (MkSection (Name pos name) _ _) =
    void (parseFailure pos $ "invalid subsection " ++ show name)

parseCondTree
    :: forall a c.
       CabalSpecVersion
    -> HasElif                  -- ^ accept @elif@
    -> ParsecFieldGrammar' a  -- ^ grammar
    -> (a -> c)                 -- ^ condition extractor
    -> [Field Position]
    -> ParseResult (CondTree ConfVar c a)
parseCondTree v hasElif grammar cond = go
  where
    go fields = do
        let (fs, ss) = partitionFields fields
        x <- parseFieldGrammar v fs grammar
        branches <- concat <$> traverse parseIfs ss
        return (CondNode x (cond x) branches) -- TODO: branches

    parseIfs :: [Section Position] -> ParseResult [CondBranch ConfVar c a]
    parseIfs [] = return []
    parseIfs (MkSection (Name _ name) test fields : sections) | name == "if" = do
        test' <- parseConditionConfVar test
        fields' <- go fields
        -- TODO: else
        (elseFields, sections') <- parseElseIfs sections
        return (CondBranch test' fields' elseFields : sections')
    parseIfs (MkSection (Name pos name) _ _ : sections) = do
        parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name
        parseIfs sections

    parseElseIfs
        :: [Section Position]
        -> ParseResult (Maybe (CondTree ConfVar c a), [CondBranch ConfVar c a])
    parseElseIfs [] = return (Nothing, [])
    parseElseIfs (MkSection (Name pos name) args fields : sections) | name == "else" = do
        unless (null args) $
            parseFailure pos $ "`else` section has section arguments " ++ show args
        elseFields <- go fields
        sections' <- parseIfs sections
        return (Just elseFields, sections')



    parseElseIfs (MkSection (Name _ name) test fields : sections) | hasElif == HasElif, name == "elif" = do
        -- TODO: check cabal-version
        test' <- parseConditionConfVar test
        fields' <- go fields
        (elseFields, sections') <- parseElseIfs sections
        -- we parse an empty 'Fields', to get empty value for a node
        a <- parseFieldGrammar v mempty grammar
        return (Just $ CondNode a (cond a) [CondBranch test' fields' elseFields], sections')

    parseElseIfs (MkSection (Name pos name) _ _ : sections) | name == "elif" = do
        parseWarning pos PWTInvalidSubsection $ "invalid subsection \"elif\". You should set cabal-version: 2.2 or larger to use elif-conditionals."
        (,) Nothing <$> parseIfs sections

    parseElseIfs sections = (,) Nothing <$> parseIfs sections

{- Note [Accumulating parser]

Note: Outdated a bit

In there parser, @'FieldDescr' a@ is transformed into @Map FieldName (a ->
FieldParser a)@.  The weird value is used because we accumulate structure of
@a@ by folding over the fields.  There are various reasons for that:

* Almost all fields are optional

* This is simple approach so declarative bi-directional format (parsing and
printing) of structure could be specified (list of @'FieldDescr' a@)

* There are surface syntax fields corresponding to single field in the file:
  @license-file@ and @license-files@

* This is quite safe approach.

When/if we re-implement the parser to support formatting preservging roundtrip
with new AST, this all need to be rewritten.
-}

-------------------------------------------------------------------------------
-- Common stanzas
-------------------------------------------------------------------------------

-- $commonStanzas
--
-- [Note: Common stanzas]
--
-- In Cabal 2.2 we support simple common stanzas:
--
-- * Commons stanzas define 'BuildInfo'
--
-- * import "fields" can only occur at top of other stanzas (think: imports)
--
-- In particular __there aren't__
--
-- * implicit stanzas
--
-- * More specific common stanzas (executable, test-suite).
--
--
-- The approach uses the fact that 'BuildInfo' is a 'Monoid':
--
-- @
-- mergeCommonStanza' :: HasBuildInfo comp => BuildInfo -> comp -> comp
-- mergeCommonStanza' bi = over L.BuildInfo (bi <>)
-- @
--
-- Real 'mergeCommonStanza' is more complicated as we have to deal with
-- conditional trees.
--
-- The approach is simple, and have good properties:
--
-- * Common stanzas are parsed exactly once, even if not-used. Thus we report errors in them.
--
type CondTreeBuildInfo = CondTree ConfVar [Dependency] BuildInfo

-- | Create @a@ from 'BuildInfo'.
--
-- Law: @view buildInfo . fromBuildInfo = id@
class L.HasBuildInfo a => FromBuildInfo a where
    fromBuildInfo :: BuildInfo -> a

instance FromBuildInfo BuildInfo  where fromBuildInfo = id
instance FromBuildInfo Library    where fromBuildInfo bi = set L.buildInfo bi emptyLibrary
instance FromBuildInfo ForeignLib where fromBuildInfo bi = set L.buildInfo bi emptyForeignLib
instance FromBuildInfo Executable where fromBuildInfo bi = set L.buildInfo bi emptyExecutable

instance FromBuildInfo TestSuiteStanza where
    fromBuildInfo = TestSuiteStanza Nothing Nothing Nothing

instance FromBuildInfo BenchmarkStanza where
    fromBuildInfo = BenchmarkStanza Nothing Nothing Nothing

parseCondTreeWithCommonStanzas
    :: forall a. FromBuildInfo a
    => CabalSpecVersion
    -> ParsecFieldGrammar' a       -- ^ grammar
    -> Map String CondTreeBuildInfo  -- ^ common stanzas
    -> [Field Position]
    -> ParseResult (CondTree ConfVar [Dependency] a)
parseCondTreeWithCommonStanzas v grammar commonStanzas = goImports []
  where
    hasElif = specHasElif v
    hasCommonStanzas = specHasCommonStanzas v

    getList' :: List CommaFSep Token String -> [String]
    getList' = Newtype.unpack

    -- parse leading imports
    -- not supported:
    goImports acc (Field (Name pos name) _ : fields) | name == "import", hasCommonStanzas == NoCommonStanzas = do
        parseWarning pos PWTUnknownField "Unknown field: import. You should set cabal-version: 2.2 or larger to use common stanzas"
        goImports acc fields
    -- supported:
    goImports acc (Field (Name pos name) fls : fields) | name == "import" = do
        names <- getList' <$> runFieldParser pos parsec v fls
        names' <- for names $ \commonName ->
            case Map.lookup commonName commonStanzas of
                Nothing -> do
                    parseFailure pos $ "Undefined common stanza imported: " ++ commonName
                    pure Nothing
                Just commonTree ->
                    pure (Just commonTree)

        goImports (acc ++ catMaybes names') fields

    -- Go to parsing condTree after first non-import 'Field'.
    goImports acc fields = go acc fields

    -- parse actual CondTree
    go :: [CondTreeBuildInfo] -> [Field Position] -> ParseResult (CondTree ConfVar [Dependency] a)
    go bis fields = do
        x <- parseCondTree v hasElif grammar (view L.targetBuildDepends) fields
        pure $ foldr mergeCommonStanza x bis

mergeCommonStanza
    :: forall a. FromBuildInfo a
    => CondTree ConfVar [Dependency] BuildInfo
    -> CondTree ConfVar [Dependency] a
    -> CondTree ConfVar [Dependency] a
mergeCommonStanza (CondNode bi _ bis) (CondNode x _ cs) =
    CondNode x' (x' ^. L.targetBuildDepends) cs'
  where
    -- new value is old value with buildInfo field _prepended_.
    x' = x & L.buildInfo %~ (bi <>)

    -- tree components are appended together.
    cs' = map (fmap fromBuildInfo) bis ++ cs

-------------------------------------------------------------------------------
-- Branches
-------------------------------------------------------------------------------

-- Check that a property holds on all branches of a condition tree
onAllBranches :: forall v c a. Monoid a => (a -> Bool) -> CondTree v c a -> Bool
onAllBranches p = go mempty
  where
    -- If the current level of the tree satisfies the property, then we are
    -- done. If not, then one of the conditional branches below the current node
    -- must satisfy it. Each node may have multiple immediate children; we only
    -- one need one to satisfy the property because the configure step uses
    -- 'mappend' to join together the results of flag resolution.
    go :: a -> CondTree v c a -> Bool
    go acc ct = let acc' = acc `mappend` condTreeData ct
                in p acc' || any (goBranch acc') (condTreeComponents ct)

    -- Both the 'true' and the 'false' block must satisfy the property.
    goBranch :: a -> CondBranch v c a -> Bool
    goBranch _   (CondBranch _ _ Nothing) = False
    goBranch acc (CondBranch _ t (Just e))  = go acc t && go acc e

-------------------------------------------------------------------------------
-- Old syntax
-------------------------------------------------------------------------------

-- TODO: move to own module

-- | "Sectionize" an old-style Cabal file.  A sectionized file has:
--
--  * all global fields at the beginning, followed by
--
--  * all flag declarations, followed by
--
--  * an optional library section, and an arbitrary number of executable
--    sections (in any order).
--
-- The current implementation just gathers all library-specific fields
-- in a library section and wraps all executable stanzas in an executable
-- section.
sectionizeFields :: [Field ann] -> (Syntax, [Field ann])
sectionizeFields fs = case classifyFields fs of
    Just fields -> (OldSyntax, convert fields)
    Nothing     -> (NewSyntax, fs)
  where
    -- return 'Just' if all fields are simple fields
    classifyFields :: [Field ann] -> Maybe [(Name ann, [FieldLine ann])]
    classifyFields = traverse f
      where
        f (Field name fieldlines) = Just (name, fieldlines)
        f _                      = Nothing

    trim = BS.dropWhile isSpace' . BS.reverse . BS.dropWhile isSpace' . BS.reverse
    isSpace' = (== 32)

    convert :: [(Name ann, [FieldLine ann])] -> [Field ann]
    convert fields =
      let
        toField (name, ls) = Field name ls
        -- "build-depends" is a local field now.  To be backwards
        -- compatible, we still allow it as a global field in old-style
        -- package description files and translate it to a local field by
        -- adding it to every non-empty section
        (hdr0, exes0) = break ((=="executable") . getName . fst) fields
        (hdr, libfs0) = partition (not . (`elem` libFieldNames) . getName . fst) hdr0

        (deps, libfs) = partition ((== "build-depends") . getName . fst)
                                   libfs0

        exes = unfoldr toExe exes0
        toExe [] = Nothing
        toExe ((Name pos n, ls) : r)
          | n == "executable" =
              let (efs, r') = break ((== "executable") . getName . fst) r
              in Just (Section (Name pos "executable") [SecArgName pos $ trim $ fieldlinesToBS ls] (map toField $ deps ++ efs), r')
        toExe _ = error "unexpected input to 'toExe'"

        lib = case libfs of
            []                         -> []
            ((Name pos _,  _) : _) ->
                [Section (Name pos "library") [] (map toField $ deps ++ libfs)]

      in map toField hdr ++ lib ++ exes

-- | See 'sectionizeFields'.
data Syntax = OldSyntax | NewSyntax
    deriving (Eq, Show)

-- TODO:
libFieldNames :: [FieldName]
libFieldNames = fieldGrammarKnownFieldList (libraryFieldGrammar Nothing)

-------------------------------------------------------------------------------
-- Suplementary build information
-------------------------------------------------------------------------------

readHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
readHookedBuildInfo = readAndParseFile parseHookedBuildInfo

parseHookedBuildInfo :: BS.ByteString -> ParseResult HookedBuildInfo
parseHookedBuildInfo bs = case readFields' bs of
    Right (fs, lexWarnings) -> do
        parseHookedBuildInfo' lexWarnings fs
    -- TODO: better marshalling of errors
    Left perr -> parseFatalFailure zeroPos (show perr)

parseHookedBuildInfo'
    :: [LexWarning]
    -> [Field Position]
    -> ParseResult HookedBuildInfo
parseHookedBuildInfo' lexWarnings fs = do
    parseWarnings (toPWarnings lexWarnings)
    (mLibFields, exes) <- stanzas fs
    mLib <- parseLib mLibFields
    biExes <- traverse parseExe exes
    return (mLib, biExes)
  where
    parseLib :: Fields Position -> ParseResult (Maybe BuildInfo)
    parseLib fields
        | Map.null fields = pure Nothing
        | otherwise       = Just <$> parseFieldGrammar cabalSpecLatest fields buildInfoFieldGrammar

    parseExe :: (UnqualComponentName, Fields Position) -> ParseResult (UnqualComponentName, BuildInfo)
    parseExe (n, fields) = do
        bi <- parseFieldGrammar cabalSpecLatest fields buildInfoFieldGrammar
        pure (n, bi)

    stanzas :: [Field Position] -> ParseResult (Fields Position, [(UnqualComponentName, Fields Position)])
    stanzas fields = do
        let (hdr0, exes0) = breakMaybe isExecutableField fields
        hdr <- toFields hdr0
        exes <- unfoldrM (traverse toExe) exes0
        pure (hdr, exes)

    toFields :: [Field Position] -> ParseResult (Fields Position)
    toFields fields = do
        let (fields', ss) = partitionFields fields
        traverse_ (traverse_ warnInvalidSubsection) ss
        pure fields'

    toExe
        :: ([FieldLine Position], [Field Position])
        -> ParseResult ((UnqualComponentName, Fields Position), Maybe ([FieldLine Position], [Field Position]))
    toExe (fss, fields) = do
        name <- runFieldParser zeroPos parsec cabalSpecLatest fss
        let (hdr0, rest) = breakMaybe isExecutableField fields
        hdr <- toFields hdr0
        pure ((name, hdr), rest)

    isExecutableField (Field (Name _ name) fss)
        | name == "executable" = Just fss
        | otherwise            = Nothing
    isExecutableField _ = Nothing

-- | Quickly scan new-style spec-version
--
-- A new-style spec-version declaration begins the .cabal file and
-- follow the following case-insensitive grammar (expressed in
-- RFC5234 ABNF):
--
-- @
-- newstyle-spec-version-decl = "cabal-version" *WS ":" *WS newstyle-pec-version *WS
--
-- spec-version               = NUM "." NUM [ "." NUM ]
--
-- NUM    = DIGIT0 / DIGITP 1*DIGIT0
-- DIGIT0 = %x30-39
-- DIGITP = %x31-39
-- WS = %20
-- @
--
scanSpecVersion :: BS.ByteString -> Maybe Version
scanSpecVersion bs = do
    fstline':_ <- pure (BS8.lines bs)

    -- parse <newstyle-spec-version-decl>
    -- normalise: remove all whitespace, convert to lower-case
    let fstline = BS.map toLowerW8 $ BS.filter (/= 0x20) fstline'
    ["cabal-version",vers] <- pure (BS8.split ':' fstline)

    -- parse <spec-version>
    --
    -- This is currently more tolerant regarding leading 0 digits.
    --
    ver <- simpleParsec (BS8.unpack vers)
    guard $ case versionNumbers ver of
              [_,_]   -> True
              [_,_,_] -> True
              _       -> False

    pure ver
  where
    -- | Translate ['A'..'Z'] to ['a'..'z']
    toLowerW8 :: Word8 -> Word8
    toLowerW8 w | 0x40 < w && w < 0x5b = w+0x20
                | otherwise            = w
