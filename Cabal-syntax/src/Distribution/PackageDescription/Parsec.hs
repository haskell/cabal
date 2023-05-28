{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
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
module Distribution.PackageDescription.Parsec
  ( -- * Package descriptions
    parseGenericPackageDescription
  , parseGenericPackageDescriptionMaybe

    -- ** Parsing
  , ParseResult
  , runParseResult

    -- * New-style spec-version
  , scanSpecVersion

    -- ** Supplementary build information
  , parseHookedBuildInfo
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad.State.Strict (StateT, execStateT)
import Control.Monad.Trans.Class (lift)
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Parsec (NamelessField (..))
import Distribution.Fields.ConfVar (parseConditionConfVar)
import Distribution.Fields.Field (FieldName, getName)
import Distribution.Fields.LexerMonad (LexWarning, toPWarnings)
import Distribution.Fields.ParseResult
import Distribution.Fields.Parser
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (freeVars, transformAllBuildInfos)
import Distribution.PackageDescription.FieldGrammar
import Distribution.PackageDescription.Quirks (patchQuirks)
import Distribution.Parsec (parsec, simpleParsecBS)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Warning (PWarnType (..))
import Distribution.Pretty (prettyShow)
import Distribution.Utils.Generic (breakMaybe, fromUTF8BS, toUTF8BS, unfoldrM, validateUTF8)
import Distribution.Version (Version, mkVersion, versionNumbers)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Compat.Newtype as Newtype
import qualified Distribution.Compat.NonEmptySet as NES
import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.Executable.Lens as L
import qualified Distribution.Types.ForeignLib.Lens as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens as L
import qualified Distribution.Types.SetupBuildInfo.Lens as L
import qualified Text.Parsec as P

------------------------------------------------------------------------------

-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.
parseGenericPackageDescription :: BS.ByteString -> ParseResult GenericPackageDescription
parseGenericPackageDescription bs = do
  -- set scanned version
  setCabalSpecVersion ver

  csv <- case ver of
    -- if we get too new version, fail right away
    Just v -> case cabalSpecFromVersionDigits (versionNumbers v) of
      Just csv -> return (Just csv)
      Nothing ->
        parseFatalFailure zeroPos $
          "Unsupported cabal-version " ++ prettyShow v ++ ". See https://github.com/haskell/cabal/issues/4899."
    _ -> pure Nothing

  case readFields' bs'' of
    Right (fs, lexWarnings) -> do
      when patched $
        parseWarning zeroPos PWTQuirkyCabalFile "Legacy cabal file"
      -- UTF8 is validated in a prepass step, afterwards parsing is lenient.
      parseGenericPackageDescription' csv lexWarnings invalidUtf8 fs
    -- TODO: better marshalling of errors
    Left perr -> parseFatalFailure pos (show perr)
      where
        ppos = P.errorPos perr
        pos = Position (P.sourceLine ppos) (P.sourceColumn ppos)
  where
    (patched, bs') = patchQuirks bs
    ver = scanSpecVersion bs'

    invalidUtf8 = validateUTF8 bs'

    -- if there are invalid utf8 characters, we make the bytestring valid.
    bs'' = case invalidUtf8 of
      Nothing -> bs'
      Just _ -> toUTF8BS (fromUTF8BS bs')

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
  { _stateGpd :: !GenericPackageDescription
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
  :: Maybe CabalSpecVersion
  -> [LexWarning]
  -> Maybe Int
  -> [Field Position]
  -> ParseResult GenericPackageDescription
parseGenericPackageDescription' scannedVer lexWarnings utf8WarnPos fs = do
  parseWarnings (toPWarnings lexWarnings)
  for_ utf8WarnPos $ \pos ->
    parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
  let (syntax, fs') = sectionizeFields fs
  let (fields, sectionFields) = takeFields fs'

  -- cabal-version
  specVer <- case scannedVer of
    Just v -> return v
    Nothing -> case Map.lookup "cabal-version" fields >>= safeLast of
      Nothing -> return CabalSpecV1_0
      Just (MkNamelessField pos fls) -> do
        -- version will be parsed twice, therefore we parse without warnings.
        v <-
          withoutWarnings $
            Newtype.unpack' SpecVersion
              <$>
              -- Use version with || and && but before addition of ^>= and removal of -any
              runFieldParser pos parsec CabalSpecV1_24 fls

        -- if it were at the beginning, scanner would found it
        when (v >= CabalSpecV2_2) $
          parseFailure pos $
            "cabal-version should be at the beginning of the file starting with spec version 2.2. "
              ++ "See https://github.com/haskell/cabal/issues/4899"

        return v

  -- reset cabal version, it might not be set
  let specVer' = mkVersion (cabalSpecToVersionDigits specVer)
  setCabalSpecVersion (Just specVer')

  -- Package description
  pd <- parseFieldGrammar specVer fields packageDescriptionFieldGrammar

  -- Check that scanned and parsed versions match.
  unless (specVer == specVersion pd) $
    parseFailure zeroPos $
      "Scanned and parsed cabal-versions don't match "
        ++ prettyShow (SpecVersion specVer)
        ++ " /= "
        ++ prettyShow (SpecVersion (specVersion pd))

  maybeWarnCabalVersion syntax pd

  -- Sections
  let gpd =
        emptyGenericPackageDescription
          & L.packageDescription .~ pd
  gpd1 <- view stateGpd <$> execStateT (goSections specVer sectionFields) (SectionS gpd Map.empty)

  let gpd2 = postProcessInternalDeps specVer gpd1
  checkForUndefinedFlags gpd2
  checkForUndefinedCustomSetup gpd2
  -- See nothunks test, without this deepseq we get (at least):
  -- Thunk in ThunkInfo {thunkContext = ["PackageIdentifier","PackageDescription","GenericPackageDescription"]}
  --
  -- TODO: re-benchmark, whether `deepseq` is important (both cabal-benchmarks and solver-benchmarks)
  -- TODO: remove the need for deepseq if `deepseq` in fact matters
  -- NOTE: IIRC it does affect (maximal) memory usage, which causes less GC pressure
  gpd2 `deepseq` return gpd2
  where
    safeLast :: [a] -> Maybe a
    safeLast = listToMaybe . reverse

    newSyntaxVersion :: CabalSpecVersion
    newSyntaxVersion = CabalSpecV1_2

    maybeWarnCabalVersion :: Syntax -> PackageDescription -> ParseResult ()
    maybeWarnCabalVersion syntax pkg
      | syntax == NewSyntax && specVersion pkg < newSyntaxVersion =
          parseWarning zeroPos PWTNewSyntax $
            "A package using section syntax must specify at least\n"
              ++ "'cabal-version: >= 1.2'."
    maybeWarnCabalVersion syntax pkg
      | syntax == OldSyntax && specVersion pkg >= newSyntaxVersion =
          parseWarning zeroPos PWTOldSyntax $
            "A package using 'cabal-version: "
              ++ prettyShow (SpecVersion (specVersion pkg))
              ++ "' must use section syntax. See the Cabal user guide for details."
    maybeWarnCabalVersion _ _ = return ()

goSections :: CabalSpecVersion -> [Field Position] -> SectionParser ()
goSections specVer = traverse_ process
  where
    process (Field (Name pos name) _) =
      lift $
        parseWarning pos PWTTrailingFields $
          "Ignoring trailing fields after sections: " ++ show name
    process (Section name args secFields) =
      parseSection name args secFields

    snoc x xs = xs ++ [x]

    hasCommonStanzas = specHasCommonStanzas specVer

    -- we need signature, because this is polymorphic, but not-closed
    parseCondTree'
      :: L.HasBuildInfo a
      => ParsecFieldGrammar' a
      -- \^ grammar
      -> (BuildInfo -> a)
      -> Map String CondTreeBuildInfo
      -- \^ common stanzas
      -> [Field Position]
      -> ParseResult (CondTree ConfVar [Dependency] a)
    parseCondTree' = parseCondTreeWithCommonStanzas specVer

    parseSection :: Name Position -> [SectionArg Position] -> [Field Position] -> SectionParser ()
    parseSection (Name pos name) args fields
      | hasCommonStanzas == NoCommonStanzas
      , name == "common" = lift $ do
          parseWarning pos PWTUnknownSection $ "Ignoring section: common. You should set cabal-version: 2.2 or larger to use common stanzas."
      | name == "common" = do
          commonStanzas <- use stateCommonStanzas
          name' <- lift $ parseCommonName pos args
          biTree <- lift $ parseCondTree' buildInfoFieldGrammar id commonStanzas fields

          case Map.lookup name' commonStanzas of
            Nothing -> stateCommonStanzas .= Map.insert name' biTree commonStanzas
            Just _ ->
              lift $
                parseFailure pos $
                  "Duplicate common stanza: " ++ name'
      | name == "library" && null args = do
          prev <- use $ stateGpd . L.condLibrary
          when (isJust prev) $
            lift $
              parseFailure pos $
                "Multiple main libraries; have you forgotten to specify a name for an internal library?"

          commonStanzas <- use stateCommonStanzas
          let name'' = LMainLibName
          lib <- lift $ parseCondTree' (libraryFieldGrammar name'') (libraryFromBuildInfo name'') commonStanzas fields
          --
          -- TODO check that not set
          stateGpd . L.condLibrary ?= lib

      -- Sublibraries
      -- TODO: check cabal-version
      | name == "library" = do
          commonStanzas <- use stateCommonStanzas
          name' <- parseUnqualComponentName pos args
          let name'' = LSubLibName name'
          lib <- lift $ parseCondTree' (libraryFieldGrammar name'') (libraryFromBuildInfo name'') commonStanzas fields
          -- TODO check duplicate name here?
          stateGpd . L.condSubLibraries %= snoc (name', lib)

      -- TODO: check cabal-version
      | name == "foreign-library" = do
          commonStanzas <- use stateCommonStanzas
          name' <- parseUnqualComponentName pos args
          flib <- lift $ parseCondTree' (foreignLibFieldGrammar name') (fromBuildInfo' name') commonStanzas fields

          let hasType ts = foreignLibType ts /= foreignLibType mempty
          unless (onAllBranches hasType flib) $
            lift $
              parseFailure pos $
                concat
                  [ "Foreign library " ++ show (prettyShow name')
                  , " is missing required field \"type\" or the field "
                  , "is not present in all conditional branches. The "
                  , "available test types are: "
                  , intercalate ", " (map prettyShow knownForeignLibTypes)
                  ]

          -- TODO check duplicate name here?
          stateGpd . L.condForeignLibs %= snoc (name', flib)
      | name == "executable" = do
          commonStanzas <- use stateCommonStanzas
          name' <- parseUnqualComponentName pos args
          exe <- lift $ parseCondTree' (executableFieldGrammar name') (fromBuildInfo' name') commonStanzas fields
          -- TODO check duplicate name here?
          stateGpd . L.condExecutables %= snoc (name', exe)
      | name == "test-suite" = do
          commonStanzas <- use stateCommonStanzas
          name' <- parseUnqualComponentName pos args
          testStanza <- lift $ parseCondTree' testSuiteFieldGrammar (fromBuildInfo' name') commonStanzas fields
          testSuite <- lift $ traverse (validateTestSuite specVer pos) testStanza

          let hasType ts = testInterface ts /= testInterface mempty
          unless (onAllBranches hasType testSuite) $
            lift $
              parseFailure pos $
                concat
                  [ "Test suite " ++ show (prettyShow name')
                  , concat $ case specVer of
                      v
                        | v >= CabalSpecV3_8 ->
                            [ " is missing required field \"main-is\" or the field "
                            , "is not present in all conditional branches."
                            ]
                      _ ->
                        [ " is missing required field \"type\" or the field "
                        , "is not present in all conditional branches. The "
                        , "available test types are: "
                        , intercalate ", " (map prettyShow knownTestTypes)
                        ]
                  ]

          -- TODO check duplicate name here?
          stateGpd . L.condTestSuites %= snoc (name', testSuite)
      | name == "benchmark" = do
          commonStanzas <- use stateCommonStanzas
          name' <- parseUnqualComponentName pos args
          benchStanza <- lift $ parseCondTree' benchmarkFieldGrammar (fromBuildInfo' name') commonStanzas fields
          bench <- lift $ traverse (validateBenchmark specVer pos) benchStanza

          let hasType ts = benchmarkInterface ts /= benchmarkInterface mempty
          unless (onAllBranches hasType bench) $
            lift $
              parseFailure pos $
                concat
                  [ "Benchmark " ++ show (prettyShow name')
                  , concat $ case specVer of
                      v
                        | v >= CabalSpecV3_8 ->
                            [ " is missing required field \"main-is\" or the field "
                            , "is not present in all conditional branches."
                            ]
                      _ ->
                        [ " is missing required field \"type\" or the field "
                        , "is not present in all conditional branches. The "
                        , "available benchmark types are: "
                        , intercalate ", " (map prettyShow knownBenchmarkTypes)
                        ]
                  ]

          -- TODO check duplicate name here?
          stateGpd . L.condBenchmarks %= snoc (name', bench)
      | name == "flag" = do
          name' <- parseNameBS pos args
          name'' <- lift $ runFieldParser' [pos] parsec specVer (fieldLineStreamFromBS name') `recoverWith` mkFlagName ""
          flag <- lift $ parseFields specVer fields (flagFieldGrammar name'')
          -- Check default flag
          stateGpd . L.genPackageFlags %= snoc flag
      | name == "custom-setup" && null args = do
          sbi <- lift $ parseFields specVer fields (setupBInfoFieldGrammar False)
          stateGpd . L.packageDescription . L.setupBuildInfo ?= sbi
      | name == "source-repository" = do
          kind <- lift $ case args of
            [SecArgName spos secName] ->
              runFieldParser' [spos] parsec specVer (fieldLineStreamFromBS secName) `recoverWith` RepoHead
            [] -> do
              parseFailure pos "'source-repository' requires exactly one argument"
              pure RepoHead
            _ -> do
              parseFailure pos $ "Invalid source-repository kind " ++ show args
              pure RepoHead

          sr <- lift $ parseFields specVer fields (sourceRepoFieldGrammar kind)
          stateGpd . L.packageDescription . L.sourceRepos %= snoc sr
      | otherwise =
          lift $
            parseWarning pos PWTUnknownSection $
              "Ignoring section: " ++ show name

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
  -> [Field Position]
  -- ^ fields to be parsed
  -> ParsecFieldGrammar' a
  -> ParseResult a
parseFields v fields grammar = do
  let (fs0, ss) = partitionFields fields
  traverse_ (traverse_ warnInvalidSubsection) ss
  parseFieldGrammar v fs0 grammar

warnInvalidSubsection :: Section Position -> ParseResult ()
warnInvalidSubsection (MkSection (Name pos name) _ _) =
  void $ parseFailure pos $ "invalid subsection " ++ show name

parseCondTree
  :: forall a
   . L.HasBuildInfo a
  => CabalSpecVersion
  -> HasElif
  -- ^ accept @elif@
  -> ParsecFieldGrammar' a
  -- ^ grammar
  -> Map String CondTreeBuildInfo
  -- ^ common stanzas
  -> (BuildInfo -> a)
  -- ^ constructor from buildInfo
  -> (a -> [Dependency])
  -- ^ condition extractor
  -> [Field Position]
  -> ParseResult (CondTree ConfVar [Dependency] a)
parseCondTree v hasElif grammar commonStanzas fromBuildInfo cond = go
  where
    go fields0 = do
      (fields, endo) <-
        if v >= CabalSpecV3_0
          then processImports v fromBuildInfo commonStanzas fields0
          else traverse (warnImport v) fields0 >>= \fields1 -> return (catMaybes fields1, id)

      let (fs, ss) = partitionFields fields
      x <- parseFieldGrammar v fs grammar
      branches <- concat <$> traverse parseIfs ss
      return $ endo $ CondNode x (cond x) branches

    parseIfs :: [Section Position] -> ParseResult [CondBranch ConfVar [Dependency] a]
    parseIfs [] = return []
    parseIfs (MkSection (Name _ name) test fields : sections) | name == "if" = do
      test' <- parseConditionConfVar test
      fields' <- go fields
      (elseFields, sections') <- parseElseIfs sections
      return (CondBranch test' fields' elseFields : sections')
    parseIfs (MkSection (Name pos name) _ _ : sections) = do
      parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name
      parseIfs sections

    parseElseIfs
      :: [Section Position]
      -> ParseResult (Maybe (CondTree ConfVar [Dependency] a), [CondBranch ConfVar [Dependency] a])
    parseElseIfs [] = return (Nothing, [])
    parseElseIfs (MkSection (Name pos name) args fields : sections) | name == "else" = do
      unless (null args) $
        parseFailure pos $
          "`else` section has section arguments " ++ show args
      elseFields <- go fields
      sections' <- parseIfs sections
      return (Just elseFields, sections')
    parseElseIfs (MkSection (Name _ name) test fields : sections)
      | hasElif == HasElif
      , name == "elif" = do
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

\* Almost all fields are optional

\* This is simple approach so declarative bi-directional format (parsing and
printing) of structure could be specified (list of @'FieldDescr' a@)

\* There are surface syntax fields corresponding to single field in the file:
  @license-file@ and @license-files@

\* This is quite safe approach.

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
type CondTreeBuildInfo = CondTree ConfVar [Dependency] BuildInfo

-- | Create @a@ from 'BuildInfo'.
-- This class is used to implement common stanza parsing.
--
-- Law: @view buildInfo . fromBuildInfo = id@
--
-- This takes name, as 'FieldGrammar's take names too.
class L.HasBuildInfo a => FromBuildInfo a where
  fromBuildInfo' :: UnqualComponentName -> BuildInfo -> a

libraryFromBuildInfo :: LibraryName -> BuildInfo -> Library
libraryFromBuildInfo n bi =
  emptyLibrary
    { libName = n
    , libVisibility = case n of
        LMainLibName -> LibraryVisibilityPublic
        LSubLibName _ -> LibraryVisibilityPrivate
    , libBuildInfo = bi
    }

instance FromBuildInfo BuildInfo where fromBuildInfo' _ = id
instance FromBuildInfo ForeignLib where fromBuildInfo' n bi = set L.foreignLibName n $ set L.buildInfo bi emptyForeignLib
instance FromBuildInfo Executable where fromBuildInfo' n bi = set L.exeName n $ set L.buildInfo bi emptyExecutable

instance FromBuildInfo TestSuiteStanza where
  fromBuildInfo' _ bi = TestSuiteStanza Nothing Nothing Nothing bi []

instance FromBuildInfo BenchmarkStanza where
  fromBuildInfo' _ bi = BenchmarkStanza Nothing Nothing Nothing bi

parseCondTreeWithCommonStanzas
  :: forall a
   . L.HasBuildInfo a
  => CabalSpecVersion
  -> ParsecFieldGrammar' a
  -- ^ grammar
  -> (BuildInfo -> a)
  -- ^ construct fromBuildInfo
  -> Map String CondTreeBuildInfo
  -- ^ common stanzas
  -> [Field Position]
  -> ParseResult (CondTree ConfVar [Dependency] a)
parseCondTreeWithCommonStanzas v grammar fromBuildInfo commonStanzas fields = do
  (fields', endo) <- processImports v fromBuildInfo commonStanzas fields
  x <- parseCondTree v hasElif grammar commonStanzas fromBuildInfo (view L.targetBuildDepends) fields'
  return (endo x)
  where
    hasElif = specHasElif v

processImports
  :: forall a
   . L.HasBuildInfo a
  => CabalSpecVersion
  -> (BuildInfo -> a)
  -- ^ construct fromBuildInfo
  -> Map String CondTreeBuildInfo
  -- ^ common stanzas
  -> [Field Position]
  -> ParseResult ([Field Position], CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] a)
processImports v fromBuildInfo commonStanzas = go []
  where
    hasCommonStanzas = specHasCommonStanzas v

    getList' :: List CommaFSep Token String -> [String]
    getList' = Newtype.unpack

    go acc (Field (Name pos name) _ : fields)
      | name == "import"
      , hasCommonStanzas == NoCommonStanzas = do
          parseWarning pos PWTUnknownField "Unknown field: import. You should set cabal-version: 2.2 or larger to use common stanzas"
          go acc fields
    -- supported:
    go acc (Field (Name pos name) fls : fields) | name == "import" = do
      names <- getList' <$> runFieldParser pos parsec v fls
      names' <- for names $ \commonName ->
        case Map.lookup commonName commonStanzas of
          Nothing -> do
            parseFailure pos $ "Undefined common stanza imported: " ++ commonName
            pure Nothing
          Just commonTree ->
            pure (Just commonTree)

      go (acc ++ catMaybes names') fields

    -- parse actual CondTree
    go acc fields = do
      fields' <- catMaybes <$> traverse (warnImport v) fields
      pure $ (fields', \x -> foldr (mergeCommonStanza fromBuildInfo) x acc)

-- | Warn on "import" fields, also map to Maybe, so errorneous fields can be filtered
warnImport :: CabalSpecVersion -> Field Position -> ParseResult (Maybe (Field Position))
warnImport v (Field (Name pos name) _) | name == "import" = do
  if specHasCommonStanzas v == NoCommonStanzas
    then parseWarning pos PWTUnknownField "Unknown field: import. You should set cabal-version: 2.2 or larger to use common stanzas"
    else parseWarning pos PWTUnknownField "Unknown field: import. Common stanza imports should be at the top of the enclosing section"
  return Nothing
warnImport _ f = pure (Just f)

mergeCommonStanza
  :: L.HasBuildInfo a
  => (BuildInfo -> a)
  -> CondTree ConfVar [Dependency] BuildInfo
  -> CondTree ConfVar [Dependency] a
  -> CondTree ConfVar [Dependency] a
mergeCommonStanza fromBuildInfo (CondNode bi _ bis) (CondNode x _ cs) =
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
    go acc ct =
      let acc' = acc `mappend` condTreeData ct
       in p acc' || any (goBranch acc') (condTreeComponents ct)

    -- Both the 'true' and the 'false' block must satisfy the property.
    goBranch :: a -> CondBranch v c a -> Bool
    goBranch _ (CondBranch _ _ Nothing) = False
    goBranch acc (CondBranch _ t (Just e)) = go acc t && go acc e

-------------------------------------------------------------------------------
-- Post parsing checks
-------------------------------------------------------------------------------

-- | Check that we
--
-- * don't use undefined flags (very bad)
-- * define flags which are unused (just bad)
checkForUndefinedFlags :: GenericPackageDescription -> ParseResult ()
checkForUndefinedFlags gpd = do
  let definedFlags, usedFlags :: Set.Set FlagName
      definedFlags = toSetOf (L.genPackageFlags . traverse . getting flagName) gpd
      usedFlags = getConst $ L.allCondTrees f gpd

  -- Note: we can check for defined, but unused flags here too.
  unless (usedFlags `Set.isSubsetOf` definedFlags) $
    parseFailure zeroPos $
      "These flags are used without having been defined: "
        ++ intercalate ", " [unFlagName fn | fn <- Set.toList $ usedFlags `Set.difference` definedFlags]
  where
    f :: CondTree ConfVar c a -> Const (Set.Set FlagName) (CondTree ConfVar c a)
    f ct = Const (Set.fromList (freeVars ct))

-- | Since @cabal-version: 1.24@ one can specify @custom-setup@.
-- Let us require it.
checkForUndefinedCustomSetup :: GenericPackageDescription -> ParseResult ()
checkForUndefinedCustomSetup gpd = do
  let pd = packageDescription gpd
  let csv = specVersion pd

  when (buildType pd == Custom && isNothing (setupBuildInfo pd)) $
    when (csv >= CabalSpecV1_24) $
      parseFailure zeroPos $
        "Since cabal-version: 1.24 specifying custom-setup section is mandatory"

-------------------------------------------------------------------------------
-- Post processing of internal dependencies
-------------------------------------------------------------------------------

-- Note [Dependencies on sublibraries]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- This is solution to https://github.com/haskell/cabal/issues/6083
--
-- Before 'cabal-version: 3.0' we didn't have a syntax specially
-- for referring to internal libraries. Internal library names
-- shadowed the outside ones.
--
-- Since 'cabal-version: 3.0' we have ability to write
--
--     build-depends: some-package:its-sub-lib >=1.2.3
--
-- This allows us to refer also to local packages by `this-package:sublib`.
-- So since 'cabal-version: 3.4' to refer to *any*
-- sublibrary we must use the two part syntax. Here's small table:
--
--                   | pre-3.4             |      3.4 and after            |
-- ------------------|---------------------|-------------------------------|
-- pkg-name          | may refer to sublib | always refers to external pkg |
-- pkg-name:sublib   | refers to sublib    | refers to sublib              |
-- pkg-name:pkg-name | may refer to sublib | always refers to external pkg |
--
-- In pre-3.4 case, if a package 'this-pkg' has a sublibrary 'pkg-name',
-- all dependency definitions will refer to that sublirary.
--
-- In 3.4 and after case, 'pkg-name' will always refer to external package,
-- and to use internal library you have to say 'this-pkg:pkg-name'.
--
-- In summary, In 3.4 and after, the internal names don't shadow,
-- as there is an explicit syntax to refer to them,
-- i.e. what you write is what you get;
-- For pre-3.4 we post-process the file.
--
-- Similarly, we process mixins.
-- See https://github.com/haskell/cabal/issues/6281
--

postProcessInternalDeps :: CabalSpecVersion -> GenericPackageDescription -> GenericPackageDescription
postProcessInternalDeps specVer gpd
  | specVer >= CabalSpecV3_4 = gpd
  | otherwise = transformAllBuildInfos transformBI transformSBI gpd
  where
    transformBI :: BuildInfo -> BuildInfo
    transformBI =
      over L.targetBuildDepends (concatMap transformD)
        . over L.mixins (map transformM)

    transformSBI :: SetupBuildInfo -> SetupBuildInfo
    transformSBI = over L.setupDepends (concatMap transformD)

    transformD :: Dependency -> [Dependency]
    transformD (Dependency pn vr ln)
      | uqn `Set.member` internalLibs
      , LMainLibName `NES.member` ln =
          case NES.delete LMainLibName ln of
            Nothing -> [dep]
            Just ln' -> [dep, Dependency pn vr ln']
      where
        uqn = packageNameToUnqualComponentName pn
        dep = Dependency thisPn vr (NES.singleton (LSubLibName uqn))
    transformD d = [d]

    transformM :: Mixin -> Mixin
    transformM (Mixin pn LMainLibName incl)
      | uqn `Set.member` internalLibs =
          mkMixin thisPn (LSubLibName uqn) incl
      where
        uqn = packageNameToUnqualComponentName pn
    transformM m = m

    thisPn :: PackageName
    thisPn = pkgName (package (packageDescription gpd))

    internalLibs :: Set UnqualComponentName
    internalLibs =
      Set.fromList
        [ n
        | (n, _) <- condSubLibraries gpd
        ]

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
  Nothing -> (NewSyntax, fs)
  where
    -- return 'Just' if all fields are simple fields
    classifyFields :: [Field ann] -> Maybe [(Name ann, [FieldLine ann])]
    classifyFields = traverse f
      where
        f (Field name fieldlines) = Just (name, fieldlines)
        f _ = Nothing

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
        (hdr0, exes0) = break ((== "executable") . getName . fst) fields
        (hdr, libfs0) = partition (not . (`elem` libFieldNames) . getName . fst) hdr0

        (deps, libfs) =
          partition
            ((== "build-depends") . getName . fst)
            libfs0

        exes = unfoldr toExe exes0
        toExe [] = Nothing
        toExe ((Name pos n, ls) : r)
          | n == "executable" =
              let (efs, r') = break ((== "executable") . getName . fst) r
               in Just (Section (Name pos "executable") [SecArgName pos $ trim $ fieldlinesToBS ls] (map toField $ deps ++ efs), r')
        toExe _ = error "unexpected input to 'toExe'"

        lib = case libfs of
          [] -> []
          ((Name pos _, _) : _) ->
            [Section (Name pos "library") [] (map toField $ deps ++ libfs)]
       in
        map toField hdr ++ lib ++ exes

-- | See 'sectionizeFields'.
data Syntax = OldSyntax | NewSyntax
  deriving (Eq, Show)

-- TODO:
libFieldNames :: [FieldName]
libFieldNames = fieldGrammarKnownFieldList (libraryFieldGrammar LMainLibName)

-------------------------------------------------------------------------------
-- Supplementary build information
-------------------------------------------------------------------------------

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
      | otherwise = Just <$> parseFieldGrammar cabalSpecLatest fields buildInfoFieldGrammar

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
      | otherwise = Nothing
    isExecutableField _ = Nothing

-------------------------------------------------------------------------------
-- Scan of spec version
-------------------------------------------------------------------------------

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
scanSpecVersion :: BS.ByteString -> Maybe Version
scanSpecVersion bs = do
  fstline' : _ <- pure (BS8.lines bs)

  -- parse <newstyle-spec-version-decl>
  -- normalise: remove all whitespace, convert to lower-case
  let fstline = BS.map toLowerW8 $ BS.filter (/= 0x20) fstline'
  ["cabal-version", vers] <- pure (BS8.split ':' fstline)

  -- parse <spec-version>
  --
  -- This is currently more tolerant regarding leading 0 digits.
  --
  ver <- simpleParsecBS vers
  guard $ case versionNumbers ver of
    [_, _] -> True
    [_, _, _] -> True
    _ -> False

  pure ver
  where
    -- \| Translate ['A'..'Z'] to ['a'..'z']
    toLowerW8 :: Word8 -> Word8
    toLowerW8 w
      | 0x40 < w && w < 0x5b = w + 0x20
      | otherwise = w
