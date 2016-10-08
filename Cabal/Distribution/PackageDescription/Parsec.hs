{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE Rank2Types #-}
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

    -- ** Parsing
    ParseResult,

    -- ** Supplementary build information
    -- readHookedBuildInfo,
    -- parseHookedBuildInfo,
    ) where

import           Distribution.Compat.Prelude
import           Prelude ()

import qualified Data.ByteString                                   as BS
import           Data.List                                         (partition)
import qualified Data.Map                                          as Map
import           System.Directory
                 (doesFileExist)
import qualified Text.Parsec                                       as P
import qualified Text.Parsec.Error                                 as P

import           Distribution.PackageDescription
import           Distribution.Simple.Utils
                 (die, fromUTF8BS, warn)
import           Distribution.Verbosity                            (Verbosity)

import           Distribution.PackageDescription.Parsec.FieldDescr
import           Distribution.Parsec.Class                         (parsec)
import           Distribution.Parsec.ConfVar
                 (parseConditionConfVar)
import           Distribution.Parsec.Parser
import           Distribution.Parsec.Types.Common
import           Distribution.Parsec.Types.Field                   (getName)
import           Distribution.Parsec.Types.FieldDescr
import           Distribution.Parsec.Types.ParseResult
import           Distribution.Parsec.LexerMonad
                 (LexWarning, toPWarning)

import Distribution.Text (display)
import Distribution.Version (mkVersion, Version, asVersionIntervals, orLaterVersion, LowerBound (..))

-- ---------------------------------------------------------------
-- Parsing

-- | Helper combinator to do parsing plumbing for files.
--
-- Given a parser and a filename, return the parse of the file,
-- after checking if the file exists.
--
-- Argument order is chosen to encourage partial application.
readAndParseFile
    :: (BS.ByteString -> ParseResult a)
    -> Verbosity
    -> FilePath
    -> IO a
readAndParseFile parser verbosity fpath = do
    exists <- doesFileExist fpath
    unless exists
        (die $ "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue.")
    bs <- BS.readFile fpath
    let (warnings, errors, result) = runParseResult (parser bs)
    traverse_ (warn verbosity . showPWarning fpath) warnings
    traverse_ (warn verbosity . showPError fpath) errors
    case result of
        Nothing -> die $ "Failing parsing \"" ++ fpath ++ "\"."
        Just x  -> return x

-- | Parse the given package file.
readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readAndParseFile parseGenericPackageDescription

------------------------------------------------------------------------------
-- | Parses the given file into a 'GenericPackageDescription'.
--
-- In Cabal 1.2 the syntax for package descriptions was changed to a format
-- with sections and possibly indented property descriptions.
--
-- TODO: add lex warnings
parseGenericPackageDescription :: BS.ByteString -> ParseResult GenericPackageDescription
parseGenericPackageDescription bs = case readFields' bs of
    Right (fs, lexWarnings) -> parseGenericPackageDescription' lexWarnings fs
    -- | TODO: better marshalling of errors
    Left perr -> parseFatalFailure (Position 0 0) (show perr)

runFieldParser :: FieldParser a -> [FieldLine Position] -> ParseResult a
runFieldParser p ls = runFieldParser' pos p =<< fieldlinesToString pos ls
  where
    -- TODO: make per line lookup
    pos = case ls of
        []                     -> Position 0 0
        (FieldLine pos' _ : _) -> pos'

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)

-- TODO: Take position  from FieldLine
-- TODO: Take field name
fieldlinesToString :: Position -> [FieldLine ann] -> ParseResult String
fieldlinesToString pos fls =
    let str = intercalate "\n" . map (\(FieldLine _ bs') -> fromUTF8BS bs') $ fls
    in if '\xfffd' `elem` str
        then str <$ parseWarning pos PWTUTF "Invalid UTF8 encoding"
        else pure str

runFieldParser' :: Position -> FieldParser a -> String -> ParseResult a
runFieldParser' (Position row col) p str = case P.runParser p' [] "<field>" str of
    Right (pok, ws) -> do
        -- | TODO: map pos
        traverse_ (\(PWarning t pos w) -> parseWarning pos t w) ws 
        pure pok
    Left err        -> do
        let ppos = P.errorPos err
        -- Positions start from 1:1, not 0:0
        let epos = Position (row - 1 + P.sourceLine ppos) (col - 1 + P.sourceColumn ppos)
        let msg = P.showErrorMessages
                "or" "unknown parse error" "expecting" "unexpected" "end of input"
                (P.errorMessages err)

        parseFatalFailure epos $ msg ++ ": " ++ show str
  where
    p' = (,) <$ P.spaces <*> p <* P.spaces <* P.eof <*> P.getState

data GPDS = Fields | Sections

-- Note [Accumulating parser]
parseGenericPackageDescription'
    :: [LexWarning]
    -> [Field Position]
    -> ParseResult GenericPackageDescription
parseGenericPackageDescription' lexWarnings fs = do
    parseWarnings' (fmap toPWarning lexWarnings)
    let (newSyntax, fs') = sectionizeFields fs
    (_, gpd) <- foldM go (Fields, emptyGpd) fs'
    -- Various post checks
    maybeWarnCabalVersion newSyntax (packageDescription gpd)
    checkForUndefinedFlags gpd
    -- TODO: do other validations
    return gpd
  where
    go :: (GPDS, GenericPackageDescription)
       -> Field Position
       -> ParseResult (GPDS, GenericPackageDescription)
    go s                (IfElseBlock pos _ _ _)  = do
        parseFailure pos "if else block on the top level"
        return s
    go (Fields, gpd)   (Field (Name pos name) fieldLines) =
        case Map.lookup name pdFieldParsers of
            -- | TODO: can be more accurate
            Nothing -> fieldlinesToString pos fieldLines >>= \value -> case storeXFieldsPD name value (packageDescription gpd) of
                Nothing -> do
                    parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
                    return (Fields, gpd)
                Just pd -> return (Fields, gpd { packageDescription = pd })
            Just parser -> (\pd -> (Fields, gpd { packageDescription = pd }))
                <$> runFieldParser (parser $ packageDescription gpd) fieldLines
    go (Sections, gpd) (Field (Name pos _) _) = do
        parseWarning pos PWTTrailingFields "Ignoring trailing fields after sections"
        return (Sections, gpd)
    go (s, gpd)        (Section name args fields) =
        (,) s <$> parseSection gpd name args fields

    pdFieldParsers :: Map FieldName (PackageDescription -> FieldParser PackageDescription)
    pdFieldParsers = Map.fromList $
        map (\x -> (fieldName x, fieldParser x)) pkgDescrFieldDescrs

    emptyGpd :: GenericPackageDescription
    emptyGpd = GenericPackageDescription emptyPackageDescription [] Nothing [] [] [] []

    parseSection
        :: GenericPackageDescription
        -> Name Position
        -> [SectionArg Position]
        -> [Field Position]
        -> ParseResult GenericPackageDescription
    parseSection gpd (Name pos name) args fields
        | name == "library" && null args = do
            -- TODO: check that library is defined once
            l <- parseCondTree libFieldDescrs storeXFieldsLib (targetBuildDepends . libBuildInfo) emptyLibrary fields
            let gpd' = gpd { condLibrary = Just l }
            pure gpd'

        | name == "executable" = do
            -- TODO: make a combinator for this, repeated in tests and bench
            name' <- parseName pos args
            -- Note: we don't parse the "executable" field here, hence the tail hack. Duncan 2010
            exe <- parseCondTree (tail executableFieldDescrs) storeXFieldsExe (targetBuildDepends . buildInfo) emptyExecutable fields
            -- TODO check duplicate name here?
            let gpd' = gpd { condExecutables = condExecutables gpd ++ [(name', exe)] }
            pure gpd'

        | name == "test-suite" = do
            name' <- parseName pos args
            testStanza <- parseCondTree testSuiteFieldDescrs storeXFieldsTest (targetBuildDepends . testStanzaBuildInfo) emptyTestStanza fields
            testSuite <- traverse (validateTestSuite pos) testStanza
            -- TODO check duplicate name here?
            let gpd' = gpd { condTestSuites = condTestSuites gpd ++ [(name', testSuite)] }
            pure gpd'

        | name == "benchmark" = do
            name' <- parseName pos args
            benchStanza <- parseCondTree benchmarkFieldDescrs storeXFieldsBenchmark (targetBuildDepends . benchmarkStanzaBuildInfo) emptyBenchmarkStanza fields
            bench <- traverse (validateBenchmark pos) benchStanza
            -- TODO check duplicate name here?
            let gpd' = gpd { condBenchmarks = condBenchmarks gpd ++ [(name', bench)] }
            pure gpd'

        | name == "flag" = do
            name' <- parseName pos args
            name'' <- runFieldParser' pos parsec name' `recoverWith` FlagName ""
            flag <- parseFields flagFieldDescrs warnUnrec (emptyFlag name'') fields
            -- Check default flag
            let gpd' = gpd { genPackageFlags = genPackageFlags gpd ++ [flag] }
            pure gpd'

        | name == "custom-setup" && null args = do
            sbi <- parseFields setupBInfoFieldDescrs warnUnrec mempty fields
            let pd = packageDescription gpd
            -- TODO: what if already defined?
            let gpd' = gpd { packageDescription = pd { setupBuildInfo = Just sbi } }
            pure gpd'

        | name == "source-repository" = do
            kind <- case args of
                [SecArgName spos secName] ->
                    runFieldParser' spos parsec (fromUTF8BS secName) `recoverWith` RepoHead
                [] -> do
                    parseFailure pos $ "'source-repository' needs one argument"
                    pure RepoHead
                _ -> do
                    parseFailure pos $ "Invalid source-repository kind " ++ show args
                    pure RepoHead
            sr <- parseFields sourceRepoFieldDescrs warnUnrec (emptySourceRepo kind) fields
            -- I want lens
            let pd =  packageDescription gpd
            let srs = sourceRepos pd
            let gpd' = gpd { packageDescription = pd { sourceRepos = srs ++ [sr] } }
            pure gpd'

        | otherwise = do
            parseWarning pos PWTUnknownSection $ "Ignoring section: " ++ show name
            pure gpd

    newSyntaxVersion :: Version
    newSyntaxVersion = mkVersion [1, 2]

    maybeWarnCabalVersion :: Bool -> PackageDescription -> ParseResult ()
    maybeWarnCabalVersion newsyntax pkg
      | newsyntax && specVersion pkg < newSyntaxVersion 
      = parseWarning (Position 0 0) PWTNewSyntax $
             "A package using section syntax must specify at least\n"
          ++ "'cabal-version: >= 1.2'."

    maybeWarnCabalVersion newsyntax pkg
      | not newsyntax && specVersion pkg >= newSyntaxVersion 
      = parseWarning (Position 0 0) PWTOldSyntax $
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

{-
    handleFutureVersionParseFailure :: Version -> ParseResult a -> ParseResult GenericPackageDescription
    handleFutureVersionParseFailure _cabalVersionNeeded _parseBody =
        error "handleFutureVersionParseFailure"
-}

 {-
      undefined (unless versionOk (warning message) >> parseBody)
        `catchParseError` \parseError -> case parseError of
        TabsError _   -> parseFail parseError
        _ | versionOk -> parseFail parseError
          | otherwise -> fail message
      where versionOk = cabalVersionNeeded <= cabalVersion
            message   = "This package requires at least Cabal version "
                     ++ display cabalVersionNeeded
    -}

    checkForUndefinedFlags
        :: GenericPackageDescription
        -> ParseResult ()
    checkForUndefinedFlags _gpd = pure ()
{-
        let definedFlags = map flagName flags
        mapM_ (checkCondTreeFlags definedFlags) (maybeToList mlib)
        mapM_ (checkCondTreeFlags definedFlags . snd) sub_libs
        mapM_ (checkCondTreeFlags definedFlags . snd) exes
        mapM_ (checkCondTreeFlags definedFlags . snd) tests

    checkCondTreeFlags :: [FlagName] -> CondTree ConfVar c a -> PM ()
    checkCondTreeFlags definedFlags ct = do
        let fv = nub $ freeVars ct
        unless (all (`elem` definedFlags) fv) $
            fail $ "These flags are used without having been defined: "
                ++ intercalate ", " [ n | FlagName n <- fv \\ definedFlags ]
-}

parseName :: Position -> [SectionArg Position] -> ParseResult String
parseName pos args = case args of
    [SecArgName _pos secName] ->
         pure $ fromUTF8BS secName
    [SecArgStr _pos secName] ->
         pure secName
    [] -> do
         parseFailure pos $ "name required"
         pure ""
    _ -> do
         -- TODO: pretty print args
         parseFailure pos $ "Invalid name " ++ show args
         pure ""

-- | Parse a list of fields, given a list of field descriptions,
--   a structure to accumulate the parsed fields, and a function
--   that can decide what to do with fields which don't match any
--   of the field descriptions.
parseFields
    :: forall a.
       [FieldDescr a]        -- ^ descriptions of fields we know how to parse
    -> UnknownFieldParser a  -- ^ possibly do something with unrecognized fields
    -> a                     -- ^ accumulator
    -> [Field Position]      -- ^ fields to be parsed
    -> ParseResult a
parseFields descrs _unknown = foldM go
  where
    go :: a -> Field Position -> ParseResult a
    go x (Section (Name pos name) _ _) = do
        -- Even we occur a subsection, we can continue parsing
        parseFailure pos $ "invalid subsection " ++ show name
        return x
    go x (IfElseBlock pos _ _ _) = do
        parseFailure pos $ "invalid if-else-block"
        return x
    go x (Field (Name pos name) fieldLines) =
        case Map.lookup name fieldParsers of
            Nothing -> do
                -- TODO: use 'unknown'
                parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
                return x
            Just parser ->
                runFieldParser (parser x) fieldLines

    fieldParsers :: Map FieldName (a -> FieldParser a)
    fieldParsers = Map.fromList $
        map (\x -> (fieldName x, fieldParser x)) descrs

type C c a = (Condition ConfVar, CondTree ConfVar c a, Maybe (CondTree ConfVar c a))

parseCondTree
    :: forall a c.
       [FieldDescr a]        -- ^ Field descriptions
    -> UnknownFieldParser a  -- ^ How to parse unknown fields
    -> (a -> c)              -- ^ Condition extractor
    -> a                     -- ^ Initial value
    -> [Field Position]      -- ^ Fields to parse
    -> ParseResult (CondTree ConfVar c a)
parseCondTree descs unknown cond ini = go0
  where
    go0 fields = do
        (x, xs) <- foldM go (ini, []) fields
        return $ CondNode x (cond x) xs

    -- | TODO: change to take and return condnode
    go :: (a, [C c a])  -> Field Position -> ParseResult (a, [C c a])
    go x (Section (Name pos name) _ _) = do
        -- Even we occur a subsection, we can continue parsing
        -- http://hackage.haskell.org/package/constraints-0.1/constraints.cabal
        parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name
        return x

    go (x, xs) (IfElseBlock _pos tes con alt) = do
        tes'  <- parseConditionConfVar tes
        con' <- go0 con
        alt' <- case alt of
            [] -> pure Nothing
            _  -> Just <$> go0 alt
        let ieb = (tes', con', alt')
        return (x, xs ++ [ieb])

    go (x, xs) (Field (Name pos name) fieldLines) =
        case Map.lookup name fieldParsers of
            Nothing -> fieldlinesToString pos fieldLines >>= \value -> case unknown name value x of
                Nothing -> do
                    parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
                    return (x, xs)
                Just x' -> return (x', xs)
            Just parser ->
                (,xs) <$> runFieldParser (parser x) fieldLines

    fieldParsers :: Map FieldName (a -> FieldParser a)
    fieldParsers = Map.fromList $
        map (\x -> (fieldName x, fieldParser x)) descs
{-
    -- Extracts all fields in a block and returns a 'CondTree'.
    --
    -- We have to recurse down into conditionals and we treat fields that
    -- describe dependencies specially.
    collectFields :: ([Field Position] -> PM a) -> [Field Position]
                  -> PM (CondTree ConfVar [Dependency] a)
    collectFields parser allflds = do

        let simplFlds = [ f | f@Field{} <- allflds ]
            condFlds = [ f | f@IfElseBlock{} <- allflds ]
            sections = [ s | s@Section{} <- allflds ]

        -- Put these through the normal parsing pass too, so that we
        -- collect the ModRenamings
        let depFlds = filter isConstraint simplFlds

        mapM_
            (\(Section (Name pos n) _ _) -> lift . warning $
                "Unexpected section '" ++ show n ++ "' on " ++ showPos pos)
            sections

        a <- parser simplFlds
        deps <- liftM concat . mapM (lift . parseConstraint) $ depFlds

        ifs <- mapM processIfs condFlds

        return (CondNode a deps ifs)
      where
        isConstraint (Field (Name _ n) _) = n `elem` constraintFieldNames
        isConstraint _ = False

        processIfs (IfElseBlock c t e) = do
            cnd <- lift $ runP undefined "if" parseCondition (undefined c)
            t' <- collectFields parser t
            e' <- case e of
                   [] -> return Nothing
                   es -> do fs <- collectFields parser es
                            return (Just fs)
            return (cnd, t', e')
        processIfs _ = cabalBug "processIfs called with wrong field type"
-}

{- Note [Accumulating parser]

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
-- Old syntax
-------------------------------------------------------------------------------

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
--
-- Boolean in the return pair is 'False', if the file was using old syntax.
sectionizeFields :: [Field ann] -> (Bool, [Field ann])
sectionizeFields fs = case classifyFields fs of
    Just fields -> (False, convert fields)
    Nothing     -> (True, fs)
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

libFieldNames :: [FieldName]
libFieldNames = map fieldName libFieldDescrs
