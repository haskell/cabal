{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Parsing project configuration.
module Distribution.Client.ProjectConfig.Parsec
  ( -- * Package configuration
    parseProjectSkeleton
  , parseProject
  , ProjectConfigSkeleton
  , ProjectConfig (..)

    -- ** Parsing
  , ParseResult
  , runParseResult
  ) where

import Distribution.CabalSpecVersion
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig.FieldGrammar (packageConfigFieldGrammar, projectConfigFieldGrammar)
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton)
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Types.Repo hiding (repoName)
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Client.Types.SourceRepo (sourceRepositoryPackageGrammar)
import Distribution.Client.Utils.Parsec
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Parsec (NamelessField (..), namelessFieldAnn)
import Distribution.Fields (Field (..), FieldLine (..), FieldName, Name (..), SectionArg (..), readFields')
import Distribution.Fields.ConfVar (parseConditionConfVar)
import Distribution.Fields.Field (fieldLinesToString, sectionArgAnn)
import Distribution.Fields.LexerMonad (toPWarnings)
import Distribution.Fields.ParseResult
import Distribution.Parsec (ParsecParser, eitherParsec, parsec, parsecFilePath, runParsecParser)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
import Distribution.Parsec.Position (Position (..), incPos, zeroPos)
import Distribution.Parsec.Warning (PWarnType (..))
import Distribution.Simple.Program.Db (ProgramDb, defaultProgramDb, knownPrograms, lookupKnownProgram)
import Distribution.Simple.Program.Types (programName)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (debug, noticeDoc)
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.System (buildOS)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Utils.Generic (fromUTF8BS, toUTF8BS, validateUTF8)
import Distribution.Utils.NubList (toNubList)
import Distribution.Utils.String (trim)
import Distribution.Verbosity

import Control.Monad.State.Strict (StateT, execStateT, lift)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Client.Errors.Parser (ProjectFileSource (..))
import qualified Distribution.Compat.CharParsing as P
import Network.URI (parseURI, uriFragment, uriPath, uriScheme)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath (isAbsolute, isPathSeparator, makeValid, splitFileName, (</>))
import qualified Text.Parsec
import Text.PrettyPrint (render)
import qualified Text.PrettyPrint as Disp

singletonProjectConfigSkeleton :: ProjectConfig -> ProjectConfigSkeleton
singletonProjectConfigSkeleton x = CondNode x mempty mempty

readPreprocessFields :: BS.ByteString -> ParseResult src [Field Position]
readPreprocessFields bs = do
  case readFields' bs' of
    Right (fs, lexWarnings) -> do
      parseWarnings (toPWarnings lexWarnings)
      for_ invalidUtf8 $ \pos ->
        parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
      return fs
    Left perr -> parseFatalFailure pos (show perr)
      where
        ppos = Text.Parsec.errorPos perr
        pos = Position (Text.Parsec.sourceLine ppos) (Text.Parsec.sourceColumn ppos)
  where
    invalidUtf8 = validateUTF8 bs
    bs' = case invalidUtf8 of
      Nothing -> bs
      Just _ -> toUTF8BS (fromUTF8BS bs)

-- | Parses a project from its root config file, typically cabal.project.
parseProject
  :: FilePath
  -- ^ The root of the project configuration, typically cabal.project
  -> FilePath
  -> HttpTransport
  -> Verbosity
  -> ProjectConfigToParse
  -- ^ The contents of the file to parse
  -> IO (ParseResult ProjectFileSource ProjectConfigSkeleton)
parseProject rootPath cacheDir httpTransport verbosity configToParse = do
  let (dir, projectFileName) = splitFileName rootPath
  projectDir <- makeAbsolute dir
  projectPath <- canonicalizeConfigPath projectDir (ProjectConfigPath $ projectFileName :| [])
  parseProjectSkeleton cacheDir httpTransport verbosity projectDir projectPath configToParse

parseProjectSkeleton
  :: FilePath
  -> HttpTransport
  -> Verbosity
  -> FilePath
  -- ^ The directory of the project configuration, typically the directory of cabal.project
  -> ProjectConfigPath
  -- ^ The path of the file being parsed, either the root or an import
  -> ProjectConfigToParse
  -- ^ The contents of the file to parse
  -> IO (ParseResult ProjectFileSource ProjectConfigSkeleton)
parseProjectSkeleton cacheDir httpTransport verbosity projectDir source (ProjectConfigToParse bs) = do
  normSource <- canonicalizeConfigPath projectDir source
  res <- (sanityWalkPCS False =<<) <$> liftParseResult (go []) (readPreprocessFields bs)
  pure $ withSource (ProjectFileSource (normSource, bs)) res
  where
    go :: [Field Position] -> [Field Position] -> IO (ParseResult ProjectFileSource ProjectConfigSkeleton)
    go acc (x : xs) = case x of
      (Field (Name pos name) importLines) | name == "import" -> do
        liftParseResult
          ( \importLoc -> do
              let importLocPath = importLoc `consProjectConfigPath` source

              -- Once we canonicalize the import path, we can check for cyclical imports
              normSource <- canonicalizeConfigPath projectDir source
              normLocPath <- canonicalizeConfigPath projectDir importLocPath
              debug verbosity $ "\nimport path, normalized\n=======================\n" ++ render (docProjectConfigPath normLocPath)

              if isCyclicConfigPath normLocPath
                then pure $ parseFatalFailure pos (render $ cyclicalImportMsg normLocPath)
                else do
                  when
                    (isUntrimmedUriConfigPath importLocPath)
                    (noticeDoc verbosity $ untrimmedUriImportMsg (Disp.text "Warning:") importLocPath)
                  let fs = (\z -> CondNode z [normLocPath] mempty) <$> fieldsToConfig normSource (reverse acc)
                  importParseResult <- parseProjectSkeleton cacheDir httpTransport verbosity projectDir importLocPath . ProjectConfigToParse =<< fetchImportConfig normLocPath

                  rest <- go [] xs
                  pure . fmap mconcat . sequence $ [fs, importParseResult, rest]
          )
          (parseImport pos importLines)
      (Section (Name pos "if") args xs') -> do
        subpcs <- go [] xs'
        let fs = fmap singletonProjectConfigSkeleton $ fieldsToConfig source (reverse acc)
        (elseClauses, rest) <- parseElseClauses xs
        let condNode =
              (\c pcs e -> CondNode mempty mempty [CondBranch c pcs e])
                <$> parseConditionConfVar (startOfSection (incPos 2 pos) args) args
                <*> subpcs
                <*> elseClauses
        pure . fmap mconcat . sequence $ [fs, condNode, rest]
      _ -> go (x : acc) xs
    go acc [] = do
      normSource <- canonicalizeConfigPath projectDir source
      pure . fmap singletonProjectConfigSkeleton . fieldsToConfig normSource $ reverse acc

    parseElseClauses :: [Field Position] -> IO (ParseResult ProjectFileSource (Maybe ProjectConfigSkeleton), ParseResult ProjectFileSource ProjectConfigSkeleton)
    parseElseClauses x = case x of
      (Section (Name _pos "else") _args xs' : xs) -> do
        subpcs <- go [] xs'
        rest <- go [] xs
        pure (Just <$> subpcs, rest)
      (Section (Name pos "elif") args xs' : xs) -> do
        subpcs <- go [] xs'
        (elseClauses, rest) <- parseElseClauses xs
        let condNode =
              (\c pcs e -> CondNode mempty mempty [CondBranch c pcs e])
                <$> parseConditionConfVar (startOfSection (incPos 4 pos) args) args
                <*> subpcs
                <*> elseClauses
        pure (Just <$> condNode, rest)
      _ -> (pure Nothing,) <$> go [] x

    parseImport :: Position -> [FieldLine Position] -> ParseResult ProjectFileSource FilePath
    parseImport pos lines' = runFieldParser pos (P.many P.anyChar) cabalSpec lines'

    -- We want a normalized path for @fieldsToConfig@. This eventually surfaces
    -- in solver rejection messages and build messages "this build was affected
    -- by the following (project) config files" so we want all paths shown there
    -- to be relative to the directory of the project, not relative to the file
    -- they were imported from.
    fieldsToConfig :: ProjectConfigPath -> [Field Position] -> ParseResult ProjectFileSource ProjectConfig
    fieldsToConfig sourceConfigPath xs = do
      let (fs, sectionGroups) = partitionFields xs
          sections = concat sectionGroups
      config <- parseFieldGrammarCheckingStanzas cabalSpec fs (projectConfigFieldGrammar sourceConfigPath (knownProgramNames programDb)) stanzas
      config' <- view stateConfig <$> execStateT (goSections programDb sections) (SectionS config)
      return config'

    fetchImportConfig :: ProjectConfigPath -> IO BS.ByteString
    fetchImportConfig (ProjectConfigPath (pci :| _)) = do
      debug verbosity $ "fetching import: " ++ pci
      fetch pci

    fetch :: FilePath -> IO BS.ByteString
    fetch pci = case parseURI (trim pci) of
      Just uri -> do
        let fp = cacheDir </> map (\x -> if isPathSeparator x then '_' else x) (makeValid $ show uri)
        createDirectoryIfMissing True cacheDir
        _ <- downloadURI httpTransport verbosity uri fp
        BS.readFile fp
      Nothing ->
        BS.readFile $
          if isAbsolute pci then pci else coerce projectDir </> pci

    modifiesCompiler :: ProjectConfig -> Bool
    modifiesCompiler pc = isSet projectConfigHcFlavor || isSet projectConfigHcPath || isSet projectConfigHcPkg
      where
        isSet f = f (projectConfigShared pc) /= NoFlag

    sanityWalkPCS :: Bool -> ProjectConfigSkeleton -> ParseResult ProjectFileSource ProjectConfigSkeleton
    sanityWalkPCS underConditional t@(CondNode d _c comps)
      | underConditional && modifiesCompiler d = parseFatalFailure zeroPos "Cannot set compiler in a conditional clause of a cabal project file"
      | otherwise = mapM_ sanityWalkBranch comps >> pure t

    sanityWalkBranch :: CondBranch ConfVar [ProjectConfigPath] ProjectConfig -> ParseResult ProjectFileSource ()
    sanityWalkBranch (CondBranch _c t f) = traverse_ (sanityWalkPCS True) f >> sanityWalkPCS True t >> pure ()

    programDb = defaultProgramDb

startOfSection :: Position -> [SectionArg Position] -> Position
-- The case where we have no args is the start of the section
startOfSection defaultPos [] = defaultPos
-- Otherwise the start of the section is the position of the first argument.
startOfSection _ (cond : _) = sectionArgAnn cond

knownProgramNames :: ProgramDb -> [String]
knownProgramNames programDb = (programName . fst) <$> knownPrograms programDb

-- | Monad in which sections are parsed
type SectionParser src = StateT SectionS (ParseResult src)

-- | State of 'SectionParser'
newtype SectionS = SectionS
  { _stateConfig :: ProjectConfig
  }

stateConfig :: Lens' SectionS ProjectConfig
stateConfig f (SectionS cfg) = SectionS <$> f cfg
{-# INLINEABLE stateConfig #-}

goSections :: ProgramDb -> [Section Position] -> SectionParser src ()
goSections programDb = traverse_ (parseSection programDb)

parseSection :: ProgramDb -> Section Position -> SectionParser src ()
parseSection programDb (MkSection (Name pos name) args secFields)
  | name == "source-repository-package" = do
      verifyNullSubsections
      verifyNullSectionArgs
      srp <- lift $ parseFieldGrammar cabalSpec fields sourceRepositoryPackageGrammar
      stateConfig . L.projectPackagesRepo %= (<> [srp])
  | name == "program-options" = do
      verifyNullSubsections
      verifyNullSectionArgs
      opts' <- lift $ parseProgramArgs programDb fields
      stateConfig . L.projectConfigLocalPackages . L.packageConfigProgramArgs %= (opts' <>)
  | name == "program-locations" = do
      verifyNullSubsections
      verifyNullSectionArgs
      paths' <- lift $ parseProgramPaths programDb fields
      stateConfig . L.projectConfigLocalPackages . L.packageConfigProgramPaths %= (paths' <>)
  | name == "repository" = do
      verifyNullSubsections
      mRepoName <- lift $ parseRepoName pos args
      case mRepoName of
        Just repoName -> do
          remoteRepo <- lift $ parseFieldGrammar cabalSpec fields (remoteRepoGrammar repoName)
          remoteOrLocalRepo <- lift $ postProcessRemoteRepo pos remoteRepo
          case remoteOrLocalRepo of
            Left local -> stateConfig . L.projectConfigShared . L.projectConfigLocalNoIndexRepos %= (<> toNubList [local])
            Right remote -> stateConfig . L.projectConfigShared . L.projectConfigRemoteRepos %= (<> toNubList [remote])
        Nothing -> lift $ parseFailure pos "a 'repository' section requires the repository name as an argument"
  | name == "package" = do
      verifyNullSubsections
      package <- lift $ parsePackageName pos args
      case package of
        Just AllPackages -> do
          packageCfg' <- parsePackageConfig
          stateConfig . L.projectConfigAllPackages %= (packageCfg' <>)
        Just (SpecificPackage packageName) -> do
          packageCfg <- parsePackageConfig
          stateConfig . L.projectConfigSpecificPackage %= (<> MapMappend (Map.singleton packageName packageCfg))
        Nothing -> do
          lift $ parseWarning pos PWTUnknownSection "target package name or * required"
          return ()
  | otherwise = do
      warnInvalidSubsection pos name
  where
    (fields, sections) = partitionFields secFields
    warnInvalidSubsection pos' name' = lift $ parseWarning pos' PWTInvalidSubsection $ "Invalid subsection " ++ show name'
    programNames = knownProgramNames programDb
    verifyNullSubsections = unless (null sections) (warnInvalidSubsection pos name)
    verifyNullSectionArgs = unless (null args) (lift $ parseFailure pos $ "The section '" <> (show name) <> "' takes no arguments")
    parsePackageConfig = do
      packageCfg <- lift $ parseFieldGrammar cabalSpec fields (packageConfigFieldGrammar programNames)
      args' <- lift $ parseProgramArgs programDb fields
      paths <- lift $ parseProgramPaths programDb fields
      return packageCfg{packageConfigProgramPaths = paths, packageConfigProgramArgs = args'}

stanzas :: Set BS.ByteString
stanzas = Set.fromList ["source-repository-package", "program-options", "program-locations", "repository", "package"]

-- | Currently a duplicate of 'Distribution.Client.Config.postProcessRepo' but migrated to Parsec ParseResult.
postProcessRemoteRepo :: Position -> RemoteRepo -> ParseResult src (Either LocalRepo RemoteRepo)
postProcessRemoteRepo pos repo = case uriScheme (remoteRepoURI repo) of
  -- TODO: check that there are no authority, query or fragment
  -- Note: the trailing colon is important
  "file+noindex:" -> do
    let uri = normaliseFileNoIndexURI buildOS $ remoteRepoURI repo
    return $ Left $ LocalRepo (remoteRepoName repo) (uriPath uri) (uriFragment uri == "#shared-cache")
  _ -> do
    when (remoteRepoKeyThreshold repo > length (remoteRepoRootKeys repo)) $
      warning $
        "'key-threshold' for repository "
          ++ show (remoteRepoName repo)
          ++ " higher than number of keys"

    when (not (null (remoteRepoRootKeys repo)) && remoteRepoSecure repo /= Just True) $
      warning $
        "'root-keys' for repository "
          ++ show (remoteRepoName repo)
          ++ " non-empty, but 'secure' not set to True."

    return $ Right repo
    where
      warning msg = parseWarning pos PWTOther msg

parseRepoName :: Position -> [SectionArg Position] -> ParseResult src (Maybe RepoName)
parseRepoName pos args = case args of
  [SecArgName _ secName] -> parseName secName
  [SecArgStr _ secName] -> parseName secName
  [SecArgOther _ secName] -> parseName secName
  _ -> return Nothing
  where
    parseName :: BS.ByteString -> ParseResult src (Maybe RepoName)
    parseName str =
      let repoNameStr = fromUTF8BS str
       in case eitherParsec repoNameStr of
            Left _ -> do
              parseFailure pos ("Invalid repository name" ++ repoNameStr)
              return Nothing
            Right name -> return $ Just name

data PackageConfigTarget = AllPackages | SpecificPackage !PackageName

parsePackageName :: Position -> [SectionArg Position] -> ParseResult src (Maybe PackageConfigTarget)
parsePackageName pos args = case args of
  [SecArgName _ secName] -> parseName secName
  [SecArgStr _ secName] -> parseName secName
  [SecArgOther _ secName] -> parseName secName
  _ -> return Nothing
  where
    parseName secName = case runParsecParser parser "<parsePackageName>" (fieldLineStreamFromBS secName) of
      Left _ -> do
        parseFailure pos ("Invalid package name" ++ fromUTF8BS secName)
        return Nothing
      Right cfgTarget -> return $ pure cfgTarget
    parser :: ParsecParser PackageConfigTarget
    parser =
      P.choice [P.try (P.char '*' >> return AllPackages), SpecificPackage <$> parsec]

-- | Parse fields of a program-options stanza.
parseProgramArgs :: ProgramDb -> Fields Position -> ParseResult src (MapMappend String [String])
parseProgramArgs programDb fields = foldM parseField mempty (filter hasOptionsSuffix $ Map.toList fields)
  where
    parseField programArgs (fieldName, fieldLines) = do
      case readProgramName "-options" programDb fieldName of
        Nothing -> warnUnknownFields fieldName fieldLines >> return programArgs
        Just program -> do
          args <- parseProgramArgsField $ reverse fieldLines
          return $ programArgs <> MapMappend (Map.singleton program args)
    hasOptionsSuffix (fieldName, _) = BS.isSuffixOf "-options" fieldName

-- | Parse fields of a program-locations stanza.
parseProgramPaths :: ProgramDb -> Fields Position -> ParseResult src (MapLast String FilePath)
parseProgramPaths programDb fields = foldM parseField mempty (filter hasLocationSuffix $ Map.toList fields)
  where
    parseField paths (fieldName, fieldLines) = do
      case readProgramName "-location" programDb fieldName of
        Nothing -> warnUnknownFields fieldName fieldLines >> return paths
        Just program -> do
          case fieldLines of
            (MkNamelessField pos lines') : _ -> do
              fp <- runFieldParser pos parsecFilePath cabalSpec lines'
              return $ paths <> MapLast (Map.singleton program fp)
            [] -> return mempty
    hasLocationSuffix (fieldName, _) = BS.isSuffixOf "-location" fieldName

-- | Parse all arguments to a single program in program-options stanza.
-- By processing '[NamelessField Position]', we support multiple occurrences of the field, concatenating the arguments.
parseProgramArgsField :: [NamelessField Position] -> ParseResult src ([String])
parseProgramArgsField fieldLines =
  concat <$> mapM (\(MkNamelessField _ lines') -> parseProgramArgsFieldLines lines') fieldLines

-- | Parse all fieldLines of a single field occurrence in a program-options stanza.
parseProgramArgsFieldLines :: [FieldLine Position] -> ParseResult src [String]
parseProgramArgsFieldLines lines' = return $ splitArgs strLines
  where
    strLines = fieldLinesToString lines'

type FieldSuffix = String

-- | Extract the program name of a <progname> field, allow it to have a suffix such as '-options' and check whether the 'ProgramDB' contains it.
readProgramName :: FieldSuffix -> ProgramDb -> FieldName -> Maybe String
readProgramName suffix programDb fieldName =
  parseProgramName suffix fieldName >>= ((flip lookupKnownProgram) programDb) >>= pure . programName

parseProgramName :: FieldSuffix -> FieldName -> Maybe String
parseProgramName suffix fieldName = case runParsecParser parser "<parseProgramName>" fieldNameStream of
  Left _ -> Nothing
  Right str -> Just str
  where
    parser = P.manyTill P.anyChar (P.try ((P.string suffix)) <* P.eof)
    fieldNameStream = fieldLineStreamFromBS fieldName

-- | Issue a 'PWTUnknownField' warning at all occurrences of a field.
warnUnknownFields :: FieldName -> [NamelessField Position] -> ParseResult src ()
warnUnknownFields fieldName fieldLines = for_ fieldLines (\field -> parseWarning (pos field) PWTUnknownField message)
  where
    message = "Unknown field: " ++ show fieldName
    pos = namelessFieldAnn

cabalSpec :: CabalSpecVersion
cabalSpec = cabalSpecLatest
