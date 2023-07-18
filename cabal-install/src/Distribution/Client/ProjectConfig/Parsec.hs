{-# LANGUAGE OverloadedStrings #-}

-- | Parsing project configuration.
module Distribution.Client.ProjectConfig.Parsec
  ( -- * Package configuration
    parseProjectSkeleton
  , ProjectConfigSkeleton
  , ProjectConfig (..)

    -- ** Parsing
  , ParseResult
  , runParseResult
  ) where

import Control.Monad.State.Strict (StateT, execStateT, lift, modify)
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.FieldGrammar

-- TODO #6101 .Legacy -> ProjectConfigSkeleton should probably be moved here
import Distribution.Client.ProjectConfig.FieldGrammar (projectConfigFieldGrammar)
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigImport, ProjectConfigSkeleton)
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..))
import Distribution.Client.Types.SourceRepo (SourceRepoList, sourceRepositoryPackageGrammar)
import Distribution.Fields.ConfVar (parseConditionConfVar)
import Distribution.Fields.ParseResult

-- AST type
import Distribution.Fields (Field, Name (..), readFields')
import Distribution.Fields.LexerMonad (LexWarning, toPWarnings)
import Distribution.PackageDescription.Quirks (patchQuirks)
import Distribution.Parsec (parsec, simpleParsecBS)
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Warning (PWarnType (..))
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Utils.Generic (breakMaybe, fromUTF8BS, toUTF8BS, unfoldrM, validateUTF8)

import qualified Data.ByteString as BS
import qualified Text.Parsec as P

-- | Preprocess file and start parsing
parseProjectSkeleton :: BS.ByteString -> ParseResult ProjectConfigSkeleton
parseProjectSkeleton bs = do
  case readFields' bs' of
    Right (fs, lexWarnings) -> do
      parseWarnings (toPWarnings lexWarnings)
      for_ invalidUtf8 $ \pos ->
        parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
      parseCondTree fs
    Left perr -> parseFatalFailure pos (show perr)
      where
        ppos = P.errorPos perr
        pos = Position (P.sourceLine ppos) (P.sourceColumn ppos)
  where
    invalidUtf8 = validateUTF8 bs
    bs' = case invalidUtf8 of
      Nothing -> bs
      Just _ -> toUTF8BS (fromUTF8BS bs)

-- List of conditional blocks
newtype Conditional ann = Conditional [Section ann]
  deriving (Eq, Show)

-- | Separate valid conditional blocks from other sections so
-- all conditionals form their own groups.
-- TODO implement
partitionConditionals :: [[Section ann]] -> ([Section ann], [Conditional ann])
partitionConditionals sections = (concat sections, [])

parseCondTree
  :: [Field Position]
  -> ParseResult ProjectConfigSkeleton
parseCondTree fields0 = do
  -- sections are groups of sections between fields
  let (fs, sectionGroups) = partitionFields fields0
      (sections, conditionals) = partitionConditionals sectionGroups
      msg = show sectionGroups
  imports <- parseImports fs
  config <- parseFieldGrammar cabalSpecLatest fs projectConfigFieldGrammar
  config' <- view stateConfig <$> execStateT (goSections sections) (SectionS config)
  let configSkeleton = CondNode config' imports []
  -- TODO parse conditionals
  return configSkeleton

-- Monad in which sections are parsed
type SectionParser = StateT SectionS ParseResult

-- | State of section parser
newtype SectionS = SectionS
  { _stateConfig :: ProjectConfig
  }

stateConfig :: Lens' SectionS ProjectConfig
stateConfig f (SectionS cfg) = SectionS <$> f cfg
{-# INLINEABLE stateConfig #-}

goSections :: [Section Position] -> SectionParser ()
goSections = traverse_ parseSection

parseSection :: Section Position -> SectionParser ()
parseSection (MkSection (Name pos name) args secFields)
  | name == "source-repository-package" = do
      let (fields, secs) = partitionFields secFields
      srp <- lift $ parseFieldGrammar cabalSpecLatest fields sourceRepositoryPackageGrammar
      stateConfig . L.projectPackagesRepo %= (++ [srp])
      unless (null secs) (warnInvalidSubsection pos name)
  | otherwise = do
      warnInvalidSubsection pos name

warnInvalidSubsection pos name = lift $ parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name

-- TODO implement, caution: check for cyclical imports
parseImports :: Fields Position -> ParseResult [ProjectConfigImport]
parseImports fs = return mempty
