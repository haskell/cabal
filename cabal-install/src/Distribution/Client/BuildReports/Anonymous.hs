{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Reporting
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Anonymous build report data structure, printing and parsing
module Distribution.Client.BuildReports.Anonymous
  ( BuildReport (..)
  , InstallOutcome (..)
  , Outcome (..)

    -- * Constructing and writing reports
  , newBuildReport

    -- * parsing and pretty printing
  , parseBuildReport
  , parseBuildReportList
  , showBuildReport
  , cabalInstallID
  --    showList,
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Client.BuildReports.Types
import Distribution.Client.Version (cabalInstallVersion)
import Distribution.Compiler (CompilerId (..))
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.Package (PackageIdentifier (..), mkPackageName)
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Parsec
import Distribution.System (Arch, OS)

import qualified Distribution.Client.BuildReports.Lens as L
import qualified Distribution.Client.Types as BR (BuildFailure (..), BuildOutcome, BuildResult (..), DocsResult (..), TestsResult (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

-------------------------------------------------------------------------------
-- New
-------------------------------------------------------------------------------

newBuildReport
  :: OS
  -> Arch
  -> CompilerId
  -> PackageIdentifier
  -> FlagAssignment
  -> [PackageIdentifier]
  -> BR.BuildOutcome
  -> BuildReport
newBuildReport os' arch' comp pkgid flags deps result =
  BuildReport
    { package = pkgid
    , os = os'
    , arch = arch'
    , compiler = comp
    , client = cabalInstallID
    , flagAssignment = flags
    , dependencies = deps
    , installOutcome = convertInstallOutcome
    , --    cabalVersion          = undefined
      docsOutcome = convertDocsOutcome
    , testsOutcome = convertTestsOutcome
    }
  where
    convertInstallOutcome = case result of
      Left BR.PlanningFailed -> PlanningFailed
      Left (BR.GracefulFailure _) -> PlanningFailed
      Left (BR.DependentFailed p) -> DependencyFailed p
      Left (BR.DownloadFailed _) -> DownloadFailed
      Left (BR.UnpackFailed _) -> UnpackFailed
      Left (BR.ConfigureFailed _) -> ConfigureFailed
      Left (BR.BuildFailed _) -> BuildFailed
      Left (BR.TestsFailed _) -> TestsFailed
      Left (BR.InstallFailed _) -> InstallFailed
      Right (BR.BuildResult _ _ _) -> InstallOk
    convertDocsOutcome = case result of
      Left _ -> NotTried
      Right (BR.BuildResult BR.DocsNotTried _ _) -> NotTried
      Right (BR.BuildResult BR.DocsFailed _ _) -> Failed
      Right (BR.BuildResult BR.DocsOk _ _) -> Ok
    convertTestsOutcome = case result of
      Left (BR.TestsFailed _) -> Failed
      Left _ -> NotTried
      Right (BR.BuildResult _ BR.TestsNotTried _) -> NotTried
      Right (BR.BuildResult _ BR.TestsOk _) -> Ok

cabalInstallID :: PackageIdentifier
cabalInstallID =
  PackageIdentifier (mkPackageName "cabal-install") cabalInstallVersion

-------------------------------------------------------------------------------
-- FieldGrammar
-------------------------------------------------------------------------------

fieldDescrs
  :: ( Applicative (g BuildReport)
     , FieldGrammar c g
     , c (Identity Arch)
     , c (Identity CompilerId)
     , c (Identity FlagAssignment)
     , c (Identity InstallOutcome)
     , c (Identity OS)
     , c (Identity Outcome)
     , c (Identity PackageIdentifier)
     , c (List VCat (Identity PackageIdentifier) PackageIdentifier)
     )
  => g BuildReport BuildReport
fieldDescrs =
  BuildReport
    <$> uniqueField "package" L.package
    <*> uniqueField "os" L.os
    <*> uniqueField "arch" L.arch
    <*> uniqueField "compiler" L.compiler
    <*> uniqueField "client" L.client
    <*> monoidalField "flags" L.flagAssignment
    <*> monoidalFieldAla "dependencies" (alaList VCat) L.dependencies
    <*> uniqueField "install-outcome" L.installOutcome
    <*> uniqueField "docs-outcome" L.docsOutcome
    <*> uniqueField "tests-outcome" L.testsOutcome

-- -----------------------------------------------------------------------------
-- Parsing

parseBuildReport :: BS.ByteString -> Either String BuildReport
parseBuildReport s = case snd $ runParseResult $ parseFields s of
  Left (_, perrors) -> Left $ unlines [err | PError _ err <- toList perrors]
  Right report -> Right report

parseFields :: BS.ByteString -> ParseResult BuildReport
parseFields input = do
  fields <- either (parseFatalFailure zeroPos . show) pure $ readFields input
  case partitionFields fields of
    (fields', []) -> parseFieldGrammar CabalSpecV2_4 fields' fieldDescrs
    _otherwise -> parseFatalFailure zeroPos "found sections in BuildReport"

parseBuildReportList :: BS.ByteString -> [BuildReport]
parseBuildReportList str =
  [report | Right report <- map parseBuildReport (split str)]
  where
    split :: BS.ByteString -> [BS.ByteString]
    split = filter (not . BS.null) . unfoldr chunk . BS8.lines
    chunk [] = Nothing
    chunk ls = case break BS.null ls of
      (r, rs) -> Just (BS8.unlines r, dropWhile BS.null rs)

-- -----------------------------------------------------------------------------
-- Pretty-printing

showBuildReport :: BuildReport -> String
showBuildReport = showFields (const NoComment) . prettyFieldGrammar CabalSpecV2_4 fieldDescrs
