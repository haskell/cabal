{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.BuildReports.Types
-- Copyright   :  (c) Duncan Coutts 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Types related to build reporting
module Distribution.Client.BuildReports.Types
  ( ReportLevel (..)
  , BuildReport (..)
  , InstallOutcome (..)
  , Outcome (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

import Distribution.Compiler (CompilerId (..))
import Distribution.PackageDescription (FlagAssignment)
import Distribution.System (Arch, OS)
import Distribution.Types.PackageId (PackageIdentifier)

-------------------------------------------------------------------------------
-- ReportLevel
-------------------------------------------------------------------------------

data ReportLevel = NoReports | AnonymousReports | DetailedReports
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Binary ReportLevel
instance Structured ReportLevel

instance Pretty ReportLevel where
  pretty NoReports = Disp.text "none"
  pretty AnonymousReports = Disp.text "anonymous"
  pretty DetailedReports = Disp.text "detailed"

instance Parsec ReportLevel where
  parsec = do
    name <- P.munch1 isAlpha
    case lowercase name of
      "none" -> return NoReports
      "anonymous" -> return AnonymousReports
      "detailed" -> return DetailedReports
      _ -> P.unexpected $ "ReportLevel: " ++ name

lowercase :: String -> String
lowercase = map toLower

-------------------------------------------------------------------------------
-- BuildReport
-------------------------------------------------------------------------------

data BuildReport = BuildReport
  { package :: PackageIdentifier
  -- ^ The package this build report is about
  , os :: OS
  -- ^ The OS and Arch the package was built on
  , arch :: Arch
  , compiler :: CompilerId
  -- ^ The Haskell compiler (and hopefully version) used
  , client :: PackageIdentifier
  -- ^ The uploading client, ie cabal-install-x.y.z
  , flagAssignment :: FlagAssignment
  -- ^ Which configurations flags we used
  , dependencies :: [PackageIdentifier]
  -- ^ Which dependent packages we were using exactly
  , installOutcome :: InstallOutcome
  -- ^ Did installing work ok?
  , --   Which version of the Cabal library was used to compile the Setup.hs
    --    cabalVersion    :: Version,

    --   Which build tools we were using (with versions)
    --    tools      :: [PackageIdentifier],

    docsOutcome :: Outcome
  -- ^ Configure outcome, did configure work ok?
  , testsOutcome :: Outcome
  -- ^ Configure outcome, did configure work ok?
  }
  deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------
-- InstallOutcome
-------------------------------------------------------------------------------

data InstallOutcome
  = PlanningFailed
  | DependencyFailed PackageIdentifier
  | DownloadFailed
  | UnpackFailed
  | SetupFailed
  | ConfigureFailed
  | BuildFailed
  | TestsFailed
  | InstallFailed
  | InstallOk
  deriving (Eq, Show, Generic)

instance Pretty InstallOutcome where
  pretty PlanningFailed = Disp.text "PlanningFailed"
  pretty (DependencyFailed pkgid) = Disp.text "DependencyFailed" <+> pretty pkgid
  pretty DownloadFailed = Disp.text "DownloadFailed"
  pretty UnpackFailed = Disp.text "UnpackFailed"
  pretty SetupFailed = Disp.text "SetupFailed"
  pretty ConfigureFailed = Disp.text "ConfigureFailed"
  pretty BuildFailed = Disp.text "BuildFailed"
  pretty TestsFailed = Disp.text "TestsFailed"
  pretty InstallFailed = Disp.text "InstallFailed"
  pretty InstallOk = Disp.text "InstallOk"

instance Parsec InstallOutcome where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "PlanningFailed" -> return PlanningFailed
      "DependencyFailed" -> DependencyFailed <$ P.spaces <*> parsec
      "DownloadFailed" -> return DownloadFailed
      "UnpackFailed" -> return UnpackFailed
      "SetupFailed" -> return SetupFailed
      "ConfigureFailed" -> return ConfigureFailed
      "BuildFailed" -> return BuildFailed
      "TestsFailed" -> return TestsFailed
      "InstallFailed" -> return InstallFailed
      "InstallOk" -> return InstallOk
      _ -> P.unexpected $ "InstallOutcome: " ++ name

-------------------------------------------------------------------------------
-- Outcome
-------------------------------------------------------------------------------

data Outcome = NotTried | Failed | Ok
  deriving (Eq, Show, Enum, Bounded, Generic)

instance Pretty Outcome where
  pretty NotTried = Disp.text "NotTried"
  pretty Failed = Disp.text "Failed"
  pretty Ok = Disp.text "Ok"

instance Parsec Outcome where
  parsec = do
    name <- P.munch1 isAlpha
    case name of
      "NotTried" -> return NotTried
      "Failed" -> return Failed
      "Ok" -> return Ok
      _ -> P.unexpected $ "Outcome: " ++ name
