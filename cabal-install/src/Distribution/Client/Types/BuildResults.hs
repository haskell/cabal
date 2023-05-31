{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.BuildResults
  ( BuildOutcome
  , BuildOutcomes
  , BuildFailure (..)
  , BuildResult (..)
  , TestsResult (..)
  , DocsResult (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Types.PackageId (PackageId)
import Distribution.Types.UnitId (UnitId)

-- | A summary of the outcome for building a single package.
type BuildOutcome = Either BuildFailure BuildResult

-- | A summary of the outcome for building a whole set of packages.
type BuildOutcomes = Map UnitId BuildOutcome

data BuildFailure
  = PlanningFailed
  | DependentFailed PackageId
  | GracefulFailure String
  | DownloadFailed SomeException
  | UnpackFailed SomeException
  | ConfigureFailed SomeException
  | BuildFailed SomeException
  | TestsFailed SomeException
  | InstallFailed SomeException
  deriving (Show, Typeable, Generic)

instance Exception BuildFailure

-- Note that the @Maybe InstalledPackageInfo@ is a slight hack: we only
-- the public library's 'InstalledPackageInfo' is stored here, even if
-- there were 'InstalledPackageInfo' from internal libraries.  This
-- 'InstalledPackageInfo' is not used anyway, so it makes no difference.
data BuildResult
  = BuildResult
      DocsResult
      TestsResult
      (Maybe InstalledPackageInfo)
  deriving (Show, Generic)

data DocsResult = DocsNotTried | DocsFailed | DocsOk
  deriving (Show, Generic, Typeable)
data TestsResult = TestsNotTried | TestsOk
  deriving (Show, Generic, Typeable)

instance Binary BuildFailure
instance Binary BuildResult
instance Binary DocsResult
instance Binary TestsResult

instance Structured BuildFailure
instance Structured BuildResult
instance Structured DocsResult
instance Structured TestsResult
