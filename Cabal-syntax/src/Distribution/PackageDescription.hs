-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Backwards compatibility reexport of most things you need to know
-- about @.cabal@ files.
module Distribution.PackageDescription
  ( -- * PD and GPD
      module Distribution.Types.PackageDescription
  , module Distribution.Types.GenericPackageDescription

    -- * Components
  , module Distribution.Types.ComponentName

    -- ** Library
  , module Distribution.Types.Library
  , module Distribution.Types.LibraryName
  , module Distribution.Types.LibraryVisibility

    -- ** Executable
  , module Distribution.Types.Executable
  , module Distribution.Types.ExecutableScope

    -- ** TestSuite
  , module Distribution.Types.TestSuite
  , module Distribution.Types.TestType
  , module Distribution.Types.TestSuiteInterface

    -- ** Benchmark
  , module Distribution.Types.Benchmark
  , module Distribution.Types.BenchmarkType
  , module Distribution.Types.BenchmarkInterface

    -- ** Foreign library
  , module Distribution.Types.ForeignLib
  , module Distribution.Types.ForeignLibType
  , module Distribution.Types.ForeignLibOption

    -- * BuildInfo
  , module Distribution.Types.BuildType
  , module Distribution.Types.BuildInfo
  , module Distribution.Types.HookedBuildInfo
  , module Distribution.Types.SetupBuildInfo

    -- * Flags
  , module Distribution.Types.Flag

    -- * Identifiers
  , module Distribution.Types.PackageId
  , module Distribution.Types.PackageName
  , module Distribution.Types.UnqualComponentName

    -- * Pkgconfig
  , module Distribution.Types.PkgconfigName
  , module Distribution.Types.PkgconfigVersion
  , module Distribution.Types.PkgconfigVersionRange

    -- * Dependencies
  , module Distribution.Types.Dependency
  , module Distribution.Types.ExeDependency
  , module Distribution.Types.LegacyExeDependency
  , module Distribution.Types.PkgconfigDependency

    -- * Condition trees
  , module Distribution.Types.CondTree
  , module Distribution.Types.Condition
  , module Distribution.Types.ConfVar

    -- * Mixin
  , module Distribution.Types.IncludeRenaming
  , module Distribution.Types.Mixin
  , module Distribution.Types.ModuleReexport
  , module Distribution.Types.ModuleRenaming

    -- * Source repository
  , module Distribution.Types.SourceRepo
  ) where

import Prelude ()

-- import Distribution.Compat.Prelude

import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BenchmarkType
import Distribution.Types.BuildInfo
import Distribution.Types.BuildType
import Distribution.Types.ComponentName
import Distribution.Types.CondTree
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.Executable
import Distribution.Types.ExecutableScope
import Distribution.Types.Flag
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.GenericPackageDescription
import Distribution.Types.HookedBuildInfo
import Distribution.Types.IncludeRenaming
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.Mixin
import Distribution.Types.ModuleReexport
import Distribution.Types.ModuleRenaming
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.SetupBuildInfo
import Distribution.Types.SourceRepo
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Types.TestType
import Distribution.Types.UnqualComponentName
