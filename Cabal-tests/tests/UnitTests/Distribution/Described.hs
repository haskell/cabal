{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module UnitTests.Distribution.Described where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.Described (testDescribed)
import Test.Tasty             (TestTree, testGroup)

import Distribution.Compiler                       (CompilerFlavor, CompilerId)
import Distribution.ModuleName                     (ModuleName)
import Distribution.System                         (Arch, OS)
import Distribution.Types.Dependency               (Dependency)
import Distribution.Types.Flag                     (FlagAssignment, FlagName)
import Distribution.Types.IncludeRenaming          (IncludeRenaming)
import Distribution.Types.Mixin                    (Mixin)
import Distribution.Types.ModuleRenaming           (ModuleRenaming)
import Distribution.Types.PackageId                (PackageIdentifier)
import Distribution.Types.PackageName              (PackageName)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint)
import Distribution.Types.Version                  (Version)
import Distribution.Types.VersionRange             (VersionRange)
import Distribution.Verbosity                      (VerbosityFlags)

-- instances
import Test.QuickCheck.Instances.Cabal ()

tests :: TestTree
tests = testGroup "Described"
    [ testDescribed (Proxy :: Proxy Dependency)
    , testDescribed (Proxy :: Proxy PackageName)
    , testDescribed (Proxy :: Proxy PackageIdentifier)
    , testDescribed (Proxy :: Proxy PackageVersionConstraint)
    , testDescribed (Proxy :: Proxy Version)
    , testDescribed (Proxy :: Proxy VersionRange)
    , testDescribed (Proxy :: Proxy FlagName)
    , testDescribed (Proxy :: Proxy FlagAssignment)
    , testDescribed (Proxy :: Proxy ModuleName)
    , testDescribed (Proxy :: Proxy OS)
    , testDescribed (Proxy :: Proxy Arch)
    , testDescribed (Proxy :: Proxy CompilerFlavor)
    , testDescribed (Proxy :: Proxy CompilerId)
    , testDescribed (Proxy :: Proxy ModuleRenaming)
    , testDescribed (Proxy :: Proxy IncludeRenaming)
    , testDescribed (Proxy :: Proxy Mixin)
    , testDescribed (Proxy :: Proxy VerbosityFlags)
    ]
