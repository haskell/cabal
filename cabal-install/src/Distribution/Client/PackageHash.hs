{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functions to calculate nix-style hashes for package ids.
--
-- The basic idea is simple, hash the combination of:
--
--   * the package tarball
--   * the ids of all the direct dependencies
--   * other local configuration (flags, profiling, etc)
--
-- See 'PackageHashInputs' for a detailed list of what determines the hash.
module Distribution.Client.PackageHash
  ( -- * Calculating package hashes
    PackageHashInputs (..)
  , PackageHashConfigInputs (..)
  , PackageSourceHash
  , hashedInstalledPackageId
  , hashPackageHashInputs
  , renderPackageHashInputs

    -- ** Platform-specific variations
  , hashedInstalledPackageIdLong
  , hashedInstalledPackageIdShort
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.HashValue
import Distribution.Client.Types
  ( InstalledPackageId
  )
import Distribution.Package
  ( PackageId
  , PackageIdentifier (..)
  , PkgconfigName
  , mkComponentId
  )
import Distribution.Simple.Compiler
  ( AbiTag (..)
  , CompilerId
  , DebugInfoLevel (..)
  , OptimisationLevel (..)
  , PackageDB
  , ProfDetailLevel (..)
  , showProfDetailLevel
  )
import Distribution.Simple.InstallDirs
  ( PathTemplate
  , fromPathTemplate
  )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.System
  ( OS (OSX, Windows)
  , Platform
  , buildOS
  )
import Distribution.Types.Flag
  ( FlagAssignment
  , showFlagAssignment
  )
import Distribution.Types.PkgconfigVersion (PkgconfigVersion)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------
-- Calculating package hashes
--

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
-- Note that due to path length limitations on Windows, this function uses
-- a different method on Windows that produces shorted package ids.
-- See 'hashedInstalledPackageIdLong' vs 'hashedInstalledPackageIdShort'.
hashedInstalledPackageId :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageId
  | buildOS == Windows = hashedInstalledPackageIdShort
  | buildOS == OSX = hashedInstalledPackageIdVeryShort
  | otherwise = hashedInstalledPackageIdLong

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
-- This produces large ids with big hashes. It is only suitable for systems
-- without significant path length limitations (ie not Windows).
hashedInstalledPackageIdLong :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageIdLong
  pkghashinputs@PackageHashInputs{pkgHashPkgId, pkgHashComponent} =
    mkComponentId $
      prettyShow pkgHashPkgId -- to be a bit user friendly
        ++ maybe "" displayComponent pkgHashComponent
        ++ "-"
        ++ showHashValue (hashPackageHashInputs pkghashinputs)
    where
      displayComponent :: CD.Component -> String
      displayComponent CD.ComponentLib = ""
      displayComponent (CD.ComponentSubLib s) = "-l-" ++ prettyShow s
      displayComponent (CD.ComponentFLib s) = "-f-" ++ prettyShow s
      displayComponent (CD.ComponentExe s) = "-e-" ++ prettyShow s
      displayComponent (CD.ComponentTest s) = "-t-" ++ prettyShow s
      displayComponent (CD.ComponentBench s) = "-b-" ++ prettyShow s
      displayComponent CD.ComponentSetup = "-setup"

-- | On Windows we have serious problems with path lengths. Windows imposes a
-- maximum path length of 260 chars, and even if we can use the windows long
-- path APIs ourselves, we cannot guarantee that ghc, gcc, ld, ar, etc etc all
-- do so too.
--
-- So our only choice is to limit the lengths of the paths, and the only real
-- way to do that is to limit the size of the 'InstalledPackageId's that we
-- generate. We do this by truncating the package names and versions and also
-- by truncating the hash sizes.
--
-- Truncating the package names and versions is technically ok because they are
-- just included for human convenience, the full source package id is included
-- in the hash.
--
-- Truncating the hash size is disappointing but also technically ok. We
-- rely on the hash primarily for collision avoidance not for any security
-- properties (at least for now).
hashedInstalledPackageIdShort :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageIdShort pkghashinputs@PackageHashInputs{pkgHashPkgId} =
  mkComponentId $
    intercalate
      "-"
      -- max length now 64
      [ truncateStr 14 (prettyShow name)
      , truncateStr 8 (prettyShow version)
      , showHashValue (truncateHash 20 (hashPackageHashInputs pkghashinputs))
      ]
  where
    PackageIdentifier name version = pkgHashPkgId

    -- Truncate a string, with a visual indication that it is truncated.
    truncateStr n s
      | length s <= n = s
      | otherwise = take (n - 1) s ++ "_"

-- | On macOS we shorten the name very aggressively.  The mach-o linker on
-- macOS has a limited load command size, to which the name of the library
-- as well as its relative path (\@rpath) entry count.  To circumvent this,
-- on macOS the libraries are not stored as
--  @store/<libraryname>/libHS<libraryname>.dylib@
-- where libraryname contains the libraries name, version and abi hash, but in
--  @store/lib/libHS<very short libraryname>.dylib@
-- where the very short library name drops all vowels from the package name,
-- and truncates the hash to 4 bytes.
--
-- We therefore we only need one \@rpath entry to @store/lib@ instead of one
-- \@rpath entry for each library. And the reduced library name saves some
-- additional space.
--
-- This however has two major drawbacks:
-- 1) Packages can collide more easily due to the shortened hash.
-- 2) The libraries are *not* prefix relocatable anymore as they all end up
--    in the same @store/lib@ folder.
--
-- The ultimate solution would have to include generating proxy dynamic
-- libraries on macOS, such that the proxy libraries and the linked libraries
-- stay under the load command limit, and the recursive linker is still able
-- to link all of them.
hashedInstalledPackageIdVeryShort :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageIdVeryShort pkghashinputs@PackageHashInputs{pkgHashPkgId} =
  mkComponentId $
    intercalate
      "-"
      [ filter (not . flip elem "aeiou") (prettyShow name)
      , prettyShow version
      , showHashValue (truncateHash 4 (hashPackageHashInputs pkghashinputs))
      ]
  where
    PackageIdentifier name version = pkgHashPkgId

-- | All the information that contributes to a package's hash, and thus its
-- 'InstalledPackageId'.
data PackageHashInputs = PackageHashInputs
  { pkgHashPkgId :: PackageId
  , pkgHashComponent :: Maybe CD.Component
  , pkgHashSourceHash :: PackageSourceHash
  , pkgHashPkgConfigDeps :: Set (PkgconfigName, Maybe PkgconfigVersion)
  , pkgHashDirectDeps :: Set InstalledPackageId
  , pkgHashOtherConfig :: PackageHashConfigInputs
  }

type PackageSourceHash = HashValue

-- | Those parts of the package configuration that contribute to the
-- package hash.
data PackageHashConfigInputs = PackageHashConfigInputs
  { pkgHashCompilerId :: CompilerId
  , pkgHashCompilerABI :: AbiTag
  , pkgHashPlatform :: Platform
  , pkgHashFlagAssignment :: FlagAssignment -- complete not partial
  , pkgHashConfigureScriptArgs :: [String] -- just ./configure for build-type Configure
  , pkgHashVanillaLib :: Bool
  , pkgHashSharedLib :: Bool
  , pkgHashDynExe :: Bool
  , pkgHashFullyStaticExe :: Bool
  , pkgHashGHCiLib :: Bool
  , pkgHashProfLib :: Bool
  , pkgHashProfExe :: Bool
  , pkgHashProfLibDetail :: ProfDetailLevel
  , pkgHashProfExeDetail :: ProfDetailLevel
  , pkgHashCoverage :: Bool
  , pkgHashOptimization :: OptimisationLevel
  , pkgHashSplitObjs :: Bool
  , pkgHashSplitSections :: Bool
  , pkgHashStripLibs :: Bool
  , pkgHashStripExes :: Bool
  , pkgHashDebugInfo :: DebugInfoLevel
  , pkgHashProgramArgs :: Map String [String]
  , pkgHashExtraLibDirs :: [FilePath]
  , pkgHashExtraLibDirsStatic :: [FilePath]
  , pkgHashExtraFrameworkDirs :: [FilePath]
  , pkgHashExtraIncludeDirs :: [FilePath]
  , pkgHashProgPrefix :: Maybe PathTemplate
  , pkgHashProgSuffix :: Maybe PathTemplate
  , pkgHashPackageDbs :: [Maybe PackageDB]
  , -- Haddock options
    pkgHashDocumentation :: Bool
  , pkgHashHaddockHoogle :: Bool
  , pkgHashHaddockHtml :: Bool
  , pkgHashHaddockHtmlLocation :: Maybe String
  , pkgHashHaddockForeignLibs :: Bool
  , pkgHashHaddockExecutables :: Bool
  , pkgHashHaddockTestSuites :: Bool
  , pkgHashHaddockBenchmarks :: Bool
  , pkgHashHaddockInternal :: Bool
  , pkgHashHaddockCss :: Maybe FilePath
  , pkgHashHaddockLinkedSource :: Bool
  , pkgHashHaddockQuickJump :: Bool
  , pkgHashHaddockContents :: Maybe PathTemplate
  , pkgHashHaddockIndex :: Maybe PathTemplate
  , pkgHashHaddockBaseUrl :: Maybe String
  , pkgHashHaddockLib :: Maybe String
  , pkgHashHaddockOutputDir :: Maybe FilePath
  --     TODO: [required eventually] pkgHashToolsVersions     ?
  --     TODO: [required eventually] pkgHashToolsExtraOptions ?
  }
  deriving (Show)

-- | Calculate the overall hash to be used for an 'InstalledPackageId'.
hashPackageHashInputs :: PackageHashInputs -> HashValue
hashPackageHashInputs = hashValue . renderPackageHashInputs

-- | Render a textual representation of the 'PackageHashInputs'.
--
-- The 'hashValue' of this text is the overall package hash.
renderPackageHashInputs :: PackageHashInputs -> LBS.ByteString
renderPackageHashInputs
  PackageHashInputs
    { pkgHashPkgId
    , pkgHashComponent
    , pkgHashSourceHash
    , pkgHashDirectDeps
    , pkgHashPkgConfigDeps
    , pkgHashOtherConfig =
      PackageHashConfigInputs{..}
    } =
    -- The purpose of this somewhat laboured rendering (e.g. why not just
    -- use show?) is so that existing package hashes do not change
    -- unnecessarily when new configuration inputs are added into the hash.

    -- In particular, the assumption is that when a new configuration input
    -- is included into the hash, that existing packages will typically get
    -- the default value for that feature. So if we avoid adding entries with
    -- the default value then most of the time adding new features will not
    -- change the hashes of existing packages and so fewer packages will need
    -- to be rebuilt.

    -- TODO: [nice to have] ultimately we probably want to put this config info
    -- into the ghc-pkg db. At that point this should probably be changed to
    -- use the config file infrastructure so it can be read back in again.
    LBS.pack $
      unlines $
        catMaybes $
          [ entry "pkgid" prettyShow pkgHashPkgId
          , mentry "component" show pkgHashComponent
          , entry "src" showHashValue pkgHashSourceHash
          , entry
              "pkg-config-deps"
              ( intercalate ", "
                  . map
                    ( \(pn, mb_v) ->
                        prettyShow pn
                          ++ case mb_v of
                            Nothing -> ""
                            Just v -> " " ++ prettyShow v
                    )
                  . Set.toList
              )
              pkgHashPkgConfigDeps
          , entry
              "deps"
              ( intercalate ", "
                  . map prettyShow
                  . Set.toList
              )
              pkgHashDirectDeps
          , -- and then all the config
            entry "compilerid" prettyShow pkgHashCompilerId
          , entry "compilerabi" prettyShow pkgHashCompilerABI
          , entry "platform" prettyShow pkgHashPlatform
          , opt "flags" mempty showFlagAssignment pkgHashFlagAssignment
          , opt "configure-script" [] unwords pkgHashConfigureScriptArgs
          , opt "vanilla-lib" True prettyShow pkgHashVanillaLib
          , opt "shared-lib" False prettyShow pkgHashSharedLib
          , opt "dynamic-exe" False prettyShow pkgHashDynExe
          , opt "fully-static-exe" False prettyShow pkgHashFullyStaticExe
          , opt "ghci-lib" False prettyShow pkgHashGHCiLib
          , opt "prof-lib" False prettyShow pkgHashProfLib
          , opt "prof-exe" False prettyShow pkgHashProfExe
          , opt "prof-lib-detail" ProfDetailDefault showProfDetailLevel pkgHashProfLibDetail
          , opt "prof-exe-detail" ProfDetailDefault showProfDetailLevel pkgHashProfExeDetail
          , opt "hpc" False prettyShow pkgHashCoverage
          , opt "optimisation" NormalOptimisation (show . fromEnum) pkgHashOptimization
          , opt "split-objs" False prettyShow pkgHashSplitObjs
          , opt "split-sections" False prettyShow pkgHashSplitSections
          , opt "stripped-lib" False prettyShow pkgHashStripLibs
          , opt "stripped-exe" True prettyShow pkgHashStripExes
          , opt "debug-info" NormalDebugInfo (show . fromEnum) pkgHashDebugInfo
          , opt "extra-lib-dirs" [] unwords pkgHashExtraLibDirs
          , opt "extra-lib-dirs-static" [] unwords pkgHashExtraLibDirsStatic
          , opt "extra-framework-dirs" [] unwords pkgHashExtraFrameworkDirs
          , opt "extra-include-dirs" [] unwords pkgHashExtraIncludeDirs
          , opt "prog-prefix" Nothing (maybe "" fromPathTemplate) pkgHashProgPrefix
          , opt "prog-suffix" Nothing (maybe "" fromPathTemplate) pkgHashProgSuffix
          , opt "package-dbs" [] (unwords . map show) pkgHashPackageDbs
          , opt "documentation" False prettyShow pkgHashDocumentation
          , opt "haddock-hoogle" False prettyShow pkgHashHaddockHoogle
          , opt "haddock-html" False prettyShow pkgHashHaddockHtml
          , opt "haddock-html-location" Nothing (fromMaybe "") pkgHashHaddockHtmlLocation
          , opt "haddock-foreign-libraries" False prettyShow pkgHashHaddockForeignLibs
          , opt "haddock-executables" False prettyShow pkgHashHaddockExecutables
          , opt "haddock-tests" False prettyShow pkgHashHaddockTestSuites
          , opt "haddock-benchmarks" False prettyShow pkgHashHaddockBenchmarks
          , opt "haddock-internal" False prettyShow pkgHashHaddockInternal
          , opt "haddock-css" Nothing (fromMaybe "") pkgHashHaddockCss
          , opt "haddock-hyperlink-source" False prettyShow pkgHashHaddockLinkedSource
          , opt "haddock-quickjump" False prettyShow pkgHashHaddockQuickJump
          , opt "haddock-contents-location" Nothing (maybe "" fromPathTemplate) pkgHashHaddockContents
          , opt "haddock-index-location" Nothing (maybe "" fromPathTemplate) pkgHashHaddockIndex
          , opt "haddock-base-url" Nothing (fromMaybe "") pkgHashHaddockBaseUrl
          , opt "haddock-lib" Nothing (fromMaybe "") pkgHashHaddockLib
          , opt "haddock-output-dir" Nothing (fromMaybe "") pkgHashHaddockOutputDir
          ]
            ++ Map.foldrWithKey (\prog args acc -> opt (prog ++ "-options") [] unwords args : acc) [] pkgHashProgramArgs
    where
      entry key format value = Just (key ++ ": " ++ format value)
      mentry key format value = fmap (\v -> key ++ ": " ++ format v) value
      opt key def format value
        | value == def = Nothing
        | otherwise = entry key format value
