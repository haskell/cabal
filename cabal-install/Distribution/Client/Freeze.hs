-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Freeze
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal freeze command
-----------------------------------------------------------------------------
module Distribution.Client.Freeze (
    freeze,
  ) where

import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.InstallPlan
         ( PlanPackage )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Setup
         ( GlobalFlags(..), FreezeFlags(..) )
import Distribution.Client.Sandbox.PackageEnvironment
         ( userPackageEnvironmentFile )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )

import Distribution.Package
         ( packageId, packageName, packageVersion )
import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDBStack )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Setup
         ( fromFlag )
import Distribution.Simple.Utils
         ( die, notice, debug, intercalate, writeFileAtomic )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.Version
         ( showVersion )

-- ------------------------------------------------------------
-- * The freeze command
-- ------------------------------------------------------------

--TODO:
-- * Don't overwrite all of `cabal.config`, just the constaints section.
-- * Should the package represented by `UserTargetLocalDir "."` be
--   constrained too? What about `base`?


-- | Freeze all of the dependencies by writing a constraints section
-- constraining each dependency to an exact version.
--
freeze :: Verbosity
      -> PackageDBStack
      -> [Repo]
      -> Compiler
      -> Platform
      -> ProgramConfiguration
      -> Maybe SandboxPackageInfo
      -> GlobalFlags
      -> FreezeFlags
      -> IO ()
freeze verbosity packageDBs repos comp platform conf mSandboxPkgInfo
      globalFlags freezeFlags = do

    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
    sourcePkgDb       <- getSourcePackages    verbosity repos

    pkgSpecifiers <- resolveUserTargets verbosity
                       (fromFlag $ globalWorldFile globalFlags)
                       (packageIndex sourcePkgDb)
                       [UserTargetLocalDir "."]

    pkgs  <- planPackages
               verbosity comp platform mSandboxPkgInfo freezeFlags
               installedPkgIndex sourcePkgDb pkgSpecifiers

    if null pkgs
      then notice verbosity $ "No packages to be frozen. "
                           ++ "As this package has no dependencies."
      else if dryRun
             then notice verbosity $ unlines $
                     "The following packages would be frozen:"
                   : formatPkgs pkgs

             else freezePackages pkgs

  where
    dryRun = fromFlag (freezeDryRun freezeFlags)


planPackages :: Verbosity
             -> Compiler
             -> Platform
             -> Maybe SandboxPackageInfo
             -> FreezeFlags
             -> PackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> IO [PlanPackage]
planPackages verbosity comp platform mSandboxPkgInfo freezeFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers = do

  solver <- chooseSolver verbosity
            (fromFlag (freezeSolver freezeFlags)) (compilerId comp)
  notice verbosity "Resolving dependencies..."

  installPlan <- foldProgress logMsg die return $
                   resolveDependencies
                     platform (compilerId comp)
                     solver
                     resolverParams

  return $ InstallPlan.toList installPlan

  where
    resolverParams =

        setMaxBackjumps (if maxBackjumps < 0 then Nothing
                                             else Just maxBackjumps)

      . setIndependentGoals independentGoals

      . setReorderGoals reorderGoals

      . setShadowPkgs shadowPkgs

      . maybe id applySandboxInstallPolicy mSandboxPkgInfo

      $ standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

    logMsg message rest = debug verbosity message >> rest

    reorderGoals     = fromFlag (freezeReorderGoals     freezeFlags)
    independentGoals = fromFlag (freezeIndependentGoals freezeFlags)
    shadowPkgs       = fromFlag (freezeShadowPkgs       freezeFlags)
    maxBackjumps     = fromFlag (freezeMaxBackjumps     freezeFlags)

freezePackages :: [PlanPackage] -> IO ()
freezePackages pkgs =
    writeFileAtomic userPackageEnvironmentFile $ constraints pkgs
  where
    constraints = BS.Char8.pack
                . (++ "\n")
                . (prefix' ++)
                . intercalate separator
                . formatPkgs
    prefix' = "constraints: "
    separator = "\n" ++ (replicate (length prefix' - 2) ' ') ++ ", "


formatPkgs :: [PlanPackage] -> [String]
formatPkgs = map $ showPkg . packageId
  where
    showPkg pid = name pid ++ " == " ++ version pid
    name = display . packageName
    version = showVersion . packageVersion
