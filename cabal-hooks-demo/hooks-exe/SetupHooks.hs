{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module SetupHooks
  ( setupHooks
  ) where

import           Data.List ( nub, sortBy )
import           Data.Ord ( comparing )
import           Distribution.InstalledPackageInfo
                   ( sourcePackageId, installedUnitId )
import           Distribution.Package
                   ( PackageId, UnitId, packageVersion, packageName )
import           Distribution.PackageDescription
                   ( PackageDescription (package), Executable (..)
                   , componentNameRaw )
import           Distribution.Pretty ( prettyShow )
import           Distribution.Simple
                   ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import           Distribution.Simple.BuildPaths ( autogenComponentModulesDir )
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex
                   ( allPackages, dependencyClosure )
import           Distribution.Simple.Setup
                   ( BuildFlags (buildVerbosity), fromFlag )
import           Distribution.Simple.SetupHooks
import           Distribution.Simple.Utils
                   ( rewriteFileEx, createDirectoryIfMissingVerbose )
import           Distribution.Types.PackageName ( PackageName, unPackageName )
import           Distribution.Types.UnqualComponentName
                   ( unUnqualComponentName )
import           Distribution.Verbosity ( Verbosity, normal )
import           System.FilePath ( (</>) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
       noBuildHooks
         { preBuildComponentHook = Just preBuildHook }
    }

preBuildHook :: LocalBuildInfo -> BuildFlags -> TargetInfo -> IO ()
preBuildHook lbi flags tgt
  | CLibName LMainLibName <- componentName $ targetComponent tgt
  = generateBuildModule (fromFlag $ buildVerbosity flags) (localPkgDescr lbi)
      lbi tgt
  | otherwise
  = return ()

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> TargetInfo -> IO ()
generateBuildModule verbosity pkg lbi mainLibTargetInfo = do
  -- Generate a module in the stack library component that lists all the
  -- dependencies of stack (both the library and the executable).
  createDirectoryIfMissingVerbose verbosity True autogenDir
  withExeLBI pkg lbi $ \ _ exeCLBI -> do
    rewriteFileEx normal buildModulePath $ unlines
      [ "module Build_" ++ pkgNm
      , "  ( lib_dep_version"
      , "  ) where"
      , ""
      , "import Data.Version"
      , "lib_dep_version :: Version"
      , "lib_dep_version = makeVersion [0,1]" -- ++ (show $ formatdeps (transDeps mainLibCLBI exeCLBI))
      ]
  where
    mainLibCLBI = targetCLBI mainLibTargetInfo
    autogenDir = autogenComponentModulesDir lbi mainLibCLBI
    pkgNm :: String
    pkgNm = unPackageName' $ package pkg
    buildModulePath = autogenDir </> "Build_" ++ pkgNm ++ ".hs"
    formatdeps = map formatone . sortBy (comparing unPackageName')
    formatone p = unPackageName' p ++ "-" ++ prettyShow (packageVersion p)
    unPackageName' = unPackageName . packageName
    transDeps xs ys =
      either (map sourcePackageId . allPackages) handleDepClosureFailure $ dependencyClosure allInstPkgsIdx availInstPkgIds
      where
        allInstPkgsIdx = installedPkgs lbi
        allInstPkgIds = map installedUnitId $ allPackages allInstPkgsIdx
        -- instPkgIds includes `stack-X.X.X`, which is not a dependency hence is missing from allInstPkgsIdx. Filter that out.
        availInstPkgIds = filter (`elem` allInstPkgIds) $ testDeps xs ys
        handleDepClosureFailure unsatisfied =
          error $
            "Computation of transitive dependencies failed." ++
            if null unsatisfied then "" else " Unresolved dependencies: " ++ show unsatisfied

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [UnitId]
testDeps xs ys = map fst $ nub $ componentPackageDeps xs ++ componentPackageDeps ys
