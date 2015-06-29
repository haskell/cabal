-- | Handling of source packages under a directory root.
module Distribution.Client.SourceTrees (
    readSourcePackagesInDir,
    constrainDepsToSourcePackages,
    isCwdASourceTree
    ) where

import Control.Monad (filterM)
import Data.Either (rights)
import Distribution.Verbosity (Verbosity)
import Distribution.Client.Types (SourcePackage(..))
import System.FilePath ((</>))

import qualified Control.Exception as Exception
import qualified Distribution.Client.Dependency.Types as Dependency.Types
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Client.Types as Types
import qualified Distribution.Package as Package
import qualified Distribution.PackageDescription as PackageDescription
import qualified Distribution.PackageDescription.Parse as Parse
import qualified Distribution.Simple.Utils as Utils
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO.Error as Error

-- | Create a source package index for all packages directly under the
-- given root directory.
readSourcePackagesInDir :: Verbosity -> FilePath
                        -> IO (PackageIndex.PackageIndex SourcePackage)
readSourcePackagesInDir verbosity rootDir = do
  Utils.debug verbosity $ "Looking for source package directories in "
                          ++ rootDir
  dirs <- filterM Directory.doesDirectoryExist
          . map (rootDir </>)
          . filter (\ f -> f /= "." && f /= "..")
          =<< Directory.getDirectoryContents rootDir
  Utils.debug verbosity $ "Founds subdirectories: " ++ show dirs
  cabalFiles <- rights `fmap` mapM findPackageDesc dirs
  Utils.debug verbosity $ "Founds .cabal files: " ++ show cabalFiles
  srcPkgs <- mapM (Parse.readPackageDescription verbosity) cabalFiles
  return $! PackageIndex.fromList $ map (uncurry mkSourcePackage)
            (zip srcPkgs (map FilePath.takeDirectory cabalFiles))
 where
  mkSourcePackage :: PackageDescription.GenericPackageDescription -> FilePath
                  -> SourcePackage
  mkSourcePackage pkg path =
    SourcePackage {
      packageInfoId        = Package.packageId pkg,
      packageDescription   = pkg,
      packageSource        = Types.LocalUnpackedPackage path,
      packageDescrOverride = Nothing
    }

  -- Version of Utils.findPackageDesc that ignores directories we
  -- don't have permission to read.
  findPackageDesc dir =
      Exception.catchJust
      (\ e -> if Error.isPermissionErrorType (Error.ioeGetErrorType e)
              then Just () else Nothing)
      (Utils.findPackageDesc dir)
      (\ _ -> return $! Left errMsg)
    where errMsg = "Permission error when reading contents of: " ++ dir

-- | Create a set of constraints that require that all packages
-- mentioned in the given index come from source packages.
constrainDepsToSourcePackages :: PackageIndex.PackageIndex SourcePackage
                              -> [Dependency.Types.PackageConstraint]
constrainDepsToSourcePackages =
  map (Dependency.Types.PackageConstraintSource . Package.packageName)
  . PackageIndex.allPackages

-- | The CWD is a source tree if it does not contain a .cabal file but
-- does contain a sandbox.
isCwdASourceTree :: IO Bool
isCwdASourceTree = do
    -- TODO: Actually check for the presence of a sandbox.
    cwd <- Directory.getCurrentDirectory
    (null . rights) `fmap` mapM Utils.findPackageDesc [cwd]

readUserTargets :: [String] -> [(a, b)]  -- Simple/Client targets.
readUserTargets = undefined  -- TODO: Continue here.
