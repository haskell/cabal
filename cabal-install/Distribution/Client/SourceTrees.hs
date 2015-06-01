-- | Handling of source packages under a directory root.
module Distribution.Client.SourceTrees (
    readSourcePackagesInDir
    ) where

import Control.Monad (filterM)
import Data.Either (rights)
import Distribution.Verbosity (Verbosity)
import Distribution.Client.Types (SourcePackage(..))
import System.FilePath ((</>))

import qualified Distribution.Package as Package
import qualified Distribution.PackageDescription as PackageDescription
import qualified Distribution.PackageDescription.Parse as Parse
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Simple.Utils as Utils
import qualified Distribution.Client.Types as Types
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

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
  cabalFiles <- rights `fmap` mapM Utils.findPackageDesc dirs
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
