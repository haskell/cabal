-- Implements the \"@.\/cabal sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball, making use of the corresponding Cabal module.
module Hackage.SrcDist (
	 sdist
  )  where
import Distribution.Simple.SrcDist (preparePackage,tarBallName,nameVersion)
import Hackage.Tar (createTarGzFile)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.Utils (notice, defaultPackageDesc )
import Distribution.Simple.Setup (SDistFlags(..), fromFlag)
import Control.Exception (finally)
import System.Directory (removeDirectoryRecursive)
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>))
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.BuildPaths ( distPref, srcPref)
import Distribution.Simple.Configure(maybeGetPersistBuildConfig)
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )

-- |Create a source distribution.
sdist :: SDistFlags -> IO ()
sdist flags = do
  let snapshot  = fromFlag (sDistSnapshot flags)
      verbosity = fromFlag (sDistVerbose flags)
  cabalFile <- defaultPackageDesc verbosity
  pkg_descr0 <- readPackageDescription verbosity cabalFile
  mb_lbi <- maybeGetPersistBuildConfig
  let pkg_descr' = (flattenPackageDescription pkg_descr0)
  pkg_descr <- preparePackage pkg_descr' mb_lbi verbosity snapshot srcPref knownSuffixHandlers
  createArchive pkg_descr verbosity srcPref distPref
  return ()

-- |Create an archive from a tree of source files, and clean up the tree.
createArchive :: PackageDescription
                 -> Verbosity
                 -> FilePath
                 -> FilePath
                 -> IO FilePath
createArchive pkg_descr verbosity tmpDir targetPref = do
  let tarBallFilePath = targetPref </> tarBallName pkg_descr
  createTarGzFile tarBallFilePath (Just tmpDir) (nameVersion pkg_descr)
      `finally` removeDirectoryRecursive tmpDir
  notice verbosity $ "Source tarball created: " ++ tarBallFilePath
  return tarBallFilePath
