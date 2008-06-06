-- Implements the \"@.\/cabal sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball, making use of the corresponding Cabal module.
module Hackage.SrcDist (
	 sdist
  )  where
import Distribution.Simple.SrcDist
         ( printPackageProblems, prepareTree, prepareSnapshotTree )
import Hackage.Tar (createTarGzFile)

import Distribution.Package
         ( Package(..) )
import Distribution.PackageDescription
         ( PackageDescription, readPackageDescription )
import Distribution.Simple.Utils
         ( withTempDirectory , defaultPackageDesc
         , die, warn, notice, setupMessage )
import Distribution.Simple.Setup (SDistFlags(..), fromFlag)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Configure(maybeGetPersistBuildConfig)
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )
import Distribution.Text
         ( display )

import System.Time (getClockTime, toCalendarTime)
import System.FilePath ((</>), (<.>))
import System.Directory (doesDirectoryExist)
import Control.Monad (when)
import Data.Maybe (isNothing)

-- |Create a source distribution.
sdist :: SDistFlags -> IO ()
sdist flags = do
  pkg <- return . flattenPackageDescription
     =<< readPackageDescription verbosity
     =<< defaultPackageDesc verbosity
  mb_lbi <- maybeGetPersistBuildConfig distPref
  let tmpDir = srcPref distPref

  -- do some QA
  printPackageProblems verbosity pkg

  exists <- doesDirectoryExist tmpDir
  when exists $
    die $ "Source distribution already in place. please move or remove: "
       ++ tmpDir

  when (isNothing mb_lbi) $
    warn verbosity "Cannot run preprocessors. Run 'configure' command first."

  withTempDirectory verbosity tmpDir $ do

    setupMessage verbosity "Building source dist for" (packageId pkg)
    if snapshot
      then getClockTime >>= toCalendarTime
       >>= prepareSnapshotTree verbosity pkg mb_lbi tmpDir knownSuffixHandlers
      else prepareTree         verbosity pkg mb_lbi tmpDir knownSuffixHandlers
    targzFile <- createArchive verbosity pkg tmpDir distPref
    notice verbosity $ "Source tarball created: " ++ targzFile

  where
    verbosity = fromFlag (sDistVerbosity flags)
    snapshot  = fromFlag (sDistSnapshot flags)
    distPref  = fromFlag (sDistDistPref flags)

-- |Create an archive from a tree of source files, and clean up the tree.
createArchive :: Verbosity
              -> PackageDescription
              -> FilePath
              -> FilePath
              -> IO FilePath
createArchive _verbosity pkg tmpDir targetPref = do
  let tarBallName     = display (packageId pkg)
      tarBallFilePath = targetPref </> tarBallName <.> "tar.gz"
  createTarGzFile tarBallFilePath tmpDir tarBallName
  return tarBallFilePath
