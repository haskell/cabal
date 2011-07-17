-- Implements the \"@.\/cabal sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball, making use of the corresponding Cabal module.
module Distribution.Client.SrcDist (
         sdist
  )  where
import Distribution.Simple.SrcDist
         ( printPackageProblems, prepareTree, snapshotPackage )
import Distribution.Client.Tar (createTarGzFile)

import Distribution.Package
         ( Package(..), packageVersion )
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Utils
         ( defaultPackageDesc, warn, notice, setupMessage
         , createDirectoryIfMissingVerbose, withTempDirectory
         , withUTF8FileContents, writeUTF8File )
import Distribution.Simple.Setup
         ( SDistFlags(..), fromFlag, flagToMaybe )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Configure(maybeGetPersistBuildConfig)
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )
import Distribution.Text
         ( display )
import Distribution.Version
         ( Version )

import System.Time (getClockTime, toCalendarTime)
import System.FilePath ((</>), (<.>))
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Char (toLower)
import Data.List (isPrefixOf)

-- |Create a source distribution.
sdist :: SDistFlags -> IO ()
sdist flags = do
  pkg <- return . flattenPackageDescription
     =<< readPackageDescription verbosity
     =<< defaultPackageDesc verbosity
  mb_lbi <- maybeGetPersistBuildConfig distPref

  -- do some QA
  printPackageProblems verbosity pkg

  when (isNothing mb_lbi) $
    warn verbosity "Cannot run preprocessors. Run 'configure' command first."

  date <- toCalendarTime =<< getClockTime
  let pkg' | snapshot  = snapshotPackage date pkg
           | otherwise = pkg

  case flagToMaybe (sDistDirectory flags) of
    Just targetDir -> do
      generateSourceDir targetDir pkg' mb_lbi
      notice verbosity $ "Source directory created: " ++ targetDir

    Nothing -> do
      createDirectoryIfMissingVerbose verbosity True tmpTargetDir
      withTempDirectory verbosity tmpTargetDir "sdist." $ \tmpDir -> do
        let targetDir = tmpDir </> tarBallName pkg'
        generateSourceDir targetDir pkg' mb_lbi
        targzFile <- createArchive verbosity pkg' tmpDir targetPref
        notice verbosity $ "Source tarball created: " ++ targzFile

  where
    generateSourceDir targetDir pkg' mb_lbi = do

      setupMessage verbosity "Building source dist for" (packageId pkg')
      prepareTree verbosity pkg' mb_lbi distPref targetDir pps
      when snapshot $
        overwriteSnapshotPackageDesc verbosity pkg' targetDir

    verbosity = fromFlag (sDistVerbosity flags)
    snapshot  = fromFlag (sDistSnapshot flags)
    pps       = knownSuffixHandlers
    distPref     = fromFlag $ sDistDistPref flags
    targetPref   = distPref
    tmpTargetDir = srcPref distPref

tarBallName :: PackageDescription -> String
tarBallName = display . packageId

overwriteSnapshotPackageDesc :: Verbosity          -- ^verbosity
                             -> PackageDescription -- ^info from the cabal file
                             -> FilePath           -- ^source tree
                             -> IO ()
overwriteSnapshotPackageDesc verbosity pkg targetDir = do
    -- We could just writePackageDescription targetDescFile pkg_descr,
    -- but that would lose comments and formatting.
    descFile <- defaultPackageDesc verbosity
    withUTF8FileContents descFile $
      writeUTF8File (targetDir </> descFile)
        . unlines . map (replaceVersion (packageVersion pkg)) . lines

  where
    replaceVersion :: Version -> String -> String
    replaceVersion version line
      | "version:" `isPrefixOf` map toLower line
                  = "version: " ++ display version
      | otherwise = line

-- |Create an archive from a tree of source files, and clean up the tree.
createArchive :: Verbosity
              -> PackageDescription
              -> FilePath
              -> FilePath
              -> IO FilePath
createArchive _verbosity pkg tmpDir targetPref = do
    createTarGzFile tarBallFilePath tmpDir (tarBallName pkg)
    return tarBallFilePath
  where
    tarBallFilePath = targetPref </> tarBallName pkg <.> "tar.gz"
