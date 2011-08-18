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
         ( defaultPackageDesc, die, warn, notice, setupMessage
         , createDirectoryIfMissingVerbose, withTempDirectory
         , withUTF8FileContents, writeUTF8File )
import Distribution.Client.Setup
         ( SDistFlags(..), SDistExFlags(..), ArchiveFormat(..) )
import Distribution.Simple.Setup
         ( fromFlag, flagToMaybe )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Configure(maybeGetPersistBuildConfig)
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )
import Distribution.Simple.Program (requireProgram, simpleProgram, programPath)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Text
         ( display )
import Distribution.Version
         ( Version )

import System.Time (getClockTime, toCalendarTime)
import System.FilePath ((</>), (<.>))
import Control.Monad (when, unless)
import Data.Maybe (isNothing)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, removeFile, canonicalizePath)
import System.Process (runProcess, waitForProcess)
import System.Exit    (ExitCode(..))

-- |Create a source distribution.
sdist :: SDistFlags -> SDistExFlags -> IO ()
sdist flags exflags = do
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
        targzFile <- createArchive verbosity format pkg' tmpDir targetPref
        notice verbosity $ "Source tarball created: " ++ targzFile

  where
    generateSourceDir targetDir pkg' mb_lbi = do

      setupMessage verbosity "Building source dist for" (packageId pkg')
      prepareTree verbosity pkg' mb_lbi distPref targetDir pps
      when snapshot $
        overwriteSnapshotPackageDesc verbosity pkg' targetDir

    verbosity = fromFlag (sDistVerbosity flags)
    snapshot  = fromFlag (sDistSnapshot flags)
    format    = fromFlag (sDistFormat exflags)
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

-- | Create an archive from a tree of source files.
--
createArchive :: Verbosity
              -> ArchiveFormat
              -> PackageDescription
              -> FilePath
              -> FilePath
              -> IO FilePath
createArchive _verbosity TargzFormat pkg tmpDir targetPref = do
    createTarGzFile tarBallFilePath tmpDir (tarBallName pkg)
    return tarBallFilePath
  where
    tarBallFilePath = targetPref </> tarBallName pkg <.> "tar.gz"

createArchive verbosity ZipFormat pkg tmpDir targetPref = do
    createZipFile verbosity zipFilePath tmpDir (tarBallName pkg)
    return zipFilePath
  where
    zipFilePath = targetPref </> tarBallName pkg <.> "zip"

createZipFile :: Verbosity -> FilePath -> FilePath -> FilePath -> IO ()
createZipFile verbosity zipfile base dir = do
    (zipProg, _) <- requireProgram verbosity zipProgram emptyProgramDb

    -- zip has an annoying habbit of updating the target rather than creating
    -- it from scratch. While that might sound like an optimisation, it doesn't
    -- remove files already in the archive that are no longer present in the
    -- uncompressed tree.
    alreadyExists <- doesFileExist zipfile
    when alreadyExists $ removeFile zipfile

    -- we call zip with a different CWD, so have to make the path absolute
    zipfileAbs <- canonicalizePath zipfile

    --TODO: use runProgramInvocation, but has to be able to set CWD
    hnd <- runProcess (programPath zipProg) ["-q", "-r", zipfileAbs, dir] (Just base)
                      Nothing Nothing Nothing Nothing
    exitCode <- waitForProcess hnd
    unless (exitCode == ExitSuccess) $
      die $ "Generating the zip file failed "
         ++ "(zip returned exit code " ++ show exitCode ++ ")"
  where
    zipProgram = simpleProgram "zip"
