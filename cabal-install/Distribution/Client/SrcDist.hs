-- Implements the \"@.\/cabal sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball, making use of the corresponding Cabal module.
module Distribution.Client.SrcDist (
         sdist
  )  where


import Distribution.Client.SetupWrapper
        ( SetupScriptOptions(..), defaultSetupScriptOptions, setupWrapper )
import Distribution.Client.Tar (createTarGzFile)

import Distribution.Package
         ( Package(..) )
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, defaultPackageDesc
         , die, notice, withTempDirectory )
import Distribution.Client.Setup
         ( SDistFlags(..), SDistExFlags(..), ArchiveFormat(..) )
import Distribution.Simple.Setup
         ( Flag(..), sdistCommand, flagToList, fromFlag, fromFlagOrDefault )
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Program (requireProgram, simpleProgram, programPath)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Text ( display )
import Distribution.Verbosity (Verbosity)
import Distribution.Version   (Version(..), orLaterVersion)

import System.FilePath ((</>), (<.>))
import Control.Monad (when, unless)
import System.Directory (doesFileExist, removeFile, canonicalizePath)
import System.Process (runProcess, waitForProcess)
import System.Exit    (ExitCode(..))

-- |Create a source distribution.
sdist :: SDistFlags -> SDistExFlags -> IO ()
sdist flags exflags = do
  pkg <- return . flattenPackageDescription
         =<< readPackageDescription verbosity
         =<< defaultPackageDesc verbosity
  let withDir = if not needMakeArchive then (\f -> f tmpTargetDir)
                else withTempDirectory verbosity tmpTargetDir "sdist."
  -- 'withTempDir' fails if we don't create 'tmpTargetDir'...
  when needMakeArchive $
    createDirectoryIfMissingVerbose verbosity True tmpTargetDir
  withDir $ \tmpDir -> do
    let outDir = if isOutDirectory then tmpDir else tmpDir </> tarBallName pkg
        flags' = (if not needMakeArchive then flags
                  else flags { sDistDirectory = Flag outDir })
    unless isListSources $
      createDirectoryIfMissingVerbose verbosity True outDir

    -- Run 'setup sdist --output-directory=tmpDir' (or
    -- '--list-source'/'--output-directory=someOtherDir') in case we were passed
    -- those options.
    setupWrapper verbosity setupOpts (Just pkg) sdistCommand (const flags') []

    -- Unless we were given --list-sources or --output-directory ourselves,
    -- create an archive.
    when needMakeArchive $
      createArchive verbosity pkg tmpDir distPref

    when isOutDirectory $
      notice verbosity $ "Source directory created: " ++ tmpTargetDir

    when isListSources $
      notice verbosity $ "List of package sources written to file '"
                         ++ (fromFlag . sDistListSources $ flags) ++ "'"

  where
    flagEnabled f  = not . null . flagToList . f $ flags

    isListSources   = flagEnabled sDistListSources
    isOutDirectory  = flagEnabled sDistDirectory
    needMakeArchive = not (isListSources || isOutDirectory)
    verbosity       = fromFlag (sDistVerbosity flags)
    distPref        = fromFlag (sDistDistPref flags)
    tmpTargetDir    = fromFlagOrDefault (srcPref distPref) (sDistDirectory flags)
    setupOpts       = defaultSetupScriptOptions {
      -- The '--output-directory' sdist flag was introduced in Cabal 1.12, and
      -- '--list-sources' in 1.17.
      useCabalVersion = if isListSources
                        then orLaterVersion $ Version [1,17,0] []
                        else orLaterVersion $ Version [1,12,0] []
      }
    format        = fromFlag (sDistFormat exflags)
    createArchive = case format of
      TargzFormat -> createTarGzArchive
      ZipFormat   -> createZipArchive

tarBallName :: PackageDescription -> String
tarBallName = display . packageId

-- | Create a tar.gz archive from a tree of source files.
createTarGzArchive :: Verbosity -> PackageDescription -> FilePath -> FilePath
                    -> IO ()
createTarGzArchive verbosity pkg tmpDir targetPref = do
    createTarGzFile tarBallFilePath tmpDir (tarBallName pkg)
    notice verbosity $ "Source tarball created: " ++ tarBallFilePath
  where
    tarBallFilePath = targetPref </> tarBallName pkg <.> "tar.gz"

-- | Create a zip archive from a tree of source files.
createZipArchive :: Verbosity -> PackageDescription -> FilePath -> FilePath
                    -> IO ()
createZipArchive verbosity pkg tmpDir targetPref = do
    let dir       = tarBallName pkg
        zipfile   = targetPref </> dir <.> "zip"
    (zipProg, _) <- requireProgram verbosity zipProgram emptyProgramDb

    -- zip has an annoying habit of updating the target rather than creating
    -- it from scratch. While that might sound like an optimisation, it doesn't
    -- remove files already in the archive that are no longer present in the
    -- uncompressed tree.
    alreadyExists <- doesFileExist zipfile
    when alreadyExists $ removeFile zipfile

    -- We call zip with a different CWD, so have to make the path
    -- absolute. Can't just use 'canonicalizePath zipfile' since this function
    -- requires its argument to refer to an existing file.
    zipfileAbs <- fmap (</> dir <.> "zip") . canonicalizePath $ targetPref

    --TODO: use runProgramInvocation, but has to be able to set CWD
    hnd <- runProcess (programPath zipProg) ["-q", "-r", zipfileAbs, dir]
                      (Just tmpDir)
                      Nothing Nothing Nothing Nothing
    exitCode <- waitForProcess hnd
    unless (exitCode == ExitSuccess) $
      die $ "Generating the zip file failed "
         ++ "(zip returned exit code " ++ show exitCode ++ ")"
    notice verbosity $ "Source zip archive created: " ++ zipfile
  where
    zipProgram = simpleProgram "zip"
