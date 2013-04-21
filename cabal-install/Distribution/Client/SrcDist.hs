-- Implements the \"@.\/cabal sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball, making use of the corresponding Cabal module.
module Distribution.Client.SrcDist (
         sdist
  )  where


import Distribution.Simple.SrcDist
         ( CreateArchiveFun, sdistWith )
import Distribution.Client.Tar (createTarGzFile)

import Distribution.Package
         ( Package(..) )
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Utils
         ( defaultPackageDesc, die )
import Distribution.Client.Setup
         ( SDistFlags(..), SDistExFlags(..), ArchiveFormat(..) )
import Distribution.Simple.Setup
         ( fromFlag )
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Configure(maybeGetPersistBuildConfig)
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Simple.Program (requireProgram, simpleProgram, programPath)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Text
         ( display )

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
  mb_lbi <- maybeGetPersistBuildConfig distPref

  sdistWith pkg mb_lbi flags srcPref knownSuffixHandlers createArchive

  where
    verbosity     = fromFlag (sDistVerbosity flags)
    distPref      = fromFlag (sDistDistPref flags)
    format        = fromFlag (sDistFormat exflags)
    createArchive = case format of
      TargzFormat -> createTarGzArchive
      ZipFormat   -> createZipArchive

tarBallName :: PackageDescription -> String
tarBallName = display . packageId

-- | Create a tar.gz archive from a tree of source files.
createTarGzArchive :: CreateArchiveFun
createTarGzArchive _verbosity pkg _mlbi tmpDir targetPref = do
    createTarGzFile tarBallFilePath tmpDir (tarBallName pkg)
    return tarBallFilePath
  where
    tarBallFilePath = targetPref </> tarBallName pkg <.> "tar.gz"

-- | Create a zip archive from a tree of source files.
createZipArchive :: CreateArchiveFun
createZipArchive verbosity pkg _mlbi tmpDir targetPref = do
    let dir       = tarBallName pkg
        zipfile   = targetPref </> dir <.> "zip"
    (zipProg, _) <- requireProgram verbosity zipProgram emptyProgramDb

    -- zip has an annoying habbit of updating the target rather than creating
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
    return zipfile
  where
    zipProgram = simpleProgram "zip"
