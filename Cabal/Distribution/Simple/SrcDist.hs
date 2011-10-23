-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.SrcDist
-- Copyright   :  Simon Marlow 2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This handles the @sdist@ command. The module exports an 'sdist' action but
-- also some of the phases that make it up so that other tools can use just the
-- bits they need. In particular the preparation of the tree of files to go
-- into the source tarball is separated from actually building the source
-- tarball.
--
-- The 'createArchive' action uses the external @tar@ program and assumes that
-- it accepts the @-z@ flag. Neither of these assumptions are valid on Windows.
-- The 'sdist' action now also does some distribution QA checks.

{- Copyright (c) 2003-2004, Simon Marlow
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

-- NOTE: FIX: we don't have a great way of testing this module, since
-- we can't easily look inside a tarball once its created.

module Distribution.Simple.SrcDist (
  -- * The top level action
  sdist,

  -- ** Parts of 'sdist'
  printPackageProblems,
  prepareTree,
  createArchive,

  -- ** Snaphots
  prepareSnapshotTree,
  snapshotPackage,
  snapshotVersion,
  dateToSnapshotNumber,
  )  where

import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), Executable(..), Library(..)
         , TestSuite(..), TestSuiteInterface(..), Benchmark(..)
         , BenchmarkInterface(..) )
import Distribution.PackageDescription.Check
         ( PackageCheck(..), checkConfiguredPackage, checkPackageFiles )
import Distribution.Package
         ( PackageIdentifier(pkgVersion), Package(..), packageVersion )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Version
         ( Version(versionBranch) )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, withUTF8FileContents, writeUTF8File
         , installOrdinaryFile, installOrdinaryFiles, setFileExecutable
         , findFile, findFileWithExtension, matchFileGlob
         , withTempDirectory, defaultPackageDesc
         , die, warn, notice, setupMessage )
import Distribution.Simple.Setup (SDistFlags(..), fromFlag, flagToMaybe)
import Distribution.Simple.PreProcess (PPSuffixHandler, ppSuffixes, preprocessComponent)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), withComponentsLBI )
import Distribution.Simple.BuildPaths ( autogenModuleName )
import Distribution.Simple.Program ( defaultProgramConfiguration, requireProgram,
                              rawSystemProgram, tarProgram )
import Distribution.Text
         ( display )

import Control.Monad(when, unless)
import Data.Char (toLower)
import Data.List (partition, isPrefixOf)
import Data.Maybe (isNothing, catMaybes)
import System.Time (getClockTime, toCalendarTime, CalendarTime(..))
import System.Directory
         ( doesFileExist, Permissions(executable), getPermissions )
import Distribution.Verbosity (Verbosity)
import System.FilePath
         ( (</>), (<.>), takeDirectory, dropExtension, isAbsolute )

-- |Create a source distribution.
sdist :: PackageDescription -- ^information from the tarball
      -> Maybe LocalBuildInfo -- ^Information from configure
      -> SDistFlags -- ^verbosity & snapshot
      -> (FilePath -> FilePath) -- ^build prefix (temp dir)
      -> [PPSuffixHandler]  -- ^ extra preprocessors (includes suffixes)
      -> IO ()
sdist pkg mb_lbi flags mkTmpDir pps = do

  -- do some QA
  printPackageProblems verbosity pkg

  when (isNothing mb_lbi) $
    warn verbosity "Cannot run preprocessors. Run 'configure' command first."

  date <- toCalendarTime =<< getClockTime
  let pkg' | snapshot  = snapshotPackage date pkg
           | otherwise = pkg

  case flagToMaybe (sDistDirectory flags) of
    Just targetDir -> do
      generateSourceDir targetDir pkg'
      notice verbosity $ "Source directory created: " ++ targetDir

    Nothing -> do
      createDirectoryIfMissingVerbose verbosity True tmpTargetDir
      withTempDirectory verbosity tmpTargetDir "sdist." $ \tmpDir -> do
        let targetDir = tmpDir </> tarBallName pkg'
        generateSourceDir targetDir pkg'
        targzFile <- createArchive verbosity pkg' mb_lbi tmpDir targetPref
        notice verbosity $ "Source tarball created: " ++ targzFile

  where
    generateSourceDir targetDir pkg' = do

      setupMessage verbosity "Building source dist for" (packageId pkg')
      prepareTree verbosity pkg' mb_lbi distPref targetDir pps
      when snapshot $
        overwriteSnapshotPackageDesc verbosity pkg' targetDir

    verbosity = fromFlag (sDistVerbosity flags)
    snapshot  = fromFlag (sDistSnapshot flags)

    distPref     = fromFlag $ sDistDistPref flags
    targetPref   = distPref
    tmpTargetDir = mkTmpDir distPref


-- |Prepare a directory tree of source files.
prepareTree :: Verbosity          -- ^verbosity
            -> PackageDescription -- ^info from the cabal file
            -> Maybe LocalBuildInfo
            -> FilePath           -- ^dist dir
            -> FilePath           -- ^source tree to populate
            -> [PPSuffixHandler]  -- ^extra preprocessors (includes suffixes)
            -> IO ()
prepareTree verbosity pkg_descr0 mb_lbi distPref targetDir pps = do
  createDirectoryIfMissingVerbose verbosity True targetDir

  -- maybe move the library files into place
  withLib $ \Library { exposedModules = modules, libBuildInfo = libBi } ->
    prepareDir verbosity pkg_descr distPref targetDir pps modules libBi

  -- move the executables into place
  withExe $ \Executable { modulePath = mainPath, buildInfo = exeBi } -> do
    prepareDir verbosity pkg_descr distPref targetDir pps [] exeBi
    srcMainFile <- do
      ppFile <- findFileWithExtension (ppSuffixes pps) (hsSourceDirs exeBi) (dropExtension mainPath)
      case ppFile of
        Nothing -> findFile (hsSourceDirs exeBi) mainPath
        Just pp -> return pp
    copyFileTo verbosity targetDir srcMainFile

  -- move the test suites into place
  withTest $ \t -> do
    let bi = testBuildInfo t
        prep = prepareDir verbosity pkg_descr distPref targetDir pps
    case testInterface t of
        TestSuiteExeV10 _ mainPath -> do
            prep [] bi
            srcMainFile <- do
                ppFile <- findFileWithExtension (ppSuffixes pps)
                                                (hsSourceDirs bi)
                                                (dropExtension mainPath)
                case ppFile of
                    Nothing -> findFile (hsSourceDirs bi) mainPath
                    Just pp -> return pp
            copyFileTo verbosity targetDir srcMainFile
        TestSuiteLibV09 _ m -> do
            prep [m] bi
        TestSuiteUnsupported tp -> die $ "Unsupported test suite type: " ++ show tp

  -- move the benchmarks into place
  withBenchmark $ \bm -> do
    let bi = benchmarkBuildInfo bm
        prep = prepareDir verbosity pkg_descr distPref targetDir pps
    case benchmarkInterface bm of
        BenchmarkExeV10 _ mainPath -> do
            prep [] bi
            srcMainFile <- do
                ppFile <- findFileWithExtension (ppSuffixes pps)
                                                (hsSourceDirs bi)
                                                (dropExtension mainPath)
                case ppFile of
                    Nothing -> findFile (hsSourceDirs bi) mainPath
                    Just pp -> return pp
            copyFileTo verbosity targetDir srcMainFile
        BenchmarkUnsupported tp -> die $ "Unsupported benchmark type: " ++ show tp

  flip mapM_ (dataFiles pkg_descr) $ \ filename -> do
    files <- matchFileGlob (dataDir pkg_descr </> filename)
    let dir = takeDirectory (dataDir pkg_descr </> filename)
    createDirectoryIfMissingVerbose verbosity True (targetDir </> dir)
    sequence_ [ installOrdinaryFile verbosity file (targetDir </> file)
              | file <- files ]

  when (not (null (licenseFile pkg_descr))) $
    copyFileTo verbosity targetDir (licenseFile pkg_descr)
  flip mapM_ (extraSrcFiles pkg_descr) $ \ fpath -> do
    files <- matchFileGlob fpath
    sequence_
      [ do copyFileTo verbosity targetDir file
           -- preserve executable bit on extra-src-files like ./configure
           perms <- getPermissions file
           when (executable perms) --only checks user x bit
                (setFileExecutable (targetDir </> file))
      | file <- files ]

  -- copy the install-include files
  withLib $ \ l -> do
    let lbi = libBuildInfo l
        relincdirs = "." : filter (not.isAbsolute) (includeDirs lbi)
    incs <- mapM (findInc relincdirs) (installIncludes lbi)
    flip mapM_ incs $ \(_,fpath) ->
       copyFileTo verbosity targetDir fpath

  -- if the package was configured then we can run platform independent
  -- pre-processors and include those generated files
  case mb_lbi of
    Just lbi | not (null pps) -> do
      let lbi' = lbi{ buildDir = targetDir </> buildDir lbi }   
      withComponentsLBI pkg_descr lbi' $ \c _ ->
        preprocessComponent pkg_descr c lbi' True verbosity pps
    _ -> return ()

  -- setup isn't listed in the description file.
  hsExists <- doesFileExist "Setup.hs"
  lhsExists <- doesFileExist "Setup.lhs"
  if hsExists then copyFileTo verbosity targetDir "Setup.hs"
    else if lhsExists then copyFileTo verbosity targetDir "Setup.lhs"
    else writeUTF8File (targetDir </> "Setup.hs") $ unlines [
                "import Distribution.Simple",
                "main = defaultMain"]
  -- the description file itself
  descFile <- defaultPackageDesc verbosity
  installOrdinaryFile verbosity descFile (targetDir </> descFile)

  where
    pkg_descr = mapAllBuildInfo filterAutogenModule pkg_descr0
    filterAutogenModule bi = bi {
      otherModules = filter (/=autogenModule) (otherModules bi)
    }
    autogenModule = autogenModuleName pkg_descr0

    findInc [] f = die ("can't find include file " ++ f)
    findInc (d:ds) f = do
      let path = (d </> f)
      b <- doesFileExist path
      if b then return (f,path) else findInc ds f

    -- We have to deal with all libs and executables, so we have local
    -- versions of these functions that ignore the 'buildable' attribute:
    withLib action = maybe (return ()) action (library pkg_descr)
    withExe action = mapM_ action (executables pkg_descr)
    withTest action = mapM_ action (testSuites pkg_descr)
    withBenchmark action = mapM_ action (benchmarks pkg_descr)

-- | Prepare a directory tree of source files for a snapshot version.
-- It is expected that the appropriate snapshot version has already been set
-- in the package description, eg using 'snapshotPackage' or 'snapshotVersion'.
--
prepareSnapshotTree :: Verbosity          -- ^verbosity
                    -> PackageDescription -- ^info from the cabal file
                    -> Maybe LocalBuildInfo
                    -> FilePath           -- ^dist dir
                    -> FilePath           -- ^source tree to populate
                    -> [PPSuffixHandler]  -- ^extra preprocessors (includes suffixes)
                    -> IO ()
prepareSnapshotTree verbosity pkg mb_lbi distPref targetDir pps = do
  prepareTree verbosity pkg mb_lbi distPref targetDir pps
  overwriteSnapshotPackageDesc verbosity pkg targetDir

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

-- | Modifies a 'PackageDescription' by appending a snapshot number
-- corresponding to the given date.
--
snapshotPackage :: CalendarTime -> PackageDescription -> PackageDescription
snapshotPackage date pkg =
  pkg {
    package = pkgid { pkgVersion = snapshotVersion date (pkgVersion pkgid) }
  }
  where pkgid = packageId pkg

-- | Modifies a 'Version' by appending a snapshot number corresponding
-- to the given date.
--
snapshotVersion :: CalendarTime -> Version -> Version
snapshotVersion date version = version {
    versionBranch = versionBranch version
                 ++ [dateToSnapshotNumber date]
  }

-- | Given a date produce a corresponding integer representation.
-- For example given a date @18/03/2008@ produce the number @20080318@.
--
dateToSnapshotNumber :: CalendarTime -> Int
dateToSnapshotNumber date = year  * 10000
                          + month * 100
                          + day
  where
    year  = ctYear date
    month = fromEnum (ctMonth date) + 1
    day   = ctDay date

-- |Create an archive from a tree of source files, and clean up the tree.
createArchive :: Verbosity            -- ^verbosity
              -> PackageDescription   -- ^info from cabal file
              -> Maybe LocalBuildInfo -- ^info from configure
              -> FilePath             -- ^source tree to archive
              -> FilePath             -- ^name of archive to create
              -> IO FilePath

createArchive verbosity pkg_descr mb_lbi tmpDir targetPref = do
  let tarBallFilePath = targetPref </> tarBallName pkg_descr <.> "tar.gz"

  (tarProg, _) <- requireProgram verbosity tarProgram
                    (maybe defaultProgramConfiguration withPrograms mb_lbi)

   -- Hmm: I could well be skating on thinner ice here by using the -C option (=> GNU tar-specific?)
   -- [The prev. solution used pipes and sub-command sequences to set up the paths correctly,
   -- which is problematic in a Windows setting.]
  rawSystemProgram verbosity tarProg
           ["-C", tmpDir, "-czf", tarBallFilePath, tarBallName pkg_descr]
  return tarBallFilePath

-- |Move the sources into place based on buildInfo
prepareDir :: Verbosity -- ^verbosity
           -> PackageDescription -- ^info from the cabal file
           -> FilePath           -- ^dist dir
           -> FilePath  -- ^TargetPrefix
           -> [PPSuffixHandler]  -- ^ extra preprocessors (includes suffixes)
           -> [ModuleName]  -- ^Exposed modules
           -> BuildInfo
           -> IO ()
prepareDir verbosity _pkg _distPref inPref pps modules bi
    = do let searchDirs = hsSourceDirs bi
         sources <- sequence
           [ let file = ModuleName.toFilePath module_
              in findFileWithExtension suffixes searchDirs file
             >>= maybe (notFound module_) return
           | module_ <- modules ++ otherModules bi ]
         bootFiles <- sequence
           [ let file = ModuleName.toFilePath module_
                 fileExts = ["hs-boot", "lhs-boot"]
              in findFileWithExtension fileExts (hsSourceDirs bi) file
           | module_ <- modules ++ otherModules bi ]

         let allSources = sources ++ catMaybes bootFiles ++ cSources bi
         installOrdinaryFiles verbosity inPref (zip (repeat []) allSources)

    where suffixes = ppSuffixes pps ++ ["hs", "lhs"]
          notFound m = die $ "Error: Could not find module: " ++ display m
                          ++ " with any suffix: " ++ show suffixes

copyFileTo :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileTo verbosity dir file = do
  let targetFile = dir </> file
  createDirectoryIfMissingVerbose verbosity True (takeDirectory targetFile)
  installOrdinaryFile verbosity file targetFile

printPackageProblems :: Verbosity -> PackageDescription -> IO ()
printPackageProblems verbosity pkg_descr = do
  ioChecks      <- checkPackageFiles pkg_descr "."
  let pureChecks = checkConfiguredPackage pkg_descr
      isDistError (PackageDistSuspicious _) = False
      isDistError _                         = True
      (errors, warnings) = partition isDistError (pureChecks ++ ioChecks)
  unless (null errors) $
      notice verbosity $ "Distribution quality errors:\n"
                      ++ unlines (map explanation errors)
  unless (null warnings) $
      notice verbosity $ "Distribution quality warnings:\n"
                      ++ unlines (map explanation warnings)
  unless (null errors) $
      notice verbosity
        "Note: the public hackage server would reject this package."

------------------------------------------------------------

-- | The name of the tarball without extension
--
tarBallName :: PackageDescription -> String
tarBallName = display . packageId

mapAllBuildInfo :: (BuildInfo -> BuildInfo)
                -> (PackageDescription -> PackageDescription)
mapAllBuildInfo f pkg = pkg {
    library     = fmap mapLibBi (library pkg),
    executables = fmap mapExeBi (executables pkg),
    testSuites  = fmap mapTestBi (testSuites pkg),
    benchmarks  = fmap mapBenchBi (benchmarks pkg)
  }
  where
    mapLibBi lib  = lib { libBuildInfo       = f (libBuildInfo lib) }
    mapExeBi exe  = exe { buildInfo          = f (buildInfo exe) }
    mapTestBi t   = t   { testBuildInfo      = f (testBuildInfo t) }
    mapBenchBi bm = bm  { benchmarkBuildInfo = f (benchmarkBuildInfo bm) }
