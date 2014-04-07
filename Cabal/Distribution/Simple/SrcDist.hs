-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.SrcDist
-- Copyright   :  Simon Marlow 2004
-- License     :  BSD3
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

  -- * Extracting the source files
  listPackageSources

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
         , installOrdinaryFiles, installMaybeExecutableFiles
         , findFile, findFileWithExtension, matchFileGlob
         , withTempDirectory, defaultPackageDesc
         , die, warn, notice, setupMessage )
import Distribution.Simple.Setup ( Flag(..), SDistFlags(..)
                                 , fromFlag, flagToMaybe)
import Distribution.Simple.PreProcess ( PPSuffixHandler, ppSuffixes
                                      , preprocessComponent )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), withAllComponentsInBuildOrder )
import Distribution.Simple.BuildPaths ( autogenModuleName )
import Distribution.Simple.Program ( defaultProgramConfiguration, requireProgram,
                              rawSystemProgram, tarProgram )
import Distribution.Text
         ( display )

import Control.Monad(when, unless, forM)
import Data.Char (toLower)
import Data.List (partition, isPrefixOf)
import Data.Maybe (isNothing, catMaybes)
import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import System.Directory ( doesFileExist )
import System.IO (IOMode(WriteMode), hPutStrLn, withFile)
import Distribution.Verbosity (Verbosity)
import System.FilePath
         ( (</>), (<.>), dropExtension, isAbsolute )

-- |Create a source distribution.
sdist :: PackageDescription     -- ^information from the tarball
      -> Maybe LocalBuildInfo   -- ^Information from configure
      -> SDistFlags             -- ^verbosity & snapshot
      -> (FilePath -> FilePath) -- ^build prefix (temp dir)
      -> [PPSuffixHandler]      -- ^ extra preprocessors (includes suffixes)
      -> IO ()
sdist pkg mb_lbi flags mkTmpDir pps =

  -- When given --list-sources, just output the list of sources to a file.
  case (sDistListSources flags) of
    Flag path -> withFile path WriteMode $ \outHandle -> do
      (ordinary, maybeExecutable) <- listPackageSources verbosity pkg pps
      mapM_ (hPutStrLn outHandle) ordinary
      mapM_ (hPutStrLn outHandle) maybeExecutable
      notice verbosity $ "List of package sources written to file '"
                         ++ path ++ "'"
    NoFlag    -> do
      -- do some QA
      printPackageProblems verbosity pkg

      when (isNothing mb_lbi) $
        warn verbosity "Cannot run preprocessors. Run 'configure' command first."

      date <- getCurrentTime
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
      prepareTree verbosity pkg' mb_lbi targetDir pps
      when snapshot $
        overwriteSnapshotPackageDesc verbosity pkg' targetDir

    verbosity = fromFlag (sDistVerbosity flags)
    snapshot  = fromFlag (sDistSnapshot flags)

    distPref     = fromFlag $ sDistDistPref flags
    targetPref   = distPref
    tmpTargetDir = mkTmpDir distPref

-- | List all source files of a package. Returns a tuple of lists: first
-- component is a list of ordinary files, second one is a list of those files
-- that may be executable.
listPackageSources :: Verbosity          -- ^ verbosity
                   -> PackageDescription -- ^ info from the cabal file
                   -> [PPSuffixHandler]  -- ^ extra preprocessors (include
                                         -- suffixes)
                   -> IO ([FilePath], [FilePath])
listPackageSources verbosity pkg_descr0 pps = do
  -- Call helpers that actually do all work.
  ordinary        <- listPackageSourcesOrdinary        verbosity pkg_descr pps
  maybeExecutable <- listPackageSourcesMaybeExecutable pkg_descr
  return (ordinary, maybeExecutable)
  where
    pkg_descr = filterAutogenModule pkg_descr0

-- | List those source files that may be executable (e.g. the configure script).
listPackageSourcesMaybeExecutable :: PackageDescription -> IO [FilePath]
listPackageSourcesMaybeExecutable pkg_descr =
  -- Extra source files.
  fmap concat . forM (extraSrcFiles pkg_descr) $ \fpath -> matchFileGlob fpath

-- | List those source files that should be copied with ordinary permissions.
listPackageSourcesOrdinary :: Verbosity
                           -> PackageDescription
                           -> [PPSuffixHandler]
                           -> IO [FilePath]
listPackageSourcesOrdinary verbosity pkg_descr pps =
  fmap concat . sequence $
  [
    -- Library sources.
    withLib $ \Library { exposedModules = modules, libBuildInfo = libBi } ->
     allSourcesBuildInfo libBi pps modules

    -- Executables sources.
  , fmap concat
    . withExe $ \Executable { modulePath = mainPath, buildInfo = exeBi } -> do
       biSrcs  <- allSourcesBuildInfo exeBi pps []
       mainSrc <- findMainExeFile exeBi pps mainPath
       return (mainSrc:biSrcs)

    -- Test suites sources.
  , fmap concat
    . withTest $ \t -> do
       let bi  = testBuildInfo t
       case testInterface t of
         TestSuiteExeV10 _ mainPath -> do
           biSrcs <- allSourcesBuildInfo bi pps []
           srcMainFile <- do
             ppFile <- findFileWithExtension (ppSuffixes pps)
                       (hsSourceDirs bi) (dropExtension mainPath)
             case ppFile of
               Nothing -> findFile (hsSourceDirs bi) mainPath
               Just pp -> return pp
           return (srcMainFile:biSrcs)
         TestSuiteLibV09 _ m ->
           allSourcesBuildInfo bi pps [m]
         TestSuiteUnsupported tp -> die $ "Unsupported test suite type: "
                                   ++ show tp

    -- Benchmarks sources.
  , fmap concat
    . withBenchmark $ \bm -> do
       let  bi = benchmarkBuildInfo bm
       case benchmarkInterface bm of
         BenchmarkExeV10 _ mainPath -> do
           biSrcs <- allSourcesBuildInfo bi pps []
           srcMainFile <- do
             ppFile <- findFileWithExtension (ppSuffixes pps)
                       (hsSourceDirs bi) (dropExtension mainPath)
             case ppFile of
               Nothing -> findFile (hsSourceDirs bi) mainPath
               Just pp -> return pp
           return (srcMainFile:biSrcs)
         BenchmarkUnsupported tp -> die $ "Unsupported benchmark type: "
                                    ++ show tp

    -- Data files.
  , fmap concat
    . forM (dataFiles pkg_descr) $ \filename ->
       matchFileGlob (dataDir pkg_descr </> filename)

    -- Extra doc files.
  , fmap concat
    . forM (extraDocFiles pkg_descr) $ \ filename ->
      matchFileGlob filename

    -- License file(s).
  , return (licenseFiles pkg_descr)

    -- Install-include files.
  , withLib $ \ l -> do
       let lbi = libBuildInfo l
           relincdirs = "." : filter (not.isAbsolute) (includeDirs lbi)
       mapM (fmap snd . findIncludeFile relincdirs) (installIncludes lbi)

    -- Setup script, if it exists.
  , fmap (maybe [] (\f -> [f])) $ findSetupFile ""

    -- The .cabal file itself.
  , fmap (\d -> [d]) (defaultPackageDesc verbosity)

  ]
  where
    -- We have to deal with all libs and executables, so we have local
    -- versions of these functions that ignore the 'buildable' attribute:
    withLib       action = maybe (return []) action (library pkg_descr)
    withExe       action = mapM action (executables pkg_descr)
    withTest      action = mapM action (testSuites pkg_descr)
    withBenchmark action = mapM action (benchmarks pkg_descr)


-- |Prepare a directory tree of source files.
prepareTree :: Verbosity          -- ^verbosity
            -> PackageDescription -- ^info from the cabal file
            -> Maybe LocalBuildInfo
            -> FilePath           -- ^source tree to populate
            -> [PPSuffixHandler]  -- ^extra preprocessors (includes suffixes)
            -> IO ()
prepareTree verbosity pkg_descr0 mb_lbi targetDir pps = do
  -- If the package was configured then we can run platform-independent
  -- pre-processors and include those generated files.
  case mb_lbi of
    Just lbi | not (null pps) -> do
      let lbi' = lbi{ buildDir = targetDir </> buildDir lbi }
      withAllComponentsInBuildOrder pkg_descr lbi' $ \c _ ->
        preprocessComponent pkg_descr c lbi' True verbosity pps
    _ -> return ()

  (ordinary, mExecutable)  <- listPackageSources verbosity pkg_descr0 pps
  installOrdinaryFiles        verbosity targetDir (zip (repeat []) ordinary)
  installMaybeExecutableFiles verbosity targetDir (zip (repeat []) mExecutable)
  maybeCreateDefaultSetupScript targetDir

  where
    pkg_descr = filterAutogenModule pkg_descr0

-- | Find the setup script file, if it exists.
findSetupFile :: FilePath -> IO (Maybe FilePath)
findSetupFile targetDir = do
  hsExists  <- doesFileExist setupHs
  lhsExists <- doesFileExist setupLhs
  if hsExists
    then return (Just setupHs)
    else if lhsExists
         then return (Just setupLhs)
         else return Nothing
    where
      setupHs  = targetDir </> "Setup.hs"
      setupLhs = targetDir </> "Setup.lhs"

-- | Create a default setup script in the target directory, if it doesn't exist.
maybeCreateDefaultSetupScript :: FilePath -> IO ()
maybeCreateDefaultSetupScript targetDir = do
  mSetupFile <- findSetupFile targetDir
  case mSetupFile of
    Just _setupFile -> return ()
    Nothing         -> do
      writeUTF8File (targetDir </> "Setup.hs") $ unlines [
        "import Distribution.Simple",
        "main = defaultMain"]

-- | Find the main executable file.
findMainExeFile :: BuildInfo -> [PPSuffixHandler] -> FilePath -> IO FilePath
findMainExeFile exeBi pps mainPath = do
  ppFile <- findFileWithExtension (ppSuffixes pps) (hsSourceDirs exeBi)
            (dropExtension mainPath)
  case ppFile of
    Nothing -> findFile (hsSourceDirs exeBi) mainPath
    Just pp -> return pp

-- | Given a list of include paths, try to find the include file named
-- @f@. Return the name of the file and the full path, or exit with error if
-- there's no such file.
findIncludeFile :: [FilePath] -> String -> IO (String, FilePath)
findIncludeFile [] f = die ("can't find include file " ++ f)
findIncludeFile (d:ds) f = do
  let path = (d </> f)
  b <- doesFileExist path
  if b then return (f,path) else findIncludeFile ds f

-- | Remove the auto-generated module ('Paths_*') from 'exposed-modules' and
-- 'other-modules'.
filterAutogenModule :: PackageDescription -> PackageDescription
filterAutogenModule pkg_descr0 = mapLib filterAutogenModuleLib $
                                 mapAllBuildInfo filterAutogenModuleBI pkg_descr0
  where
    mapLib f pkg = pkg { library = fmap f (library pkg) }
    filterAutogenModuleLib lib = lib {
      exposedModules = filter (/=autogenModule) (exposedModules lib)
    }
    filterAutogenModuleBI bi = bi {
      otherModules   = filter (/=autogenModule) (otherModules bi)
    }
    autogenModule = autogenModuleName pkg_descr0

-- | Prepare a directory tree of source files for a snapshot version.
-- It is expected that the appropriate snapshot version has already been set
-- in the package description, eg using 'snapshotPackage' or 'snapshotVersion'.
--
prepareSnapshotTree :: Verbosity          -- ^verbosity
                    -> PackageDescription -- ^info from the cabal file
                    -> Maybe LocalBuildInfo
                    -> FilePath           -- ^source tree to populate
                    -> [PPSuffixHandler]  -- ^extra preprocessors (includes
                                          -- suffixes)
                    -> IO ()
prepareSnapshotTree verbosity pkg mb_lbi targetDir pps = do
  prepareTree verbosity pkg mb_lbi targetDir pps
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
snapshotPackage :: UTCTime -> PackageDescription -> PackageDescription
snapshotPackage date pkg =
  pkg {
    package = pkgid { pkgVersion = snapshotVersion date (pkgVersion pkgid) }
  }
  where pkgid = packageId pkg

-- | Modifies a 'Version' by appending a snapshot number corresponding
-- to the given date.
--
snapshotVersion :: UTCTime -> Version -> Version
snapshotVersion date version = version {
    versionBranch = versionBranch version
                 ++ [dateToSnapshotNumber date]
  }

-- | Given a date produce a corresponding integer representation.
-- For example given a date @18/03/2008@ produce the number @20080318@.
--
dateToSnapshotNumber :: UTCTime -> Int
dateToSnapshotNumber date = case toGregorian (utctDay date) of
                            (year, month, day) ->
                                fromIntegral year * 10000
                              + month             * 100
                              + day

-- | Callback type for use by sdistWith.
type CreateArchiveFun = Verbosity               -- ^verbosity
                        -> PackageDescription   -- ^info from cabal file
                        -> Maybe LocalBuildInfo -- ^info from configure
                        -> FilePath             -- ^source tree to archive
                        -> FilePath             -- ^name of archive to create
                        -> IO FilePath

-- | Create an archive from a tree of source files, and clean up the tree.
createArchive :: CreateArchiveFun
createArchive verbosity pkg_descr mb_lbi tmpDir targetPref = do
  let tarBallFilePath = targetPref </> tarBallName pkg_descr <.> "tar.gz"

  (tarProg, _) <- requireProgram verbosity tarProgram
                    (maybe defaultProgramConfiguration withPrograms mb_lbi)

   -- Hmm: I could well be skating on thinner ice here by using the -C option
   -- (=> GNU tar-specific?)  [The prev. solution used pipes and sub-command
   -- sequences to set up the paths correctly, which is problematic in a Windows
   -- setting.]
  rawSystemProgram verbosity tarProg
           ["-C", tmpDir, "-czf", tarBallFilePath, tarBallName pkg_descr]
  return tarBallFilePath

-- | Given a buildinfo, return the names of all source files.
allSourcesBuildInfo :: BuildInfo
                       -> [PPSuffixHandler] -- ^ Extra preprocessors
                       -> [ModuleName]      -- ^ Exposed modules
                       -> IO [FilePath]
allSourcesBuildInfo bi pps modules = do
  let searchDirs = hsSourceDirs bi
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

  return $ sources ++ catMaybes bootFiles ++ cSources bi

  where
    suffixes = ppSuffixes pps ++ ["hs", "lhs"]
    notFound m = die $ "Error: Could not find module: " ++ display m
                 ++ " with any suffix: " ++ show suffixes


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
