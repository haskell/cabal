{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.PreProcess
-- Copyright   :  (c) 2003-2005, Isaac Jones, Malcolm Wallace
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines a 'PreProcessor' abstraction which represents a pre-processor
-- that can transform one kind of file into another. There is also a
-- 'PPSuffixHandler' which is a combination of a file extension and a function
-- for configuring a 'PreProcessor'. It defines a bunch of known built-in
-- preprocessors like @cpp@, @cpphs@, @c2hs@, @hsc2hs@, @happy@, @alex@ etc and
-- lists them in 'knownSuffixHandlers'. On top of this it provides a function
-- for actually preprocessing some sources given a bunch of known suffix
-- handlers. This module is not as good as it could be, it could really do with
-- a rewrite to address some of the problems we have with pre-processors.
module Distribution.Simple.PreProcess
  ( preprocessComponent
  , preprocessExtras
  , knownSuffixHandlers
  , ppSuffixes
  , PPSuffixHandler
  , PreProcessor (..)
  , mkSimplePreProcessor
  , runSimplePreProcessor
  , ppCpp
  , ppCpp'
  , ppGreenCard
  , ppC2hs
  , ppHsc2hs
  , ppHappy
  , ppAlex
  , ppUnlit
  , platformDefines
  , unsorted
  )
where

import Distribution.Compat.Prelude
import Distribution.Compat.Stack
import Prelude ()

import Distribution.Backpack.DescribeUnitId
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.Simple.BuildPaths
import Distribution.Simple.CCompiler
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PreProcess.Unlit
import Distribution.Simple.Program
import Distribution.Simple.Program.ResponseFile
import Distribution.Simple.Test.LibV09
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.PackageName.Magic
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
  ( dropExtensions
  , normalise
  , replaceExtension
  , splitExtension
  , takeDirectory
  , takeExtensions
  , (<.>)
  , (</>)
  )
import System.Info (arch, os)

-- | The interface to a preprocessor, which may be implemented using an
--  external program, but need not be.  The arguments are the name of
--  the input file, the name of the output file and a verbosity level.
--  Here is a simple example that merely prepends a comment to the given
--  source file:
--
--  > ppTestHandler :: PreProcessor
--  > ppTestHandler =
--  >   PreProcessor {
--  >     platformIndependent = True,
--  >     runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
--  >       do info verbosity (inFile++" has been preprocessed to "++outFile)
--  >          stuff <- readFile inFile
--  >          writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
--  >          return ExitSuccess
--
--  We split the input and output file names into a base directory and the
--  rest of the file name. The input base dir is the path in the list of search
--  dirs that this file was found in. The output base dir is the build dir where
--  all the generated source files are put.
--
--  The reason for splitting it up this way is that some pre-processors don't
--  simply generate one output .hs file from one input file but have
--  dependencies on other generated files (notably c2hs, where building one
--  .hs file may require reading other .chi files, and then compiling the .hs
--  file may require reading a generated .h file). In these cases the generated
--  files need to embed relative path names to each other (eg the generated .hs
--  file mentions the .h file in the FFI imports). This path must be relative to
--  the base directory where the generated files are located, it cannot be
--  relative to the top level of the build tree because the compilers do not
--  look for .h files relative to there, ie we do not use \"-I .\", instead we
--  use \"-I dist\/build\" (or whatever dist dir has been set by the user)
--
--  Most pre-processors do not care of course, so mkSimplePreProcessor and
--  runSimplePreProcessor functions handle the simple case.
data PreProcessor = PreProcessor
  { -- Is the output of the pre-processor platform independent? eg happy output
    -- is portable haskell but c2hs's output is platform dependent.
    -- This matters since only platform independent generated code can be
    -- included into a source tarball.
    platformIndependent :: Bool
  , -- TODO: deal with pre-processors that have implementation dependent output
    --       eg alex and happy have --ghc flags. However we can't really include
    --       ghc-specific code into supposedly portable source tarballs.

    ppOrdering
      :: Verbosity
      -> [FilePath] -- Source directories
      -> [ModuleName] -- Module names
      -> IO [ModuleName] -- Sorted modules

  -- ^ This function can reorder /all/ modules, not just those that the
  -- require the preprocessor in question. As such, this function should be
  -- well-behaved and not reorder modules it doesn't have dominion over!
  --
  -- @since 3.8.1.0
  , runPreProcessor
      :: (FilePath, FilePath) -- Location of the source file relative to a base dir
      -> (FilePath, FilePath) -- Output file name, relative to an output base dir
      -> Verbosity -- verbosity
      -> IO () -- Should exit if the preprocessor fails
  }

-- | Just present the modules in the order given; this is the default and it is
-- appropriate for preprocessors which do not have any sort of dependencies
-- between modules.
unsorted
  :: Verbosity
  -> [FilePath]
  -> [ModuleName]
  -> IO [ModuleName]
unsorted _ _ ms = pure ms

-- | Function to determine paths to possible extra C sources for a
-- preprocessor: just takes the path to the build directory and uses
-- this to search for C sources with names that match the
-- preprocessor's output name format.
type PreProcessorExtras = FilePath -> IO [FilePath]

mkSimplePreProcessor
  :: (FilePath -> FilePath -> Verbosity -> IO ())
  -> (FilePath, FilePath)
  -> (FilePath, FilePath)
  -> Verbosity
  -> IO ()
mkSimplePreProcessor
  simplePP
  (inBaseDir, inRelativeFile)
  (outBaseDir, outRelativeFile)
  verbosity = simplePP inFile outFile verbosity
    where
      inFile = normalise (inBaseDir </> inRelativeFile)
      outFile = normalise (outBaseDir </> outRelativeFile)

runSimplePreProcessor
  :: PreProcessor
  -> FilePath
  -> FilePath
  -> Verbosity
  -> IO ()
runSimplePreProcessor pp inFile outFile verbosity =
  runPreProcessor pp (".", inFile) (".", outFile) verbosity

-- | A preprocessor for turning non-Haskell files with the given extension
--  into plain Haskell source files.
type PPSuffixHandler =
  (String, BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor)

-- | Apply preprocessors to the sources from 'hsSourceDirs' for a given
-- component (lib, exe, or test suite).
--
-- XXX: This is terrible
preprocessComponent
  :: PackageDescription
  -> Component
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Bool
  -> Verbosity
  -> [PPSuffixHandler]
  -> IO ()
preprocessComponent pd comp lbi clbi isSrcDist verbosity handlers =
  -- Skip preprocessing for scripts since they should be regular Haskell files,
  -- but may have no or unknown extensions.
  when (package pd /= fakePackageId) $ do
    -- NB: never report instantiation here; we'll report it properly when
    -- building.
    setupMessage'
      verbosity
      "Preprocessing"
      (packageId pd)
      (componentLocalName clbi)
      (Nothing :: Maybe [(ModuleName, Module)])
    case comp of
      (CLib lib@Library{libBuildInfo = bi}) -> do
        let dirs =
              map getSymbolicPath (hsSourceDirs bi)
                ++ [autogenComponentModulesDir lbi clbi, autogenPackageModulesDir lbi]
        let hndlrs = localHandlers bi
        mods <- orderingFromHandlers verbosity dirs hndlrs (allLibModules lib clbi)
        for_ (map ModuleName.toFilePath mods) $
          pre dirs (componentBuildDir lbi clbi) hndlrs
      (CFLib flib@ForeignLib{foreignLibBuildInfo = bi, foreignLibName = nm}) -> do
        let nm' = unUnqualComponentName nm
        let flibDir = buildDir lbi </> nm' </> nm' ++ "-tmp"
            dirs =
              map getSymbolicPath (hsSourceDirs bi)
                ++ [ autogenComponentModulesDir lbi clbi
                   , autogenPackageModulesDir lbi
                   ]
        let hndlrs = localHandlers bi
        mods <- orderingFromHandlers verbosity dirs hndlrs (foreignLibModules flib)
        for_ (map ModuleName.toFilePath mods) $
          pre dirs flibDir hndlrs
      (CExe exe@Executable{buildInfo = bi, exeName = nm}) -> do
        let nm' = unUnqualComponentName nm
        let exeDir = buildDir lbi </> nm' </> nm' ++ "-tmp"
            dirs =
              map getSymbolicPath (hsSourceDirs bi)
                ++ [ autogenComponentModulesDir lbi clbi
                   , autogenPackageModulesDir lbi
                   ]
        let hndlrs = localHandlers bi
        mods <- orderingFromHandlers verbosity dirs hndlrs (otherModules bi)
        for_ (map ModuleName.toFilePath mods) $
          pre dirs exeDir hndlrs
        pre (map getSymbolicPath (hsSourceDirs bi)) exeDir (localHandlers bi) $
          dropExtensions (modulePath exe)
      CTest test@TestSuite{testName = nm} -> do
        let nm' = unUnqualComponentName nm
        case testInterface test of
          TestSuiteExeV10 _ f ->
            preProcessTest test f $ buildDir lbi </> nm' </> nm' ++ "-tmp"
          TestSuiteLibV09 _ _ -> do
            let testDir =
                  buildDir lbi
                    </> stubName test
                    </> stubName test
                    ++ "-tmp"
            writeSimpleTestStub test testDir
            preProcessTest test (stubFilePath test) testDir
          TestSuiteUnsupported tt ->
            dieWithException verbosity $ NoSupportForPreProcessingTest tt
      CBench bm@Benchmark{benchmarkName = nm} -> do
        let nm' = unUnqualComponentName nm
        case benchmarkInterface bm of
          BenchmarkExeV10 _ f ->
            preProcessBench bm f $ buildDir lbi </> nm' </> nm' ++ "-tmp"
          BenchmarkUnsupported tt ->
            dieWithException verbosity $ NoSupportForPreProcessingBenchmark tt
  where
    orderingFromHandlers v d hndlrs mods =
      foldM (\acc (_, pp) -> ppOrdering pp v d acc) mods hndlrs
    builtinHaskellSuffixes = ["hs", "lhs", "hsig", "lhsig"]
    builtinCSuffixes = cSourceExtensions
    builtinSuffixes = builtinHaskellSuffixes ++ builtinCSuffixes
    localHandlers bi = [(ext, h bi lbi clbi) | (ext, h) <- handlers]
    pre dirs dir lhndlrs fp =
      preprocessFile (map unsafeMakeSymbolicPath dirs) dir isSrcDist fp verbosity builtinSuffixes lhndlrs True
    preProcessTest test =
      preProcessComponent
        (testBuildInfo test)
        (testModules test)
    preProcessBench bm =
      preProcessComponent
        (benchmarkBuildInfo bm)
        (benchmarkModules bm)

    preProcessComponent
      :: BuildInfo
      -> [ModuleName]
      -> FilePath
      -> FilePath
      -> IO ()
    preProcessComponent bi modules exePath dir = do
      let biHandlers = localHandlers bi
          sourceDirs =
            map getSymbolicPath (hsSourceDirs bi)
              ++ [ autogenComponentModulesDir lbi clbi
                 , autogenPackageModulesDir lbi
                 ]
      sequence_
        [ preprocessFile
          (map unsafeMakeSymbolicPath sourceDirs)
          dir
          isSrcDist
          (ModuleName.toFilePath modu)
          verbosity
          builtinSuffixes
          biHandlers
          False
        | modu <- modules
        ]
      -- XXX: what we do here (re SymbolicPath dir)
      -- XXX: 2020-10-15 do we rely here on CWD being the PackageDir?
      -- Note we don't fail on missing in this case, because the main file may be generated later (i.e. by a test code generator)
      preprocessFile
        (unsafeMakeSymbolicPath dir : hsSourceDirs bi)
        dir
        isSrcDist
        (dropExtensions $ exePath)
        verbosity
        builtinSuffixes
        biHandlers
        False

-- TODO: try to list all the modules that could not be found
--      not just the first one. It's annoying and slow due to the need
--      to reconfigure after editing the .cabal file each time.

-- | Find the first extension of the file that exists, and preprocess it
-- if required.
preprocessFile
  :: [SymbolicPath PackageDir SourceDir]
  -- ^ source directories
  -> FilePath
  -- ^ build directory
  -> Bool
  -- ^ preprocess for sdist
  -> FilePath
  -- ^ module file name
  -> Verbosity
  -- ^ verbosity
  -> [String]
  -- ^ builtin suffixes
  -> [(String, PreProcessor)]
  -- ^ possible preprocessors
  -> Bool
  -- ^ fail on missing file
  -> IO ()
preprocessFile searchLoc buildLoc forSDist baseFile verbosity builtinSuffixes handlers failOnMissing = do
  -- look for files in the various source dirs with this module name
  -- and a file extension of a known preprocessor
  psrcFiles <- findFileWithExtension' (map fst handlers) (map getSymbolicPath searchLoc) baseFile
  case psrcFiles of
    -- no preprocessor file exists, look for an ordinary source file
    -- just to make sure one actually exists at all for this module.
    -- Note: by looking in the target/output build dir too, we allow
    -- source files to appear magically in the target build dir without
    -- any corresponding "real" source file. This lets custom Setup.hs
    -- files generate source modules directly into the build dir without
    -- the rest of the build system being aware of it (somewhat dodgy)
    Nothing -> do
      bsrcFiles <- findFileWithExtension builtinSuffixes (buildLoc : map getSymbolicPath searchLoc) baseFile
      case (bsrcFiles, failOnMissing) of
        (Nothing, True) ->
          dieWithException verbosity $
            CantFindSourceForPreProcessFile $
              "can't find source for "
                ++ baseFile
                ++ " in "
                ++ intercalate ", " (map getSymbolicPath searchLoc)
        _ -> return ()
    -- found a pre-processable file in one of the source dirs
    Just (psrcLoc, psrcRelFile) -> do
      let (srcStem, ext) = splitExtension psrcRelFile
          psrcFile = psrcLoc </> psrcRelFile
          pp =
            fromMaybe
              (error "Distribution.Simple.PreProcess: Just expected")
              (lookup (safeTail ext) handlers)
      -- Preprocessing files for 'sdist' is different from preprocessing
      -- for 'build'.  When preprocessing for sdist we preprocess to
      -- avoid that the user has to have the preprocessors available.
      -- ATM, we don't have a way to specify which files are to be
      -- preprocessed and which not, so for sdist we only process
      -- platform independent files and put them into the 'buildLoc'
      -- (which we assume is set to the temp. directory that will become
      -- the tarball).
      -- TODO: eliminate sdist variant, just supply different handlers
      when (not forSDist || forSDist && platformIndependent pp) $ do
        -- look for existing pre-processed source file in the dest dir to
        -- see if we really have to re-run the preprocessor.
        ppsrcFiles <- findFileWithExtension builtinSuffixes [buildLoc] baseFile
        recomp <- case ppsrcFiles of
          Nothing -> return True
          Just ppsrcFile ->
            psrcFile `moreRecentFile` ppsrcFile
        when recomp $ do
          let destDir = buildLoc </> dirName srcStem
          createDirectoryIfMissingVerbose verbosity True destDir
          runPreProcessorWithHsBootHack
            pp
            (psrcLoc, psrcRelFile)
            (buildLoc, srcStem <.> "hs")
  where
    dirName = takeDirectory

    -- FIXME: This is a somewhat nasty hack. GHC requires that hs-boot files
    -- be in the same place as the hs files, so if we put the hs file in dist/
    -- then we need to copy the hs-boot file there too. This should probably be
    -- done another way. Possibly we should also be looking for .lhs-boot
    -- files, but I think that preprocessors only produce .hs files.
    runPreProcessorWithHsBootHack
      pp
      (inBaseDir, inRelativeFile)
      (outBaseDir, outRelativeFile) = do
        runPreProcessor
          pp
          (inBaseDir, inRelativeFile)
          (outBaseDir, outRelativeFile)
          verbosity

        exists <- doesFileExist inBoot
        when exists $ copyFileVerbose verbosity inBoot outBoot
        where
          inBoot = replaceExtension inFile "hs-boot"
          outBoot = replaceExtension outFile "hs-boot"

          inFile = normalise (inBaseDir </> inRelativeFile)
          outFile = normalise (outBaseDir </> outRelativeFile)

-- ------------------------------------------------------------

-- * known preprocessors

-- ------------------------------------------------------------

ppGreenCard :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppGreenCard _ lbi _ =
  PreProcessor
    { platformIndependent = False
    , ppOrdering = unsorted
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
        runDbProgram
          verbosity
          greencardProgram
          (withPrograms lbi)
          (["-tffi", "-o" ++ outFile, inFile])
    }

-- This one is useful for preprocessors that can't handle literate source.
-- We also need a way to chain preprocessors.
ppUnlit :: PreProcessor
ppUnlit =
  PreProcessor
    { platformIndependent = True
    , ppOrdering = unsorted
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
        withUTF8FileContents inFile $ \contents ->
          either (writeUTF8File outFile) (dieWithException verbosity) (unlit inFile contents)
    }

ppCpp :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppCpp = ppCpp' []

ppCpp' :: [String] -> BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppCpp' extraArgs bi lbi clbi =
  case compilerFlavor (compiler lbi) of
    GHC -> ppGhcCpp ghcProgram (const True) args bi lbi clbi
    GHCJS -> ppGhcCpp ghcjsProgram (const True) args bi lbi clbi
    _ -> ppCpphs args bi lbi clbi
  where
    cppArgs = getCppOptions bi lbi
    args = cppArgs ++ extraArgs

ppGhcCpp
  :: Program
  -> (Version -> Bool)
  -> [String]
  -> BuildInfo
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> PreProcessor
ppGhcCpp program xHs extraArgs _bi lbi clbi =
  PreProcessor
    { platformIndependent = False
    , ppOrdering = unsorted
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
        (prog, version, _) <-
          requireProgramVersion
            verbosity
            program
            anyVersion
            (withPrograms lbi)
        runProgram verbosity prog $
          ["-E", "-cpp"]
            -- This is a bit of an ugly hack. We're going to
            -- unlit the file ourselves later on if appropriate,
            -- so we need GHC not to unlit it now or it'll get
            -- double-unlitted. In the future we might switch to
            -- using cpphs --unlit instead.
            ++ (if xHs version then ["-x", "hs"] else [])
            ++ ["-optP-include", "-optP" ++ (autogenComponentModulesDir lbi clbi </> cppHeaderName)]
            ++ ["-o", outFile, inFile]
            ++ extraArgs
    }

ppCpphs :: [String] -> BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppCpphs extraArgs _bi lbi clbi =
  PreProcessor
    { platformIndependent = False
    , ppOrdering = unsorted
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
        (cpphsProg, cpphsVersion, _) <-
          requireProgramVersion
            verbosity
            cpphsProgram
            anyVersion
            (withPrograms lbi)
        runProgram verbosity cpphsProg $
          ("-O" ++ outFile)
            : inFile
            : "--noline"
            : "--strip"
            : ( if cpphsVersion >= mkVersion [1, 6]
                  then ["--include=" ++ (autogenComponentModulesDir lbi clbi </> cppHeaderName)]
                  else []
              )
            ++ extraArgs
    }

ppHsc2hs :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppHsc2hs bi lbi clbi =
  PreProcessor
    { platformIndependent = False
    , ppOrdering = unsorted
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
        (gccProg, _) <- requireProgram verbosity gccProgram (withPrograms lbi)
        (hsc2hsProg, hsc2hsVersion, _) <-
          requireProgramVersion
            verbosity
            hsc2hsProgram
            anyVersion
            (withPrograms lbi)
        -- See Trac #13896 and https://github.com/haskell/cabal/issues/3122.
        let isCross = hostPlatform lbi /= buildPlatform
            prependCrossFlags = if isCross then ("-x" :) else id
        let hsc2hsSupportsResponseFiles = hsc2hsVersion >= mkVersion [0, 68, 4]
            pureArgs = genPureArgs hsc2hsVersion gccProg inFile outFile
        if hsc2hsSupportsResponseFiles
          then
            withResponseFile
              verbosity
              defaultTempFileOptions
              (takeDirectory outFile)
              "hsc2hs-response.txt"
              Nothing
              pureArgs
              ( \responseFileName ->
                  runProgram verbosity hsc2hsProg (prependCrossFlags ["@" ++ responseFileName])
              )
          else runProgram verbosity hsc2hsProg (prependCrossFlags pureArgs)
    }
  where
    -- Returns a list of command line arguments that can either be passed
    -- directly, or via a response file.
    genPureArgs :: Version -> ConfiguredProgram -> String -> String -> [String]
    genPureArgs hsc2hsVersion gccProg inFile outFile =
      -- Additional gcc options
      [ "--cflag=" ++ opt
      | opt <-
          programDefaultArgs gccProg
            ++ programOverrideArgs gccProg
      ]
        ++ [ "--lflag=" ++ opt
           | opt <-
              programDefaultArgs gccProg
                ++ programOverrideArgs gccProg
           ]
        -- OSX frameworks:
        ++ [ what ++ "=-F" ++ opt
           | isOSX
           , opt <- nub (concatMap Installed.frameworkDirs pkgs)
           , what <- ["--cflag", "--lflag"]
           ]
        ++ [ "--lflag=" ++ arg
           | isOSX
           , opt <- PD.frameworks bi ++ concatMap Installed.frameworks pkgs
           , arg <- ["-framework", opt]
           ]
        -- Note that on ELF systems, wherever we use -L, we must also use -R
        -- because presumably that -L dir is not on the normal path for the
        -- system's dynamic linker. This is needed because hsc2hs works by
        -- compiling a C program and then running it.

        ++ ["--cflag=" ++ opt | opt <- platformDefines lbi]
        -- Options from the current package:
        ++ ["--cflag=-I" ++ dir | dir <- PD.includeDirs bi]
        ++ ["--cflag=-I" ++ buildDir lbi </> dir | dir <- PD.includeDirs bi]
        ++ [ "--cflag=" ++ opt
           | opt <-
              PD.ccOptions bi
                ++ PD.cppOptions bi
                -- hsc2hs uses the C ABI
                -- We assume that there are only C sources
                -- and C++ functions are exported via a C
                -- interface and wrapped in a C source file.
                -- Therefore we do not supply C++ flags
                -- because there will not be C++ sources.
                --
                -- DO NOT add PD.cxxOptions unless this changes!
           ]
        ++ [ "--cflag=" ++ opt
           | opt <-
              [ "-I" ++ autogenComponentModulesDir lbi clbi
              , "-I" ++ autogenPackageModulesDir lbi
              , "-include"
              , autogenComponentModulesDir lbi clbi </> cppHeaderName
              ]
           ]
        ++ [ "--lflag=-L" ++ opt
           | opt <-
              if withFullyStaticExe lbi
                then PD.extraLibDirsStatic bi
                else PD.extraLibDirs bi
           ]
        ++ [ "--lflag=-Wl,-R," ++ opt
           | isELF
           , opt <-
              if withFullyStaticExe lbi
                then PD.extraLibDirsStatic bi
                else PD.extraLibDirs bi
           ]
        ++ ["--lflag=-l" ++ opt | opt <- PD.extraLibs bi]
        ++ ["--lflag=" ++ opt | opt <- PD.ldOptions bi]
        -- Options from dependent packages
        ++ [ "--cflag=" ++ opt
           | pkg <- pkgs
           , opt <-
              ["-I" ++ opt | opt <- Installed.includeDirs pkg]
                ++ Installed.ccOptions pkg
           ]
        ++ [ "--lflag=" ++ opt
           | pkg <- pkgs
           , opt <-
              ["-L" ++ opt | opt <- Installed.libraryDirs pkg]
                ++ [ "-Wl,-R," ++ opt | isELF, opt <- Installed.libraryDirs pkg
                   ]
                ++ [ "-l" ++ opt
                   | opt <-
                      if withFullyStaticExe lbi
                        then Installed.extraLibrariesStatic pkg
                        else Installed.extraLibraries pkg
                   ]
                ++ Installed.ldOptions pkg
           ]
        ++ preccldFlags
        ++ hsc2hsOptions bi
        ++ postccldFlags
        ++ ["-o", outFile, inFile]
      where
        -- hsc2hs flag parsing was wrong
        -- (see -- https://github.com/haskell/hsc2hs/issues/35)
        -- so we need to put -- --cc/--ld *after* hsc2hsOptions,
        -- for older hsc2hs (pre 0.68.8) so that they can be overridden.
        ccldFlags =
          [ "--cc=" ++ programPath gccProg
          , "--ld=" ++ programPath gccProg
          ]

        (preccldFlags, postccldFlags)
          | hsc2hsVersion >= mkVersion [0, 68, 8] = (ccldFlags, [])
          | otherwise = ([], ccldFlags)

    hacked_index = packageHacks (installedPkgs lbi)
    -- Look only at the dependencies of the current component
    -- being built!  This relies on 'installedPkgs' maintaining
    -- 'InstalledPackageInfo' for internal deps too; see #2971.
    pkgs = PackageIndex.topologicalOrder $
      case PackageIndex.dependencyClosure
        hacked_index
        (map fst (componentPackageDeps clbi)) of
        Left index' -> index'
        Right inf ->
          error ("ppHsc2hs: broken closure: " ++ show inf)
    isOSX = case buildOS of OSX -> True; _ -> False
    isELF = case buildOS of OSX -> False; Windows -> False; AIX -> False; _ -> True
    packageHacks = case compilerFlavor (compiler lbi) of
      GHC -> hackRtsPackage
      GHCJS -> hackRtsPackage
      _ -> id
    -- We don't link in the actual Haskell libraries of our dependencies, so
    -- the -u flags in the ldOptions of the rts package mean linking fails on
    -- OS X (its ld is a tad stricter than gnu ld). Thus we remove the
    -- ldOptions for GHC's rts package:
    hackRtsPackage index =
      case PackageIndex.lookupPackageName index (mkPackageName "rts") of
        [(_, [rts])] ->
          PackageIndex.insert rts{Installed.ldOptions = []} index
        _ -> error "No (or multiple) ghc rts package is registered!!"

ppHsc2hsExtras :: PreProcessorExtras
ppHsc2hsExtras buildBaseDir =
  filter ("_hsc.c" `isSuffixOf`)
    `fmap` getDirectoryContentsRecursive buildBaseDir

ppC2hs :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppC2hs bi lbi clbi =
  PreProcessor
    { platformIndependent = False
    , ppOrdering = unsorted
    , runPreProcessor =
        \(inBaseDir, inRelativeFile)
         (outBaseDir, outRelativeFile)
         verbosity -> do
            (c2hsProg, _, _) <-
              requireProgramVersion
                verbosity
                c2hsProgram
                (orLaterVersion (mkVersion [0, 15]))
                (withPrograms lbi)
            (gccProg, _) <- requireProgram verbosity gccProgram (withPrograms lbi)
            runProgram verbosity c2hsProg $
              -- Options from the current package:
              ["--cpp=" ++ programPath gccProg, "--cppopts=-E"]
                ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
                ++ ["--cppopts=-include" ++ (autogenComponentModulesDir lbi clbi </> cppHeaderName)]
                ++ ["--include=" ++ outBaseDir]
                -- Options from dependent packages
                ++ [ "--cppopts=" ++ opt
                   | pkg <- pkgs
                   , opt <-
                      ["-I" ++ opt | opt <- Installed.includeDirs pkg]
                        ++ [ opt | opt@('-' : c : _) <- Installed.ccOptions pkg,
                           -- c2hs uses the C ABI
                           -- We assume that there are only C sources
                           -- and C++ functions are exported via a C
                           -- interface and wrapped in a C source file.
                           -- Therefore we do not supply C++ flags
                           -- because there will not be C++ sources.
                           --
                           --
                           -- DO NOT add Installed.cxxOptions unless this changes!
                           c `elem` "DIU"
                           ]
                   ]
                -- TODO: install .chi files for packages, so we can --include
                -- those dirs here, for the dependencies

                -- input and output files
                ++ [ "--output-dir=" ++ outBaseDir
                   , "--output=" ++ outRelativeFile
                   , inBaseDir </> inRelativeFile
                   ]
    }
  where
    pkgs = PackageIndex.topologicalOrder (installedPkgs lbi)

ppC2hsExtras :: PreProcessorExtras
ppC2hsExtras d =
  filter (\p -> takeExtensions p == ".chs.c")
    `fmap` getDirectoryContentsRecursive d

-- TODO: perhaps use this with hsc2hs too
-- TODO: remove cc-options from cpphs for cabal-version: >= 1.10
-- TODO: Refactor and add separate getCppOptionsForHs, getCppOptionsForCxx, & getCppOptionsForC
--      instead of combining all these cases in a single function. This blind combination can
--      potentially lead to compilation inconsistencies.
getCppOptions :: BuildInfo -> LocalBuildInfo -> [String]
getCppOptions bi lbi =
  platformDefines lbi
    ++ cppOptions bi
    ++ ["-I" ++ dir | dir <- PD.includeDirs bi]
    ++ [opt | opt@('-' : c : _) <- PD.ccOptions bi ++ PD.cxxOptions bi, c `elem` "DIU"]

platformDefines :: LocalBuildInfo -> [String]
platformDefines lbi =
  case compilerFlavor comp of
    GHC ->
      ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
        ++ ["-D" ++ os ++ "_BUILD_OS=1"]
        ++ ["-D" ++ arch ++ "_BUILD_ARCH=1"]
        ++ map (\os' -> "-D" ++ os' ++ "_HOST_OS=1") osStr
        ++ map (\arch' -> "-D" ++ arch' ++ "_HOST_ARCH=1") archStr
    GHCJS ->
      compatGlasgowHaskell
        ++ ["-D__GHCJS__=" ++ versionInt version]
        ++ ["-D" ++ os ++ "_BUILD_OS=1"]
        ++ ["-D" ++ arch ++ "_BUILD_ARCH=1"]
        ++ map (\os' -> "-D" ++ os' ++ "_HOST_OS=1") osStr
        ++ map (\arch' -> "-D" ++ arch' ++ "_HOST_ARCH=1") archStr
    HaskellSuite{} ->
      ["-D__HASKELL_SUITE__"]
        ++ map (\os' -> "-D" ++ os' ++ "_HOST_OS=1") osStr
        ++ map (\arch' -> "-D" ++ arch' ++ "_HOST_ARCH=1") archStr
    _ -> []
  where
    comp = compiler lbi
    Platform hostArch hostOS = hostPlatform lbi
    version = compilerVersion comp
    compatGlasgowHaskell =
      maybe
        []
        (\v -> ["-D__GLASGOW_HASKELL__=" ++ versionInt v])
        (compilerCompatVersion GHC comp)
    -- TODO: move this into the compiler abstraction
    -- FIXME: this forces GHC's crazy 4.8.2 -> 408 convention on all
    -- the other compilers. Check if that's really what they want.
    versionInt :: Version -> String
    versionInt v = case versionNumbers v of
      [] -> "1"
      [n] -> show n
      n1 : n2 : _ ->
        -- 6.8.x -> 608
        -- 6.10.x -> 610
        let s1 = show n1
            s2 = show n2
            middle = case s2 of
              _ : _ : _ -> ""
              _ -> "0"
         in s1 ++ middle ++ s2

    osStr = case hostOS of
      Linux -> ["linux"]
      Windows -> ["mingw32"]
      OSX -> ["darwin"]
      FreeBSD -> ["freebsd"]
      OpenBSD -> ["openbsd"]
      NetBSD -> ["netbsd"]
      DragonFly -> ["dragonfly"]
      Solaris -> ["solaris2"]
      AIX -> ["aix"]
      HPUX -> ["hpux"]
      IRIX -> ["irix"]
      HaLVM -> []
      IOS -> ["ios"]
      Android -> ["android"]
      Ghcjs -> ["ghcjs"]
      Wasi -> ["wasi"]
      Hurd -> ["hurd"]
      Haiku -> ["haiku"]
      OtherOS _ -> []
    archStr = case hostArch of
      I386 -> ["i386"]
      X86_64 -> ["x86_64"]
      PPC -> ["powerpc"]
      PPC64 -> ["powerpc64"]
      Sparc -> ["sparc"]
      Arm -> ["arm"]
      AArch64 -> ["aarch64"]
      Mips -> ["mips"]
      SH -> []
      IA64 -> ["ia64"]
      S390 -> ["s390"]
      S390X -> ["s390x"]
      Alpha -> ["alpha"]
      Hppa -> ["hppa"]
      Rs6000 -> ["rs6000"]
      M68k -> ["m68k"]
      Vax -> ["vax"]
      RISCV64 -> ["riscv64"]
      LoongArch64 -> ["loongarch64"]
      JavaScript -> ["javascript"]
      Wasm32 -> ["wasm32"]
      OtherArch _ -> []

ppHappy :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppHappy _ lbi _ = pp{platformIndependent = True}
  where
    pp = standardPP lbi happyProgram (hcFlags hc)
    hc = compilerFlavor (compiler lbi)
    hcFlags GHC = ["-agc"]
    hcFlags GHCJS = ["-agc"]
    hcFlags _ = []

ppAlex :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppAlex _ lbi _ = pp{platformIndependent = True}
  where
    pp = standardPP lbi alexProgram (hcFlags hc)
    hc = compilerFlavor (compiler lbi)
    hcFlags GHC = ["-g"]
    hcFlags GHCJS = ["-g"]
    hcFlags _ = []

standardPP :: LocalBuildInfo -> Program -> [String] -> PreProcessor
standardPP lbi prog args =
  PreProcessor
    { platformIndependent = False
    , ppOrdering = unsorted
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
        runDbProgram
          verbosity
          prog
          (withPrograms lbi)
          (args ++ ["-o", outFile, inFile])
    }

-- | Convenience function; get the suffixes of these preprocessors.
ppSuffixes :: [PPSuffixHandler] -> [String]
ppSuffixes = map fst

-- | Standard preprocessors: GreenCard, c2hs, hsc2hs, happy, alex and cpphs.
knownSuffixHandlers :: [PPSuffixHandler]
knownSuffixHandlers =
  [ ("gc", ppGreenCard)
  , ("chs", ppC2hs)
  , ("hsc", ppHsc2hs)
  , ("x", ppAlex)
  , ("y", ppHappy)
  , ("ly", ppHappy)
  , ("cpphs", ppCpp)
  ]

-- | Standard preprocessors with possible extra C sources: c2hs, hsc2hs.
knownExtrasHandlers :: [PreProcessorExtras]
knownExtrasHandlers = [ppC2hsExtras, ppHsc2hsExtras]

-- | Find any extra C sources generated by preprocessing that need to
-- be added to the component (addresses issue #238).
preprocessExtras
  :: Verbosity
  -> Component
  -> LocalBuildInfo
  -> IO [FilePath]
preprocessExtras verbosity comp lbi = case comp of
  CLib _ -> pp $ buildDir lbi
  (CExe Executable{exeName = nm}) -> do
    let nm' = unUnqualComponentName nm
    pp $ buildDir lbi </> nm' </> nm' ++ "-tmp"
  (CFLib ForeignLib{foreignLibName = nm}) -> do
    let nm' = unUnqualComponentName nm
    pp $ buildDir lbi </> nm' </> nm' ++ "-tmp"
  CTest test -> do
    let nm' = unUnqualComponentName $ testName test
    case testInterface test of
      TestSuiteExeV10 _ _ ->
        pp $ buildDir lbi </> nm' </> nm' ++ "-tmp"
      TestSuiteLibV09 _ _ ->
        pp $ buildDir lbi </> stubName test </> stubName test ++ "-tmp"
      TestSuiteUnsupported tt ->
        dieWithException verbosity $ NoSupportPreProcessingTestExtras tt
  CBench bm -> do
    let nm' = unUnqualComponentName $ benchmarkName bm
    case benchmarkInterface bm of
      BenchmarkExeV10 _ _ ->
        pp $ buildDir lbi </> nm' </> nm' ++ "-tmp"
      BenchmarkUnsupported tt ->
        dieWithException verbosity $ NoSupportPreProcessingBenchmarkExtras tt
  where
    pp :: FilePath -> IO [FilePath]
    pp dir = do
      b <- doesDirectoryExist dir
      if b
        then
          (map (dir </>) . filter not_sub . concat)
            <$> for
              knownExtrasHandlers
              (withLexicalCallStack (\f -> f dir))
        else pure []
    -- TODO: This is a terrible hack to work around #3545 while we don't
    -- reorganize the directory layout.  Basically, for the main
    -- library, we might accidentally pick up autogenerated sources for
    -- our subcomponents, because they are all stored as subdirectories
    -- in dist/build.  This is a cheap and cheerful check to prevent
    -- this from happening.  It is not particularly correct; for example
    -- if a user has a test suite named foobar and puts their C file in
    -- foobar/foo.c, this test will incorrectly exclude it.  But I
    -- didn't want to break BC...
    not_sub p = and [not (pre `isPrefixOf` p) | pre <- component_dirs]
    component_dirs = component_names (localPkgDescr lbi)
    -- TODO: libify me
    component_names pkg_descr =
      fmap unUnqualComponentName $
        mapMaybe (libraryNameString . libName) (subLibraries pkg_descr)
          ++ map exeName (executables pkg_descr)
          ++ map testName (testSuites pkg_descr)
          ++ map benchmarkName (benchmarks pkg_descr)
