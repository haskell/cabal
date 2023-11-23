module Distribution.Simple.GHC.BuildGeneric
  ( GBuildMode (..)
  , gbuild
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad (msum)
import Data.Char (isLower)
import Distribution.CabalSpecVersion
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Utils (cabalBug)
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (Flag (..), fromFlag, toFlag)
import Distribution.Simple.GHC.Build
  ( checkNeedsRecompilation
  , componentGhcOptions
  , exeTargetName
  , flibBuildName
  , flibTargetName
  , getRPaths
  , isDynamic
  , replNoLoad
  , runReplOrWriteFlags
  , supportsDynamicToo
  )
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup.Config
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.PackageName.Magic
import Distribution.Types.ParStrat
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeFile
  , renameFile
  )
import System.FilePath
  ( replaceExtension
  , takeExtension
  , (</>)
  )

-- | A collection of:
--    * C input files
--    * C++ input files
--    * GHC input files
--    * GHC input modules
--
-- Used to correctly build and link sources.
data BuildSources = BuildSources
  { cSourcesFiles :: [FilePath]
  , cxxSourceFiles :: [FilePath]
  , jsSourceFiles :: [FilePath]
  , asmSourceFiles :: [FilePath]
  , cmmSourceFiles :: [FilePath]
  , inputSourceFiles :: [FilePath]
  , inputSourceModules :: [ModuleName]
  }

data DynamicRtsInfo = DynamicRtsInfo
  { dynRtsVanillaLib :: FilePath
  , dynRtsThreadedLib :: FilePath
  , dynRtsDebugLib :: FilePath
  , dynRtsEventlogLib :: FilePath
  , dynRtsThreadedDebugLib :: FilePath
  , dynRtsThreadedEventlogLib :: FilePath
  }

data StaticRtsInfo = StaticRtsInfo
  { statRtsVanillaLib :: FilePath
  , statRtsThreadedLib :: FilePath
  , statRtsDebugLib :: FilePath
  , statRtsEventlogLib :: FilePath
  , statRtsThreadedDebugLib :: FilePath
  , statRtsThreadedEventlogLib :: FilePath
  , statRtsProfilingLib :: FilePath
  , statRtsThreadedProfilingLib :: FilePath
  }

data RtsInfo = RtsInfo
  { rtsDynamicInfo :: DynamicRtsInfo
  , rtsStaticInfo :: StaticRtsInfo
  , rtsLibPaths :: [FilePath]
  }

-- | Building an executable, starting the REPL, and building foreign
-- libraries are all very similar and implemented in 'gbuild'. The
-- 'GBuildMode' distinguishes between the various kinds of operation.
data GBuildMode
  = GBuildExe Executable
  | GReplExe ReplOptions Executable
  | GBuildFLib ForeignLib
  | GReplFLib ReplOptions ForeignLib

gbuildInfo :: GBuildMode -> BuildInfo
gbuildInfo (GBuildExe exe) = buildInfo exe
gbuildInfo (GReplExe _ exe) = buildInfo exe
gbuildInfo (GBuildFLib flib) = foreignLibBuildInfo flib
gbuildInfo (GReplFLib _ flib) = foreignLibBuildInfo flib

gbuildIsRepl :: GBuildMode -> Bool
gbuildIsRepl (GBuildExe _) = False
gbuildIsRepl (GReplExe _ _) = True
gbuildIsRepl (GBuildFLib _) = False
gbuildIsRepl (GReplFLib _ _) = True

gbuildModDefFiles :: GBuildMode -> [FilePath]
gbuildModDefFiles (GBuildExe _) = []
gbuildModDefFiles (GReplExe _ _) = []
gbuildModDefFiles (GBuildFLib flib) = foreignLibModDefFile flib
gbuildModDefFiles (GReplFLib _ flib) = foreignLibModDefFile flib

gbuildName :: GBuildMode -> String
gbuildName (GBuildExe exe) = unUnqualComponentName $ exeName exe
gbuildName (GReplExe _ exe) = unUnqualComponentName $ exeName exe
gbuildName (GBuildFLib flib) = unUnqualComponentName $ foreignLibName flib
gbuildName (GReplFLib _ flib) = unUnqualComponentName $ foreignLibName flib

gbuildTargetName :: LocalBuildInfo -> GBuildMode -> String
gbuildTargetName lbi (GBuildExe exe) = exeTargetName (hostPlatform lbi) exe
gbuildTargetName lbi (GReplExe _ exe) = exeTargetName (hostPlatform lbi) exe
gbuildTargetName lbi (GBuildFLib flib) = flibTargetName lbi flib
gbuildTargetName lbi (GReplFLib _ flib) = flibTargetName lbi flib

gbuildNeedDynamic :: LocalBuildInfo -> GBuildMode -> Bool
gbuildNeedDynamic lbi bm =
  case bm of
    GBuildExe _ -> withDynExe lbi
    GReplExe _ _ -> withDynExe lbi
    GBuildFLib flib -> withDynFLib flib
    GReplFLib _ flib -> withDynFLib flib
  where
    withDynFLib flib =
      case foreignLibType flib of
        ForeignLibNativeShared ->
          ForeignLibStandalone `notElem` foreignLibOptions flib
        ForeignLibNativeStatic ->
          False
        ForeignLibTypeUnknown ->
          cabalBug "unknown foreign lib type"

-- | Locate and return the 'BuildSources' required to build and link.
gbuildSources
  :: Verbosity
  -> PackageId
  -> CabalSpecVersion
  -> FilePath
  -> GBuildMode
  -> IO BuildSources
gbuildSources verbosity pkgId specVer tmpDir bm =
  case bm of
    GBuildExe exe -> exeSources exe
    GReplExe _ exe -> exeSources exe
    GBuildFLib flib -> return $ flibSources flib
    GReplFLib _ flib -> return $ flibSources flib
  where
    exeSources :: Executable -> IO BuildSources
    exeSources exe@Executable{buildInfo = bnfo, modulePath = modPath} = do
      main <- findFileEx verbosity (tmpDir : map getSymbolicPath (hsSourceDirs bnfo)) modPath
      let mainModName = fromMaybe ModuleName.main $ exeMainModuleName exe
          otherModNames = exeModules exe

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if isHaskell main || pkgId == fakePackageId
        then
          if specVer < CabalSpecV2_0 && (mainModName `elem` otherModNames)
            then do
              -- The cabal manual clearly states that `other-modules` is
              -- intended for non-main modules.  However, there's at least one
              -- important package on Hackage (happy-1.19.5) which
              -- violates this. We workaround this here so that we don't
              -- invoke GHC with e.g.  'ghc --make Main src/Main.hs' which
              -- would result in GHC complaining about duplicate Main
              -- modules.
              --
              -- Finally, we only enable this workaround for
              -- specVersion < 2, as 'cabal-version:>=2.0' cabal files
              -- have no excuse anymore to keep doing it wrong... ;-)
              warn verbosity $
                "Enabling workaround for Main module '"
                  ++ prettyShow mainModName
                  ++ "' listed in 'other-modules' illegally!"

              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , jsSourceFiles = jsSources bnfo
                  , asmSourceFiles = asmSources bnfo
                  , cmmSourceFiles = cmmSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules =
                      filter (/= mainModName) $
                        exeModules exe
                  }
            else
              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , jsSourceFiles = jsSources bnfo
                  , asmSourceFiles = asmSources bnfo
                  , cmmSourceFiles = cmmSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules = exeModules exe
                  }
        else
          let (csf, cxxsf)
                | isCxx main = (cSources bnfo, main : cxxSources bnfo)
                -- if main is not a Haskell source
                -- and main is not a C++ source
                -- then we assume that it is a C source
                | otherwise = (main : cSources bnfo, cxxSources bnfo)
           in return
                BuildSources
                  { cSourcesFiles = csf
                  , cxxSourceFiles = cxxsf
                  , jsSourceFiles = jsSources bnfo
                  , asmSourceFiles = asmSources bnfo
                  , cmmSourceFiles = cmmSources bnfo
                  , inputSourceFiles = []
                  , inputSourceModules = exeModules exe
                  }

    flibSources :: ForeignLib -> BuildSources
    flibSources flib@ForeignLib{foreignLibBuildInfo = bnfo} =
      BuildSources
        { cSourcesFiles = cSources bnfo
        , cxxSourceFiles = cxxSources bnfo
        , jsSourceFiles = jsSources bnfo
        , asmSourceFiles = asmSources bnfo
        , cmmSourceFiles = cmmSources bnfo
        , inputSourceFiles = []
        , inputSourceModules = foreignLibModules flib
        }

    isCxx :: FilePath -> Bool
    isCxx fp = elem (takeExtension fp) [".cpp", ".cxx", ".c++"]

-- | Extract (and compute) information about the RTS library
--
-- TODO: This hardcodes the name as @HSrts-ghc<version>@. I don't know if we can
-- find this information somewhere. We can lookup the 'hsLibraries' field of
-- 'InstalledPackageInfo' but it will tell us @["HSrts", "Cffi"]@, which
-- doesn't really help.
extractRtsInfo :: LocalBuildInfo -> RtsInfo
extractRtsInfo lbi =
  case PackageIndex.lookupPackageName
    (installedPkgs lbi)
    (mkPackageName "rts") of
    [(_, [rts])] -> aux rts
    _otherwise -> error "No (or multiple) ghc rts package is registered"
  where
    aux :: InstalledPackageInfo -> RtsInfo
    aux rts =
      RtsInfo
        { rtsDynamicInfo =
            DynamicRtsInfo
              { dynRtsVanillaLib = withGhcVersion "HSrts"
              , dynRtsThreadedLib = withGhcVersion "HSrts_thr"
              , dynRtsDebugLib = withGhcVersion "HSrts_debug"
              , dynRtsEventlogLib = withGhcVersion "HSrts_l"
              , dynRtsThreadedDebugLib = withGhcVersion "HSrts_thr_debug"
              , dynRtsThreadedEventlogLib = withGhcVersion "HSrts_thr_l"
              }
        , rtsStaticInfo =
            StaticRtsInfo
              { statRtsVanillaLib = "HSrts"
              , statRtsThreadedLib = "HSrts_thr"
              , statRtsDebugLib = "HSrts_debug"
              , statRtsEventlogLib = "HSrts_l"
              , statRtsThreadedDebugLib = "HSrts_thr_debug"
              , statRtsThreadedEventlogLib = "HSrts_thr_l"
              , statRtsProfilingLib = "HSrts_p"
              , statRtsThreadedProfilingLib = "HSrts_thr_p"
              }
        , rtsLibPaths = InstalledPackageInfo.libraryDirs rts
        }
    withGhcVersion = (++ ("-ghc" ++ prettyShow (compilerVersion (compiler lbi))))

-- | Determine whether the given 'BuildInfo' is intended to link against the
-- threaded RTS. This is used to determine which RTS to link against when
-- building a foreign library with a GHC without support for @-flink-rts@.
hasThreaded :: BuildInfo -> Bool
hasThreaded bi = elem "-threaded" ghc
  where
    PerCompilerFlavor ghc _ = options bi

-- | FilePath has a Haskell extension: .hs or .lhs
isHaskell :: FilePath -> Bool
isHaskell fp = elem (takeExtension fp) [".hs", ".lhs"]

-- | "Main" module name when overridden by @ghc-options: -main-is ...@
-- or 'Nothing' if no @-main-is@ flag could be found.
--
-- In case of 'Nothing', 'Distribution.ModuleName.main' can be assumed.
exeMainModuleName :: Executable -> Maybe ModuleName
exeMainModuleName Executable{buildInfo = bnfo} =
  -- GHC honors the last occurrence of a module name updated via -main-is
  --
  -- Moreover, -main-is when parsed left-to-right can update either
  -- the "Main" module name, or the "main" function name, or both,
  -- see also 'decodeMainIsArg'.
  msum $ reverse $ map decodeMainIsArg $ findIsMainArgs ghcopts
  where
    ghcopts = hcOptions GHC bnfo

    findIsMainArgs [] = []
    findIsMainArgs ("-main-is" : arg : rest) = arg : findIsMainArgs rest
    findIsMainArgs (_ : rest) = findIsMainArgs rest

-- | Decode argument to '-main-is'
--
-- Returns 'Nothing' if argument set only the function name.
--
-- This code has been stolen/refactored from GHC's DynFlags.setMainIs
-- function. The logic here is deliberately imperfect as it is
-- intended to be bug-compatible with GHC's parser. See discussion in
-- https://github.com/haskell/cabal/pull/4539#discussion_r118981753.
decodeMainIsArg :: String -> Maybe ModuleName
decodeMainIsArg arg
  | headOf main_fn isLower =
      -- The arg looked like "Foo.Bar.baz"
      Just (ModuleName.fromString main_mod)
  | headOf arg isUpper -- The arg looked like "Foo" or "Foo.Bar"
    =
      Just (ModuleName.fromString arg)
  | otherwise -- The arg looked like "baz"
    =
      Nothing
  where
    headOf :: String -> (Char -> Bool) -> Bool
    headOf str pred' = any pred' (safeHead str)

    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

    splitLongestPrefix :: String -> (Char -> Bool) -> (String, String)
    splitLongestPrefix str pred'
      | null r_pre = (str, [])
      | otherwise = (reverse (safeTail r_pre), reverse r_suf)
      where
        -- 'safeTail' drops the char satisfying 'pred'
        (r_suf, r_pre) = break pred' (reverse str)

-- | Generic build function. See comment for 'GBuildMode'.
gbuild
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> GBuildMode
  -> ComponentLocalBuildInfo
  -> IO ()
gbuild verbosity numJobs pkg_descr lbi bm clbi = do
  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let replFlags = case bm of
        GReplExe flags _ -> flags
        GReplFLib flags _ -> flags
        GBuildExe{} -> mempty
        GBuildFLib{} -> mempty
      comp = compiler lbi
      platform = hostPlatform lbi
      implInfo = getImplInfo comp
      runGhcProg = runGHC verbosity ghcProg comp platform

  let bnfo = gbuildInfo bm

  -- the name that GHC really uses (e.g., with .exe on Windows for executables)
  let targetName = gbuildTargetName lbi bm
  let targetDir = buildDir lbi </> (gbuildName bm)
  let tmpDir = targetDir </> (gbuildName bm ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True tmpDir

  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = exeCoverage lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | gbuildIsRepl bm = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way (gbuildName bm)
        | otherwise = mempty

  rpaths <- getRPaths lbi clbi
  buildSources <- gbuildSources verbosity (package pkg_descr) (specVersion pkg_descr) tmpDir bm

  -- ensure extra lib dirs exist before passing to ghc
  cleanedExtraLibDirs <- filterM doesDirectoryExist (extraLibDirs bnfo)
  cleanedExtraLibDirsStatic <- filterM doesDirectoryExist (extraLibDirsStatic bnfo)

  let cSrcs = cSourcesFiles buildSources
      cxxSrcs = cxxSourceFiles buildSources
      jsSrcs = jsSourceFiles buildSources
      asmSrcs = asmSourceFiles buildSources
      cmmSrcs = cmmSourceFiles buildSources
      inputFiles = inputSourceFiles buildSources
      inputModules = inputSourceModules buildSources
      isGhcDynamic = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      cLikeObjs = map (`replaceExtension` objExtension) cSrcs
      cxxObjs = map (`replaceExtension` objExtension) cxxSrcs
      jsObjs = if hasJsSupport then map (`replaceExtension` objExtension) jsSrcs else []
      asmObjs = map (`replaceExtension` objExtension) asmSrcs
      cmmObjs = map (`replaceExtension` objExtension) cmmSrcs
      needDynamic = gbuildNeedDynamic lbi bm
      needProfiling = withProfExe lbi
      Platform hostArch _ = hostPlatform lbi
      hasJsSupport = hostArch == JavaScript

      -- build executables
      baseOpts =
        (componentGhcOptions verbosity lbi bnfo clbi tmpDir)
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptInputFiles =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter isHaskell inputFiles
                    else inputFiles
            , ghcOptInputScripts =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter (not . isHaskell) inputFiles
                    else []
            , ghcOptInputModules = toNubListR inputModules
            }
      staticOpts =
        baseOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticOnly
            , ghcOptHPCDir = hpcdir Hpc.Vanilla
            }
      profOpts =
        baseOpts
          `mappend` mempty
            { ghcOptProfilingMode = toFlag True
            , ghcOptProfilingAuto =
                Internal.profDetailLevelFlag
                  False
                  (withProfExeDetail lbi)
            , ghcOptHiSuffix = toFlag "p_hi"
            , ghcOptObjSuffix = toFlag "p_o"
            , ghcOptExtra = hcProfOptions GHC bnfo
            , ghcOptHPCDir = hpcdir Hpc.Prof
            }
      dynOpts =
        baseOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcDynamicOnly
            , -- TODO: Does it hurt to set -fPIC for executables?
              ghcOptFPic = toFlag True
            , ghcOptHiSuffix = toFlag "dyn_hi"
            , ghcOptObjSuffix = toFlag "dyn_o"
            , ghcOptExtra = hcSharedOptions GHC bnfo
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      dynTooOpts =
        staticOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic
            , ghcOptDynHiSuffix = toFlag "dyn_hi"
            , ghcOptDynObjSuffix = toFlag "dyn_o"
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      linkerOpts =
        mempty
          { ghcOptLinkOptions =
              PD.ldOptions bnfo
                ++ [ "-static"
                   | withFullyStaticExe lbi
                   ]
                -- Pass extra `ld-options` given
                -- through to GHC's linker.
                ++ maybe
                  []
                  programOverrideArgs
                  (lookupProgram ldProgram (withPrograms lbi))
          , ghcOptLinkLibs =
              if withFullyStaticExe lbi
                then extraLibsStatic bnfo
                else extraLibs bnfo
          , ghcOptLinkLibPath =
              toNubListR $
                if withFullyStaticExe lbi
                  then cleanedExtraLibDirsStatic
                  else cleanedExtraLibDirs
          , ghcOptLinkFrameworks =
              toNubListR $
                PD.frameworks bnfo
          , ghcOptLinkFrameworkDirs =
              toNubListR $
                PD.extraFrameworkDirs bnfo
          , ghcOptInputFiles =
              toNubListR
                [tmpDir </> x | x <- cLikeObjs ++ cxxObjs ++ jsObjs ++ cmmObjs ++ asmObjs]
          }
      dynLinkerOpts =
        mempty
          { ghcOptRPaths = rpaths
          , ghcOptInputFiles =
              toNubListR
                [tmpDir </> x | x <- cLikeObjs ++ cxxObjs ++ cmmObjs ++ asmObjs]
          }
      replOpts =
        baseOpts
          { ghcOptExtra =
              Internal.filterGhciFlags
                (ghcOptExtra baseOpts)
                <> replOptionsFlags replFlags
          , ghcOptInputModules = replNoLoad replFlags (ghcOptInputModules baseOpts)
          , ghcOptInputFiles = replNoLoad replFlags (ghcOptInputFiles baseOpts)
          }
          -- For a normal compile we do separate invocations of ghc for
          -- compiling as for linking. But for repl we have to do just
          -- the one invocation, so that one has to include all the
          -- linker stuff too, like -l flags and any .o files from C
          -- files etc.
          `mappend` linkerOpts
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeInteractive
            , ghcOptOptimisation = toFlag GhcNoOptimisation
            }
      commonOpts
        | needProfiling = profOpts
        | needDynamic = dynOpts
        | otherwise = staticOpts
      compileOpts
        | useDynToo = dynTooOpts
        | otherwise = commonOpts
      withStaticExe = not needProfiling && not needDynamic

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = usesTemplateHaskellOrQQ bnfo
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo =
        dynamicTooSupported
          && isGhcDynamic
          && doingTH
          && withStaticExe
          && null (hcSharedOptions GHC bnfo)
      compileTHOpts
        | isGhcDynamic = dynOpts
        | otherwise = staticOpts
      compileForTH
        | gbuildIsRepl bm = False
        | useDynToo = False
        | isGhcDynamic = doingTH && (needProfiling || withStaticExe)
        | otherwise = doingTH && (needProfiling || needDynamic)

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcProg
      compileTHOpts
        { ghcOptNoLink = toFlag True
        , ghcOptNumJobs = numJobs
        }

  -- Do not try to build anything if there are no input files.
  -- This can happen if the cabal file ends up with only cSrcs
  -- but no Haskell modules.
  unless
    ( (null inputFiles && null inputModules)
        || gbuildIsRepl bm
    )
    $ runGhcProg
      compileOpts
        { ghcOptNoLink = toFlag True
        , ghcOptNumJobs = numJobs
        }

  let
    buildExtraSources mkSrcOpts wantDyn = traverse_ $ buildExtraSource mkSrcOpts wantDyn
    buildExtraSource mkSrcOpts wantDyn filename = do
      let baseSrcOpts =
            mkSrcOpts
              verbosity
              implInfo
              lbi
              bnfo
              clbi
              tmpDir
              filename
          vanillaSrcOpts =
            if isGhcDynamic && wantDyn
              then -- Dynamic GHC requires C/C++ sources to be built
              -- with -fPIC for REPL to work. See #2207.
                baseSrcOpts{ghcOptFPic = toFlag True}
              else baseSrcOpts
          profSrcOpts =
            vanillaSrcOpts
              `mappend` mempty
                { ghcOptProfilingMode = toFlag True
                }
          sharedSrcOpts =
            vanillaSrcOpts
              `mappend` mempty
                { ghcOptFPic = toFlag True
                , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                }
          opts
            | needProfiling = profSrcOpts
            | needDynamic && wantDyn = sharedSrcOpts
            | otherwise = vanillaSrcOpts
          -- TODO: Placing all Haskell, C, & C++ objects in a single directory
          --       Has the potential for file collisions. In general we would
          --       consider this a user error. However, we should strive to
          --       add a warning if this occurs.
          odir = fromFlag (ghcOptObjDir opts)

      createDirectoryIfMissingVerbose verbosity True odir
      needsRecomp <- checkNeedsRecompilation filename opts
      when needsRecomp $
        runGhcProg opts

  -- build any C++ sources
  unless (null cxxSrcs) $ do
    info verbosity "Building C++ Sources..."
    buildExtraSources Internal.componentCxxGhcOptions True cxxSrcs

  -- build any C sources
  unless (null cSrcs) $ do
    info verbosity "Building C Sources..."
    buildExtraSources Internal.componentCcGhcOptions True cSrcs

  -- build any JS sources
  unless (not hasJsSupport || null jsSrcs) $ do
    info verbosity "Building JS Sources..."
    buildExtraSources Internal.componentJsGhcOptions False jsSrcs

  -- build any ASM sources
  unless (null asmSrcs) $ do
    info verbosity "Building Assembler Sources..."
    buildExtraSources Internal.componentAsmGhcOptions True asmSrcs

  -- build any Cmm sources
  unless (null cmmSrcs) $ do
    info verbosity "Building C-- Sources..."
    buildExtraSources Internal.componentCmmGhcOptions True cmmSrcs

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  case bm of
    GReplExe _ _ -> runReplOrWriteFlags verbosity ghcProg comp platform replFlags replOpts bnfo clbi (pkgName (PD.package pkg_descr))
    GReplFLib _ _ -> runReplOrWriteFlags verbosity ghcProg comp platform replFlags replOpts bnfo clbi (pkgName (PD.package pkg_descr))
    GBuildExe _ -> do
      let linkOpts =
            commonOpts
              `mappend` linkerOpts
              `mappend` mempty
                { ghcOptLinkNoHsMain = toFlag (null inputFiles)
                }
              `mappend` (if withDynExe lbi then dynLinkerOpts else mempty)

      info verbosity "Linking..."
      -- Work around old GHCs not relinking in this
      -- situation, see #3294
      let target = targetDir </> targetName
      when (compilerVersion comp < mkVersion [7, 7]) $ do
        e <- doesFileExist target
        when e (removeFile target)
      runGhcProg linkOpts{ghcOptOutputFile = toFlag target}
    GBuildFLib flib -> do
      let
        -- Instruct GHC to link against libHSrts.
        rtsLinkOpts :: GhcOptions
        rtsLinkOpts
          | supportsFLinkRts =
              mempty
                { ghcOptLinkRts = toFlag True
                }
          | otherwise =
              mempty
                { ghcOptLinkLibs = rtsOptLinkLibs
                , ghcOptLinkLibPath = toNubListR $ rtsLibPaths rtsInfo
                }
          where
            threaded = hasThreaded (gbuildInfo bm)
            supportsFLinkRts = compilerVersion comp >= mkVersion [9, 0]
            rtsInfo = extractRtsInfo lbi
            rtsOptLinkLibs =
              [ if needDynamic
                  then
                    if threaded
                      then dynRtsThreadedLib (rtsDynamicInfo rtsInfo)
                      else dynRtsVanillaLib (rtsDynamicInfo rtsInfo)
                  else
                    if threaded
                      then statRtsThreadedLib (rtsStaticInfo rtsInfo)
                      else statRtsVanillaLib (rtsStaticInfo rtsInfo)
              ]

        linkOpts :: GhcOptions
        linkOpts = case foreignLibType flib of
          ForeignLibNativeShared ->
            commonOpts
              `mappend` linkerOpts
              `mappend` dynLinkerOpts
              `mappend` rtsLinkOpts
              `mappend` mempty
                { ghcOptLinkNoHsMain = toFlag True
                , ghcOptShared = toFlag True
                , ghcOptFPic = toFlag True
                , ghcOptLinkModDefFiles = toNubListR $ gbuildModDefFiles bm
                }
          ForeignLibNativeStatic ->
            -- this should be caught by buildFLib
            -- (and if we do implement this, we probably don't even want to call
            -- ghc here, but rather Ar.createArLibArchive or something)
            cabalBug "static libraries not yet implemented"
          ForeignLibTypeUnknown ->
            cabalBug "unknown foreign lib type"
      -- We build under a (potentially) different filename to set a
      -- soname on supported platforms.  See also the note for
      -- @flibBuildName@.
      info verbosity "Linking..."
      let buildName = flibBuildName lbi flib
      runGhcProg linkOpts{ghcOptOutputFile = toFlag (targetDir </> buildName)}
      renameFile (targetDir </> buildName) (targetDir </> targetName)
