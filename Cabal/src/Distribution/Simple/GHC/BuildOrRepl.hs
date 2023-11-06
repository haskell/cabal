module Distribution.Simple.GHC.BuildOrRepl (buildOrReplLib) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad (forM_)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (Flag (..), fromFlag, toFlag)
import Distribution.Simple.GHC.Build
  ( checkNeedsRecompilation
  , componentGhcOptions
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
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.Ar as Ar
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.Ld as Ld
import Distribution.Simple.Setup.Config
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ParStrat
import Distribution.Utils.NubList
import Distribution.Verbosity
import Distribution.Version
import System.Directory
  ( doesDirectoryExist
  , makeRelativeToCurrentDirectory
  )
import System.FilePath
  ( replaceExtension
  , (</>)
  )

buildOrReplLib
  :: Maybe ReplOptions
  -> Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildOrReplLib mReplFlags verbosity numJobs pkg_descr lbi lib clbi = do
  let uid = componentUnitId clbi
      libTargetDir = componentBuildDir lbi clbi
      whenVanillaLib forceVanilla =
        when (forceVanilla || withVanillaLib lbi)
      whenProfLib = when (withProfLib lbi)
      whenSharedLib forceShared =
        when (forceShared || withSharedLib lbi)
      whenStaticLib forceStatic =
        when (forceStatic || withStaticLib lbi)
      whenGHCiLib = when (withGHCiLib lbi)
      forRepl = maybe False (const True) mReplFlags
      whenReplLib = forM_ mReplFlags
      replFlags = fromMaybe mempty mReplFlags
      comp = compiler lbi
      ghcVersion = compilerVersion comp
      implInfo = getImplInfo comp
      platform@(Platform hostArch hostOS) = hostPlatform lbi
      hasJsSupport = hostArch == JavaScript
      has_code = not (componentIsIndefinite clbi)

  relLibTargetDir <- makeRelativeToCurrentDirectory libTargetDir

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let runGhcProg = runGHC verbosity ghcProg comp platform

  let libBi = libBuildInfo lib

  -- ensure extra lib dirs exist before passing to ghc
  cleanedExtraLibDirs <- filterM doesDirectoryExist (extraLibDirs libBi)
  cleanedExtraLibDirsStatic <- filterM doesDirectoryExist (extraLibDirsStatic libBi)

  let isGhcDynamic = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      doingTH = usesTemplateHaskellOrQQ libBi
      forceVanillaLib = doingTH && not isGhcDynamic
      forceSharedLib = doingTH && isGhcDynamic
  -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = libCoverage lbi
      -- TODO: Historically HPC files have been put into a directory which
      -- has the package name.  I'm going to avoid changing this for
      -- now, but it would probably be better for this to be the
      -- component ID instead...
      pkg_name = prettyShow (PD.package pkg_descr)
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | forRepl = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way pkg_name
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cLikeSources =
        fromNubListR $
          mconcat
            [ toNubListR (cSources libBi)
            , toNubListR (cxxSources libBi)
            , toNubListR (cmmSources libBi)
            , toNubListR (asmSources libBi)
            , if hasJsSupport
                then -- JS files are C-like with GHC's JS backend: they are
                -- "compiled" into `.o` files (renamed with a header).
                -- This is a difference from GHCJS, for which we only
                -- pass the JS files at link time.
                  toNubListR (jsSources libBi)
                else mempty
            ]
      cLikeObjs = map (`replaceExtension` objExtension) cLikeSources
      baseOpts = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      vanillaOpts =
        baseOpts
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptNumJobs = numJobs
            , ghcOptInputModules = toNubListR $ allLibModules lib clbi
            , ghcOptHPCDir = hpcdir Hpc.Vanilla
            }

      profOpts =
        vanillaOpts
          `mappend` mempty
            { ghcOptProfilingMode = toFlag True
            , ghcOptProfilingAuto =
                Internal.profDetailLevelFlag
                  True
                  (withProfLibDetail lbi)
            , ghcOptHiSuffix = toFlag "p_hi"
            , ghcOptObjSuffix = toFlag "p_o"
            , ghcOptExtra = hcProfOptions GHC libBi
            , ghcOptHPCDir = hpcdir Hpc.Prof
            }

      sharedOpts =
        vanillaOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcDynamicOnly
            , ghcOptFPic = toFlag True
            , ghcOptHiSuffix = toFlag "dyn_hi"
            , ghcOptObjSuffix = toFlag "dyn_o"
            , ghcOptExtra = hcSharedOptions GHC libBi
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }
      linkerOpts =
        mempty
          { ghcOptLinkOptions =
              PD.ldOptions libBi
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
                then extraLibsStatic libBi
                else extraLibs libBi
          , ghcOptLinkLibPath =
              toNubListR $
                if withFullyStaticExe lbi
                  then cleanedExtraLibDirsStatic
                  else cleanedExtraLibDirs
          , ghcOptLinkFrameworks = toNubListR $ PD.frameworks libBi
          , ghcOptLinkFrameworkDirs =
              toNubListR $
                PD.extraFrameworkDirs libBi
          , ghcOptInputFiles =
              toNubListR
                [relLibTargetDir </> x | x <- cLikeObjs]
          }
      replOpts =
        vanillaOpts
          { ghcOptExtra =
              Internal.filterGhciFlags
                (ghcOptExtra vanillaOpts)
                <> replOptionsFlags replFlags
          , ghcOptNumJobs = mempty
          , ghcOptInputModules = replNoLoad replFlags (ghcOptInputModules vanillaOpts)
          }
          `mappend` linkerOpts
          `mappend` mempty
            { ghcOptMode = isInteractive
            , ghcOptOptimisation = toFlag GhcNoOptimisation
            }

      isInteractive = toFlag GhcModeInteractive

      vanillaSharedOpts =
        vanillaOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic
            , ghcOptDynHiSuffix = toFlag "dyn_hi"
            , ghcOptDynObjSuffix = toFlag "dyn_o"
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }

  unless (forRepl || null (allLibModules lib clbi)) $
    do
      let vanilla = whenVanillaLib forceVanillaLib (runGhcProg vanillaOpts)
          shared = whenSharedLib forceSharedLib (runGhcProg sharedOpts)
          useDynToo =
            dynamicTooSupported
              && (forceVanillaLib || withVanillaLib lbi)
              && (forceSharedLib || withSharedLib lbi)
              && null (hcSharedOptions GHC libBi)
      if not has_code
        then vanilla
        else
          if useDynToo
            then do
              runGhcProg vanillaSharedOpts
              case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
                (Flag dynDir, Flag vanillaDir) ->
                  -- When the vanilla and shared library builds are done
                  -- in one pass, only one set of HPC module interfaces
                  -- are generated. This set should suffice for both
                  -- static and dynamically linked executables. We copy
                  -- the modules interfaces so they are available under
                  -- both ways.
                  copyDirectoryRecursive verbosity dynDir vanillaDir
                _ -> return ()
            else
              if isGhcDynamic
                then do shared; vanilla
                else do vanilla; shared
      whenProfLib (runGhcProg profOpts)

  let
    buildExtraSources mkSrcOpts wantDyn = traverse_ $ buildExtraSource mkSrcOpts wantDyn
    buildExtraSource mkSrcOpts wantDyn filename = do
      let baseSrcOpts =
            mkSrcOpts
              verbosity
              implInfo
              lbi
              libBi
              clbi
              relLibTargetDir
              filename
          vanillaSrcOpts
            -- Dynamic GHC requires C sources to be built
            -- with -fPIC for REPL to work. See #2207.
            | isGhcDynamic && wantDyn = baseSrcOpts{ghcOptFPic = toFlag True}
            | otherwise = baseSrcOpts
          runGhcProgIfNeeded opts = do
            needsRecomp <- checkNeedsRecompilation filename opts
            when needsRecomp $ runGhcProg opts
          profSrcOpts =
            vanillaSrcOpts
              `mappend` mempty
                { ghcOptProfilingMode = toFlag True
                , ghcOptObjSuffix = toFlag "p_o"
                }
          sharedSrcOpts =
            vanillaSrcOpts
              `mappend` mempty
                { ghcOptFPic = toFlag True
                , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                , ghcOptObjSuffix = toFlag "dyn_o"
                }
          odir = fromFlag (ghcOptObjDir vanillaSrcOpts)

      createDirectoryIfMissingVerbose verbosity True odir
      runGhcProgIfNeeded vanillaSrcOpts
      unless (forRepl || not wantDyn) $
        whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedSrcOpts)
      unless forRepl $
        whenProfLib (runGhcProgIfNeeded profSrcOpts)

  -- Build any C++ sources separately.
  unless (not has_code || null (cxxSources libBi)) $ do
    info verbosity "Building C++ Sources..."
    buildExtraSources Internal.componentCxxGhcOptions True (cxxSources libBi)

  -- build any C sources
  unless (not has_code || null (cSources libBi)) $ do
    info verbosity "Building C Sources..."
    buildExtraSources Internal.componentCcGhcOptions True (cSources libBi)

  -- build any JS sources
  unless (not has_code || not hasJsSupport || null (jsSources libBi)) $ do
    info verbosity "Building JS Sources..."
    buildExtraSources Internal.componentJsGhcOptions False (jsSources libBi)

  -- build any ASM sources
  unless (not has_code || null (asmSources libBi)) $ do
    info verbosity "Building Assembler Sources..."
    buildExtraSources Internal.componentAsmGhcOptions True (asmSources libBi)

  -- build any Cmm sources
  unless (not has_code || null (cmmSources libBi)) $ do
    info verbosity "Building C-- Sources..."
    buildExtraSources Internal.componentCmmGhcOptions True (cmmSources libBi)

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  whenReplLib $ \rflags -> do
    when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
    runReplOrWriteFlags verbosity ghcProg comp platform rflags replOpts libBi clbi (pkgName (PD.package pkg_descr))

  -- link:
  when has_code . unless forRepl $ do
    info verbosity "Linking..."
    let cLikeProfObjs =
          map
            (`replaceExtension` ("p_" ++ objExtension))
            cLikeSources
        cLikeSharedObjs =
          map
            (`replaceExtension` ("dyn_" ++ objExtension))
            cLikeSources
        compiler_id = compilerId (compiler lbi)
        vanillaLibFilePath = relLibTargetDir </> mkLibName uid
        profileLibFilePath = relLibTargetDir </> mkProfLibName uid
        sharedLibFilePath =
          relLibTargetDir
            </> mkSharedLibName (hostPlatform lbi) compiler_id uid
        staticLibFilePath =
          relLibTargetDir
            </> mkStaticLibName (hostPlatform lbi) compiler_id uid
        ghciLibFilePath = relLibTargetDir </> Internal.mkGHCiLibName uid
        ghciProfLibFilePath = relLibTargetDir </> Internal.mkGHCiProfLibName uid
        libInstallPath =
          libdir $
            absoluteComponentInstallDirs
              pkg_descr
              lbi
              uid
              NoCopyDest
        sharedLibInstallPath =
          libInstallPath
            </> mkSharedLibName (hostPlatform lbi) compiler_id uid

    stubObjs <-
      catMaybes
        <$> sequenceA
          [ findFileWithExtension
            [objExtension]
            [libTargetDir]
            (ModuleName.toFilePath x ++ "_stub")
          | ghcVersion < mkVersion [7, 2] -- ghc-7.2+ does not make _stub.o files
          , x <- allLibModules lib clbi
          ]
    stubProfObjs <-
      catMaybes
        <$> sequenceA
          [ findFileWithExtension
            ["p_" ++ objExtension]
            [libTargetDir]
            (ModuleName.toFilePath x ++ "_stub")
          | ghcVersion < mkVersion [7, 2] -- ghc-7.2+ does not make _stub.o files
          , x <- allLibModules lib clbi
          ]
    stubSharedObjs <-
      catMaybes
        <$> sequenceA
          [ findFileWithExtension
            ["dyn_" ++ objExtension]
            [libTargetDir]
            (ModuleName.toFilePath x ++ "_stub")
          | ghcVersion < mkVersion [7, 2] -- ghc-7.2+ does not make _stub.o files
          , x <- allLibModules lib clbi
          ]

    hObjs <-
      Internal.getHaskellObjects
        implInfo
        lib
        lbi
        clbi
        relLibTargetDir
        objExtension
        True
    hProfObjs <-
      if withProfLib lbi
        then
          Internal.getHaskellObjects
            implInfo
            lib
            lbi
            clbi
            relLibTargetDir
            ("p_" ++ objExtension)
            True
        else return []
    hSharedObjs <-
      if withSharedLib lbi
        then
          Internal.getHaskellObjects
            implInfo
            lib
            lbi
            clbi
            relLibTargetDir
            ("dyn_" ++ objExtension)
            False
        else return []

    unless (null hObjs && null cLikeObjs && null stubObjs) $ do
      rpaths <- getRPaths lbi clbi

      let staticObjectFiles =
            hObjs
              ++ map (relLibTargetDir </>) cLikeObjs
              ++ stubObjs
          profObjectFiles =
            hProfObjs
              ++ map (relLibTargetDir </>) cLikeProfObjs
              ++ stubProfObjs
          dynamicObjectFiles =
            hSharedObjs
              ++ map (relLibTargetDir </>) cLikeSharedObjs
              ++ stubSharedObjs
          -- After the relocation lib is created we invoke ghc -shared
          -- with the dependencies spelled out as -package arguments
          -- and ghc invokes the linker with the proper library paths
          ghcSharedLinkArgs =
            mempty
              { ghcOptShared = toFlag True
              , ghcOptDynLinkMode = toFlag GhcDynamicOnly
              , ghcOptInputFiles = toNubListR dynamicObjectFiles
              , ghcOptOutputFile = toFlag sharedLibFilePath
              , ghcOptExtra = hcSharedOptions GHC libBi
              , -- For dynamic libs, Mac OS/X needs to know the install location
                -- at build time. This only applies to GHC < 7.8 - see the
                -- discussion in #1660.
                ghcOptDylibName =
                  if hostOS == OSX
                    && ghcVersion < mkVersion [7, 8]
                    then toFlag sharedLibInstallPath
                    else mempty
              , ghcOptHideAllPackages = toFlag True
              , ghcOptNoAutoLinkPackages = toFlag True
              , ghcOptPackageDBs = withPackageDB lbi
              , ghcOptThisUnitId = case clbi of
                  LibComponentLocalBuildInfo{componentCompatPackageKey = pk} ->
                    toFlag pk
                  _ -> mempty
              , ghcOptThisComponentId = case clbi of
                  LibComponentLocalBuildInfo
                    { componentInstantiatedWith = insts
                    } ->
                      if null insts
                        then mempty
                        else toFlag (componentComponentId clbi)
                  _ -> mempty
              , ghcOptInstantiatedWith = case clbi of
                  LibComponentLocalBuildInfo
                    { componentInstantiatedWith = insts
                    } ->
                      insts
                  _ -> []
              , ghcOptPackages =
                  toNubListR $
                    Internal.mkGhcOptPackages mempty clbi
              , ghcOptLinkLibs = extraLibs libBi
              , ghcOptLinkLibPath = toNubListR $ cleanedExtraLibDirs
              , ghcOptLinkFrameworks = toNubListR $ PD.frameworks libBi
              , ghcOptLinkFrameworkDirs =
                  toNubListR $ PD.extraFrameworkDirs libBi
              , ghcOptRPaths = rpaths
              }
          ghcStaticLinkArgs =
            mempty
              { ghcOptStaticLib = toFlag True
              , ghcOptInputFiles = toNubListR staticObjectFiles
              , ghcOptOutputFile = toFlag staticLibFilePath
              , ghcOptExtra = hcStaticOptions GHC libBi
              , ghcOptHideAllPackages = toFlag True
              , ghcOptNoAutoLinkPackages = toFlag True
              , ghcOptPackageDBs = withPackageDB lbi
              , ghcOptThisUnitId = case clbi of
                  LibComponentLocalBuildInfo{componentCompatPackageKey = pk} ->
                    toFlag pk
                  _ -> mempty
              , ghcOptThisComponentId = case clbi of
                  LibComponentLocalBuildInfo
                    { componentInstantiatedWith = insts
                    } ->
                      if null insts
                        then mempty
                        else toFlag (componentComponentId clbi)
                  _ -> mempty
              , ghcOptInstantiatedWith = case clbi of
                  LibComponentLocalBuildInfo
                    { componentInstantiatedWith = insts
                    } ->
                      insts
                  _ -> []
              , ghcOptPackages =
                  toNubListR $
                    Internal.mkGhcOptPackages mempty clbi
              , ghcOptLinkLibs = extraLibs libBi
              , ghcOptLinkLibPath = toNubListR $ cleanedExtraLibDirs
              }

      info verbosity (show (ghcOptPackages ghcSharedLinkArgs))

      whenVanillaLib False $ do
        Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles
        whenGHCiLib $ do
          (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
          Ld.combineObjectFiles
            verbosity
            lbi
            ldProg
            ghciLibFilePath
            staticObjectFiles

      whenProfLib $ do
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles
        whenGHCiLib $ do
          (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
          Ld.combineObjectFiles
            verbosity
            lbi
            ldProg
            ghciProfLibFilePath
            profObjectFiles

      whenSharedLib False $
        runGhcProg ghcSharedLinkArgs

      whenStaticLib False $
        runGhcProg ghcStaticLinkArgs
