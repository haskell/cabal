module Distribution.Simple.GHC.BuildOrRepl (buildOrReplLib) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.Simple.GHC.Build.ExtraSources
import Distribution.Simple.Build.Monad
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.GHC.Build
  ( replNoLoad
  , runReplOrWriteFlags
  )
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.Ar as Ar
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.Ld as Ld
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ParStrat
import Distribution.Simple.GHC.Build.Modules
import Distribution.Utils.NubList
import Distribution.Version
import System.Directory
  ( doesDirectoryExist
  , makeRelativeToCurrentDirectory
  )
import System.FilePath
  ( replaceExtension
  , (</>)
  )
import Distribution.Simple.GHC.Build.Link

buildOrReplLib
  :: BuildingWhat
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildOrReplLib what numJobs pkg_descr lbi lib clbi = do
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
      forRepl = case what of BuildRepl{} -> True; _ -> False
      whenReplLib f = case what of BuildRepl flags -> f flags; _ -> pure ()
      replFlags = case what of BuildRepl flags -> replReplOptions flags; _ -> mempty
      comp = compiler lbi
      ghcVersion = compilerVersion comp
      implInfo = getImplInfo comp
      platform@(Platform hostArch hostOS) = hostPlatform lbi
      hasJsSupport = hostArch == JavaScript
      has_code = not (componentIsIndefinite clbi)
      verbosity = buildingWhatVerbosity what
      target = TargetInfo clbi (CLib lib)

  relLibTargetDir <- makeRelativeToCurrentDirectory libTargetDir

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let runGhcProg = runGHC verbosity ghcProg comp platform

  let libBi = libBuildInfo lib

  -- ensure extra lib dirs exist before passing to ghc
  cleanedExtraLibDirs <- filterM doesDirectoryExist (extraLibDirs libBi)
  cleanedExtraLibDirsStatic <- filterM doesDirectoryExist (extraLibDirsStatic libBi)

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = libCoverage lbi
      hpcdir way
        | forRepl = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir (libTargetDir </> extraCompilationArtifacts) way
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
      baseOpts = Internal.componentGhcOptions verbosity lbi libBi clbi libTargetDir
      vanillaOpts =
        baseOpts
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptNumJobs = numJobs
            , ghcOptInputModules = toNubListR $ allLibModules lib clbi
            , ghcOptHPCDir = hpcdir Hpc.Vanilla
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


  runBuildM what lbi target (buildHaskellModules numJobs ghcProg pkg_descr libTargetDir)
  runBuildM what lbi target (buildAllExtraSources ghcProg)

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  whenReplLib $ \rflags -> do
    when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
    runReplOrWriteFlags ghcProg lbi rflags replOpts target (pkgName (PD.package pkg_descr))

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
      rpaths <- runBuildM what lbi target getRPaths

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
