{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.GHC.Build.Link where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set as Set
import Distribution.Backpack
import Distribution.Compat.Binary (encode)
import Distribution.Compat.ResponseFile
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Utils (cabalBug)
import Distribution.Pretty
import Distribution.Simple.Build.Inputs
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.GHC.Build.Modules
import Distribution.Simple.GHC.Build.Utils (exeTargetName, flibBuildName, flibTargetName, withDynFLib)
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PreProcess.Types
import Distribution.Simple.Program
import qualified Distribution.Simple.Program.Ar as Ar
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.Ld as Ld
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version

import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeFile
  , renameFile
  )
import System.FilePath
  ( isRelative
  , replaceExtension
  )

-- | Links together the object files of the Haskell modules and extra sources
-- using the context in which the component is being built.
--
-- If the build kind is 'BuildRepl', we load the component into GHCi instead of linking.
linkOrLoadComponent
  :: ConfiguredProgram
  -- ^ The configured GHC program that will be used for linking
  -> PackageDescription
  -- ^ The package description containing the component being built
  -> [SymbolicPath Pkg File]
  -- ^ The full list of extra build sources (all C, C++, Js,
  -- Asm, and Cmm sources), which were compiled to object
  -- files.
  -> (SymbolicPath Pkg (Dir Artifacts), SymbolicPath Pkg (Dir Build))
  -- ^ The build target dir, and the target dir.
  -- See Note [Build Target Dir vs Target Dir] in Distribution.Simple.GHC.Build
  -> ((Bool -> [BuildWay], Bool -> BuildWay, BuildWay), BuildWay -> GhcOptions)
  -- ^ The set of build ways wanted based on the user opts, and a function to
  -- convert a build way into the set of ghc options that were used to build
  -- that way.
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO ()
linkOrLoadComponent
  ghcProg
  pkg_descr
  extraSources
  (buildTargetDir, targetDir)
  ((wantedLibWays, wantedFLibWay, wantedExeWay), buildOpts)
  pbci = do
    let
      verbosity = buildVerbosity pbci
      target = targetInfo pbci
      component = buildComponent pbci
      what = buildingWhat pbci
      lbi = localBuildInfo pbci
      bi = buildBI pbci
      clbi = buildCLBI pbci
      isIndef = componentIsIndefinite clbi
      mbWorkDir = mbWorkDirLBI lbi
      tempFileOptions = commonSetupTempFileOptions $ buildingWhatCommonFlags what

      -- See Note [Symbolic paths] in Distribution.Utils.Path
      i = interpretSymbolicPathLBI lbi

    -- ensure extra lib dirs exist before passing to ghc
    cleanedExtraLibDirs <- liftIO $ filterM (doesDirectoryExist . i) (extraLibDirs bi)
    cleanedExtraLibDirsStatic <- liftIO $ filterM (doesDirectoryExist . i) (extraLibDirsStatic bi)

    let
      extraSourcesObjs :: [RelativePath Artifacts File]
      extraSourcesObjs =
        [ makeRelativePathEx $ getSymbolicPath src `replaceExtension` objExtension
        | src <- extraSources
        ]

      -- TODO: Shouldn't we use withStaticLib for libraries and something else
      -- for foreign libs in the three cases where we use `withFullyStaticExe` below?
      linkerOpts rpaths =
        mempty
          { ghcOptLinkOptions =
              [ "-static"
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
                then extraLibsStatic bi
                else extraLibs bi
          , ghcOptLinkLibPath =
              toNubListR $
                if withFullyStaticExe lbi
                  then cleanedExtraLibDirsStatic
                  else cleanedExtraLibDirs
          , ghcOptLinkFrameworks = toNubListR $ map getSymbolicPath $ PD.frameworks bi
          , ghcOptLinkFrameworkDirs = toNubListR $ PD.extraFrameworkDirs bi
          , ghcOptInputFiles =
              toNubListR
                [ coerceSymbolicPath $ buildTargetDir </> obj
                | obj <- extraSourcesObjs
                ]
          , ghcOptNoLink = Flag False
          , ghcOptRPaths = rpaths
          }

    case what of
      BuildRepl replFlags -> liftIO $ do
        let
          -- For repl we use the vanilla (static) ghc options
          staticOpts = buildOpts StaticWay
          replOpts =
            staticOpts
              { -- Repl options use Static as the base, but doesn't need to pass -static.
                -- However, it maybe should, for uniformity.
                ghcOptDynLinkMode = NoFlag
              , ghcOptExtra =
                  Internal.filterGhciFlags
                    (ghcOptExtra staticOpts)
                    <> replOptionsFlags (replReplOptions replFlags)
              }
              -- For a normal compile we do separate invocations of ghc for
              -- compiling as for linking. But for repl we have to do just
              -- the one invocation, so that one has to include all the
              -- linker stuff too, like -l flags and any .o files from C
              -- files etc.
              --
              -- TODO: The repl doesn't use the runtime paths from linkerOpts
              -- (ghcOptRPaths), which looks like a bug. After the refactor we
              -- can fix this.
              `mappend` linkerOpts mempty
              `mappend` mempty
                { ghcOptMode = toFlag GhcModeInteractive
                , ghcOptOptimisation = toFlag GhcNoOptimisation
                }
          replOpts_final =
            replOpts
              { ghcOptInputModules = replNoLoad (replReplOptions replFlags) (ghcOptInputModules replOpts)
              , ghcOptInputFiles = replNoLoad (replReplOptions replFlags) (ghcOptInputFiles replOpts)
              }

        -- TODO: problem here is we need the .c files built first, so we can load them
        -- with ghci, but .c files can depend on .h files generated by ghc by ffi
        -- exports.
        when (case component of CLib lib -> null (allLibModules lib clbi); _ -> False) $
          warn verbosity "No exposed modules"
        runReplOrWriteFlags
          ghcProg
          lbi
          replFlags
          replOpts_final
          (pkgName (PD.package pkg_descr))
          target
      _otherwise ->
        let
          runGhcProg =
            runGHCWithResponseFile
              "ghc.rsp"
              Nothing
              tempFileOptions
              verbosity
              ghcProg
              comp
              platform
              mbWorkDir
          platform = hostPlatform lbi
          comp = compiler lbi
          get_rpaths ways =
            if DynWay `Set.member` ways then getRPaths pbci else return (toNubListR [])
         in
          when (not $ componentIsIndefinite clbi) $ do
            -- If not building dynamically, we don't pass any runtime paths.
            liftIO $ do
              info verbosity "Linking..."
              let linkExeLike name = do
                    rpaths <- get_rpaths (Set.singleton wantedExeWay)
                    linkExecutable (linkerOpts rpaths) (wantedExeWay, buildOpts) targetDir name runGhcProg lbi
              case component of
                CLib lib -> do
                  let libWays = wantedLibWays isIndef
                  rpaths <- get_rpaths (Set.fromList libWays)
                  linkLibrary buildTargetDir cleanedExtraLibDirs pkg_descr verbosity runGhcProg lib lbi clbi extraSources rpaths libWays
                CFLib flib -> do
                  let flib_way = wantedFLibWay (withDynFLib flib)
                  rpaths <- get_rpaths (Set.singleton flib_way)
                  linkFLib flib bi lbi (linkerOpts rpaths) (flib_way, buildOpts) targetDir runGhcProg
                CExe exe -> linkExeLike (exeName exe)
                CTest test -> linkExeLike (testName test)
                CBench bench -> linkExeLike (benchmarkName bench)

-- | Link a library component
linkLibrary
  :: SymbolicPath Pkg (Dir Artifacts)
  -- ^ The library target build directory
  -> [SymbolicPath Pkg (Dir Lib)]
  -- ^ The list of extra lib dirs that exist (aka "cleaned")
  -> PackageDescription
  -- ^ The package description containing this library
  -> Verbosity
  -> (GhcOptions -> IO ())
  -- ^ Run the configured Ghc program
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> [SymbolicPath Pkg File]
  -- ^ Extra build sources (that were compiled to objects)
  -> NubListR FilePath
  -- ^ A list with the runtime-paths (rpaths), or empty if not linking dynamically
  -> [BuildWay]
  -- ^ Wanted build ways and corresponding build options
  -> IO ()
linkLibrary buildTargetDir cleanedExtraLibDirs pkg_descr verbosity runGhcProg lib lbi clbi extraSources rpaths wantedWays = do
  let
    common = configCommonFlags $ configFlags lbi
    mbWorkDir = flagToMaybe $ setupWorkingDir common

    compiler_id = compilerId comp
    comp = compiler lbi
    ghcVersion = compilerVersion comp
    implInfo = getImplInfo comp
    uid = componentUnitId clbi
    libBi = libBuildInfo lib
    Platform _hostArch hostOS = hostPlatform lbi
    vanillaLibFilePath = buildTargetDir </> makeRelativePathEx (mkLibName uid)
    profileLibFilePath = buildTargetDir </> makeRelativePathEx (mkProfLibName uid)
    sharedLibFilePath =
      buildTargetDir
        </> makeRelativePathEx (mkSharedLibName (hostPlatform lbi) compiler_id uid)
    profSharedLibFilePath =
      buildTargetDir
        </> makeRelativePathEx (mkProfSharedLibName (hostPlatform lbi) compiler_id uid)
    staticLibFilePath =
      buildTargetDir
        </> makeRelativePathEx (mkStaticLibName (hostPlatform lbi) compiler_id uid)
    ghciLibFilePath = buildTargetDir </> makeRelativePathEx (Internal.mkGHCiLibName uid)
    ghciProfLibFilePath = buildTargetDir </> makeRelativePathEx (Internal.mkGHCiProfLibName uid)
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
    profSharedLibInstallPath =
      libInstallPath
        </> mkProfSharedLibName (hostPlatform lbi) compiler_id uid

    getObjFiles :: BuildWay -> IO [SymbolicPath Pkg File]
    getObjFiles way =
      mconcat
        [ Internal.getHaskellObjects
            implInfo
            lib
            lbi
            clbi
            buildTargetDir
            (buildWayPrefix way ++ objExtension)
            True
        , pure $ map (srcObjPath way) extraSources
        , catMaybes
            <$> sequenceA
              [ findFileCwdWithExtension
                mbWorkDir
                [Suffix $ buildWayPrefix way ++ objExtension]
                [buildTargetDir]
                xPath
              | ghcVersion < mkVersion [7, 2] -- ghc-7.2+ does not make _stub.o files
              , x <- allLibModules lib clbi
              , let xPath :: RelativePath Artifacts File
                    xPath = makeRelativePathEx $ ModuleName.toFilePath x ++ "_stub"
              ]
        ]

    -- Get the @.o@ path from a source path (e.g. @.hs@),
    -- in the library target build directory.
    srcObjPath :: BuildWay -> SymbolicPath Pkg File -> SymbolicPath Pkg File
    srcObjPath way srcPath =
      case symbolicPathRelative_maybe objPath of
        -- Absolute path: should already be in the target build directory
        -- (e.g. a preprocessed file)
        -- TODO: assert this?
        Nothing -> objPath
        Just objRelPath -> coerceSymbolicPath buildTargetDir </> objRelPath
      where
        objPath = srcPath `replaceExtensionSymbolicPath` (buildWayPrefix way ++ objExtension)

    -- I'm fairly certain that, just like the executable, we can keep just the
    -- module input list, and point to the right sources dir (as is already
    -- done), and GHC will pick up the right suffix (p_ for profile, dyn_ when
    -- -shared...). The downside to doing this is that GHC would have to
    -- reconstruct the module graph again.
    -- That would mean linking the lib would be just like the executable, and
    -- we could more easily merge the two.
    --
    -- Right now, instead, we pass the path to each object file.
    ghcBaseLinkArgs =
      Internal.linkGhcOptions verbosity lbi libBi clbi
        <> mempty
          { ghcOptExtra = hcStaticOptions GHC libBi
          , ghcOptNoAutoLinkPackages = toFlag True
          }

    -- After the relocation lib is created we invoke ghc -shared
    -- with the dependencies spelled out as -package arguments
    -- and ghc invokes the linker with the proper library paths
    ghcSharedLinkArgs :: [SymbolicPath Pkg File] -> GhcOptions
    ghcSharedLinkArgs dynObjectFiles =
      ghcBaseLinkArgs
        { ghcOptShared = toFlag True
        , ghcOptDynLinkMode = toFlag GhcDynamicOnly
        , ghcOptInputFiles = toNubListR $ map coerceSymbolicPath dynObjectFiles
        , ghcOptOutputFile = toFlag sharedLibFilePath
        , -- For dynamic libs, Mac OS/X needs to know the install location
          -- at build time. This only applies to GHC < 7.8 - see the
          -- discussion in #1660.
          ghcOptDylibName =
            if hostOS == OSX
              && ghcVersion < mkVersion [7, 8]
              then toFlag sharedLibInstallPath
              else mempty
        , ghcOptLinkLibs = extraLibs libBi
        , ghcOptLinkLibPath = toNubListR $ cleanedExtraLibDirs
        , ghcOptLinkFrameworks = toNubListR $ map getSymbolicPath $ PD.frameworks libBi
        , ghcOptLinkFrameworkDirs =
            toNubListR $ PD.extraFrameworkDirs libBi
        , ghcOptRPaths = rpaths
        }
    ghcProfSharedLinkArgs pdynObjectFiles =
      ghcBaseLinkArgs
        { ghcOptShared = toFlag True
        , ghcOptProfilingMode = toFlag True
        , ghcOptProfilingAuto =
            Internal.profDetailLevelFlag
              True
              (withProfLibDetail lbi)
        , ghcOptDynLinkMode = toFlag GhcDynamicOnly
        , ghcOptInputFiles = toNubListR pdynObjectFiles
        , ghcOptOutputFile = toFlag profSharedLibFilePath
        , -- For dynamic libs, Mac OS/X needs to know the install location
          -- at build time. This only applies to GHC < 7.8 - see the
          -- discussion in #1660.
          ghcOptDylibName =
            if hostOS == OSX
              && ghcVersion < mkVersion [7, 8]
              then toFlag profSharedLibInstallPath
              else mempty
        , ghcOptLinkLibs = extraLibs libBi
        , ghcOptLinkLibPath = toNubListR $ cleanedExtraLibDirs
        , ghcOptLinkFrameworks = toNubListR $ map getSymbolicPath $ PD.frameworks libBi
        , ghcOptLinkFrameworkDirs =
            toNubListR $ PD.extraFrameworkDirs libBi
        , ghcOptRPaths = rpaths
        }
    ghcStaticLinkArgs staticObjectFiles =
      ghcBaseLinkArgs
        { ghcOptStaticLib = toFlag True
        , ghcOptInputFiles = toNubListR $ map coerceSymbolicPath staticObjectFiles
        , ghcOptOutputFile = toFlag staticLibFilePath
        , ghcOptLinkLibs = extraLibs libBi
        , -- TODO: Shouldn't this use cleanedExtraLibDirsStatic instead?
          ghcOptLinkLibPath = toNubListR $ cleanedExtraLibDirs
        }

  staticObjectFiles <- getObjFiles StaticWay
  profObjectFiles <- getObjFiles ProfWay
  dynamicObjectFiles <- getObjFiles DynWay
  profDynamicObjectFiles <- getObjFiles ProfDynWay

  let
    linkWay = \case
      ProfWay -> do
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles
        when (withGHCiLib lbi) $ do
          (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
          Ld.combineObjectFiles
            verbosity
            lbi
            ldProg
            ghciProfLibFilePath
            profObjectFiles
      ProfDynWay -> do
        runGhcProg $ ghcProfSharedLinkArgs profDynamicObjectFiles
      DynWay -> do
        runGhcProg $ ghcSharedLinkArgs dynamicObjectFiles
      StaticWay -> do
        when (withVanillaLib lbi) $ do
          Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles
          when (withGHCiLib lbi) $ do
            (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
            Ld.combineObjectFiles
              verbosity
              lbi
              ldProg
              ghciLibFilePath
              staticObjectFiles
        when (withStaticLib lbi) $ do
          runGhcProg $ ghcStaticLinkArgs staticObjectFiles

  -- ROMES: Why exactly branch on staticObjectFiles, rather than any other build
  -- kind that we might have wanted instead?
  -- This would be simpler by not adding every object to the invocation, and
  -- rather using module names.
  unless (null staticObjectFiles) $ do
    info verbosity (show (ghcOptPackages (Internal.componentGhcOptions verbosity lbi libBi clbi buildTargetDir)))
    traverse_ linkWay wantedWays

-- | Link the executable resulting from building this component, be it an
-- executable, test, or benchmark component.
linkExecutable
  :: (GhcOptions)
  -- ^ The linker-specific GHC options
  -> (BuildWay, BuildWay -> GhcOptions)
  -- ^ The wanted build ways and corresponding GhcOptions that were
  -- used to compile the modules in that way.
  -> SymbolicPath Pkg (Dir Build)
  -- ^ The target dir (2024-01:note: not the same as build target
  -- dir, see Note [Build Target Dir vs Target Dir] in Distribution.Simple.GHC.Build)
  -> UnqualComponentName
  -- ^ Name of executable-like target
  -> (GhcOptions -> IO ())
  -- ^ Run the configured GHC program
  -> LocalBuildInfo
  -> IO ()
linkExecutable linkerOpts (way, buildOpts) targetDir targetName runGhcProg lbi = do
  let baseOpts = buildOpts way
      linkOpts =
        baseOpts
          `mappend` linkerOpts
          `mappend` mempty
            { -- If there are no input Haskell files we pass -no-hs-main, and
              -- assume there is a main function in another non-haskell object
              ghcOptLinkNoHsMain = toFlag (ghcOptInputFiles baseOpts == mempty && ghcOptInputScripts baseOpts == mempty)
            }
      comp = compiler lbi

  -- Work around old GHCs not relinking in this
  -- situation, see #3294
  let target =
        targetDir </> makeRelativePathEx (exeTargetName (hostPlatform lbi) targetName)
  when (compilerVersion comp < mkVersion [7, 7]) $ do
    let targetPath = interpretSymbolicPathLBI lbi target
    e <- doesFileExist targetPath
    when e (removeFile targetPath)
  runGhcProg linkOpts{ghcOptOutputFile = toFlag target}

-- | Link a foreign library component
linkFLib
  :: ForeignLib
  -> BuildInfo
  -> LocalBuildInfo
  -> (GhcOptions)
  -- ^ The linker-specific GHC options
  -> (BuildWay, BuildWay -> GhcOptions)
  -- ^ The wanted build ways and corresponding GhcOptions that were
  -- used to compile the modules in that way.
  -> SymbolicPath Pkg (Dir Build)
  -- ^ The target dir (2024-01:note: not the same as build target
  -- dir, see Note [Build Target Dir vs Target Dir] in Distribution.Simple.GHC.Build)
  -> (GhcOptions -> IO ())
  -- ^ Run the configured GHC program
  -> IO ()
linkFLib flib bi lbi linkerOpts (way, buildOpts) targetDir runGhcProg = do
  let
    comp = compiler lbi

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
            , ghcOptLinkLibPath = toNubListR $ map makeSymbolicPath $ rtsLibPaths rtsInfo
            }
      where
        threaded = hasThreaded bi
        supportsFLinkRts = compilerVersion comp >= mkVersion [9, 0]
        rtsInfo = extractRtsInfo lbi
        rtsOptLinkLibs =
          [ if withDynFLib flib
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
        (buildOpts way)
          `mappend` linkerOpts
          `mappend` rtsLinkOpts
          `mappend` mempty
            { ghcOptLinkNoHsMain = toFlag True
            , ghcOptShared = toFlag True
            , ghcOptFPic = toFlag True
            , ghcOptLinkModDefFiles = toNubListR $ fmap getSymbolicPath $ foreignLibModDefFile flib
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
  let buildName = flibBuildName lbi flib
  let outFile = targetDir </> makeRelativePathEx buildName
  runGhcProg linkOpts{ghcOptOutputFile = toFlag outFile}
  let i = interpretSymbolicPathLBI lbi
  renameFile (i outFile) (i targetDir </> flibTargetName lbi flib)

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths
  :: PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (NubListR FilePath)
getRPaths pbci = do
  let
    lbi = localBuildInfo pbci
    bi = buildBI pbci
    clbi = buildCLBI pbci

    (Platform _ hostOS) = hostPlatform lbi
    compid = compilerId . compiler $ lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux = True
    supportRPaths Windows = False
    supportRPaths OSX = True
    supportRPaths FreeBSD =
      case compid of
        CompilerId GHC ver | ver >= mkVersion [7, 10, 2] -> True
        _ -> False
    supportRPaths OpenBSD = False
    supportRPaths NetBSD = False
    supportRPaths DragonFly = False
    supportRPaths Solaris = False
    supportRPaths AIX = False
    supportRPaths HPUX = False
    supportRPaths IRIX = False
    supportRPaths HaLVM = False
    supportRPaths IOS = False
    supportRPaths Android = False
    supportRPaths Ghcjs = False
    supportRPaths Wasi = False
    supportRPaths Hurd = True
    supportRPaths Haiku = False
    supportRPaths (OtherOS _) = False
  -- Do _not_ add a default case so that we get a warning here when a new OS
  -- is added.

  if supportRPaths hostOS
    then do
      libraryPaths <- liftIO $ depLibraryPaths False (relocatable lbi) lbi clbi
      let hostPref = case hostOS of
            OSX -> "@loader_path"
            _ -> "$ORIGIN"
          relPath p = if isRelative p then hostPref </> p else p
          rpaths =
            toNubListR (map relPath libraryPaths)
              <> toNubListR (map getSymbolicPath $ extraLibDirs bi)
      return rpaths
    else return mempty

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

-- | Load a target component into a repl, or write to disk a script which runs
-- GHCi with the GHC options Cabal elaborated to load the component interactively.
runReplOrWriteFlags
  :: ConfiguredProgram
  -> LocalBuildInfo
  -> ReplFlags
  -> GhcOptions
  -> PackageName
  -> TargetInfo
  -> IO ()
runReplOrWriteFlags ghcProg lbi rflags ghcOpts pkg_name target =
  let bi = componentBuildInfo $ targetComponent target
      clbi = targetCLBI target
      cname = componentName (targetComponent target)
      comp = compiler lbi
      platform = hostPlatform lbi
      common = configCommonFlags $ configFlags lbi
      mbWorkDir = mbWorkDirLBI lbi
      verbosity = fromFlag $ setupVerbosity common
      tempFileOptions = commonSetupTempFileOptions common
   in case replOptionsFlagOutput (replReplOptions rflags) of
        NoFlag ->
          runGHCWithResponseFile
            "ghc.rsp"
            Nothing
            tempFileOptions
            verbosity
            ghcProg
            comp
            platform
            mbWorkDir
            ghcOpts
        Flag out_dir -> do
          let uid = componentUnitId clbi
              this_unit = prettyShow uid
              getOpenModName (OpenModule _ mn) = Just mn
              getOpenModName (OpenModuleVar{}) = Nothing
              reexported_modules =
                [ (from_mn, to_mn) | LibComponentLocalBuildInfo{componentExposedModules = exposed_mods} <- [clbi], IPI.ExposedModule to_mn (Just m) <- exposed_mods, Just from_mn <- [getOpenModName m]
                ]
              renderReexportedModule (from_mn, to_mn)
                | reexportedAsSupported comp =
                    pure $ prettyShow from_mn ++ " as " ++ prettyShow to_mn
                | otherwise =
                    if from_mn == to_mn
                      then pure $ prettyShow to_mn
                      else dieWithException verbosity (MultiReplDoesNotSupportComplexReexportedModules pkg_name cname)
              hidden_modules = otherModules bi
              render_extra_opts = do
                rexp_mods <- mapM renderReexportedModule reexported_modules
                pure $
                  concat $
                    [ ["-this-package-name", prettyShow pkg_name]
                    , case mbWorkDir of
                        Nothing -> []
                        Just wd -> ["-working-dir", getSymbolicPath wd]
                    ]
                      ++ [ ["-reexported-module", m] | m <- rexp_mods
                         ]
                      ++ [ ["-hidden-module", prettyShow m] | m <- hidden_modules
                         ]
          -- Create "paths" subdirectory if it doesn't exist. This is where we write
          -- information about how the PATH was augmented.
          createDirectoryIfMissing False (out_dir </> "paths")
          -- Write out the PATH information into `paths` subdirectory.
          writeFileAtomic (out_dir </> "paths" </> this_unit) (encode ghcProg)
          -- Write out options for this component into a file ready for loading into
          -- the multi-repl
          extra_opts <- render_extra_opts
          writeFileAtomic (out_dir </> this_unit) $
            BS.pack $
              escapeArgs $
                extra_opts
                  ++ renderGhcOptions comp platform (ghcOpts{ghcOptMode = NoFlag})
                  ++ programOverrideArgs ghcProg

replNoLoad :: Ord a => ReplOptions -> NubListR a -> NubListR a
replNoLoad replFlags l
  | replOptionsNoLoad replFlags == Flag True = mempty
  | otherwise = l
