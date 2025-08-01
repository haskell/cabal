{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.GHCJS
  ( getGhcInfo
  , configure
  , configureCompiler
  , compilerProgramDb
  , getInstalledPackages
  , getInstalledPackagesMonitorFiles
  , getPackageDBContents
  , buildLib
  , buildFLib
  , buildExe
  , replLib
  , replFLib
  , replExe
  , startInterpreter
  , installLib
  , installFLib
  , installExe
  , libAbiHash
  , hcPkgInfo
  , registerPackage
  , componentGhcOptions
  , getLibDir
  , isDynamic
  , getGlobalPackageDB
  , pkgRoot
  , runCmd

    -- * Constructing and deconstructing GHC environment files
  , Internal.GhcEnvironmentFileEntry (..)
  , Internal.simpleGhcEnvironmentFile
  , Internal.renderGhcEnvironmentFile
  , Internal.writeGhcEnvironmentFile
  , Internal.ghcPlatformAndVersionString
  , readGhcEnvironmentFile
  , parseGhcEnvironmentFile
  , ParseErrorExc (..)

    -- * Version-specific implementation quirks
  , getImplInfo
  , GhcImplInfo (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

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
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import Distribution.Simple.GHC.EnvironmentParser
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PreProcess.Types
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.PackageName.Magic
import Distribution.Types.ParStrat
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity (Verbosity)
import Distribution.Version

import Control.Arrow ((***))
import Control.Monad (msum)
import Data.Char (isLower)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesFileExist
  , getAppUserDataDirectory
  , removeFile
  , renameFile
  )
import System.FilePath
  ( isRelative
  , replaceExtension
  , takeDirectory
  , takeExtension
  )
import qualified System.Info

-- -----------------------------------------------------------------------------
-- Configuring

-- | Configure GHCJS, and then auxiliary programs such as @ghc-pkg@, @haddock@
-- as well as toolchain programs such as @ar@, @ld.
configure
  :: Verbosity
  -> Maybe FilePath
  -- ^ user-specified @ghcjs@ path (optional)
  -> Maybe FilePath
  -- ^ user-specified @ghcjs-pkg@ path (optional)
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath hcPkgPath conf0 = do
  (comp, compPlatform, progdb1) <- configureCompiler verbosity hcPath conf0
  compProgDb <- compilerProgramDb verbosity comp progdb1 hcPkgPath
  return (comp, compPlatform, compProgDb)

-- | Configure GHCJS.
configureCompiler
  :: Verbosity
  -> Maybe FilePath
  -- ^ user-specified @ghc@ path (optional)
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configureCompiler verbosity hcPath conf0 = do
  (ghcjsProg, ghcjsVersion, progdb1) <-
    requireProgramVersion
      verbosity
      ghcjsProgram
      (orLaterVersion (mkVersion [0, 1]))
      (userMaybeSpecifyPath "ghcjs" hcPath conf0)

  Just ghcjsGhcVersion <- findGhcjsGhcVersion verbosity (programPath ghcjsProg)
  unless (ghcjsGhcVersion < mkVersion [8, 8]) $
    warn verbosity $
      "Unknown/unsupported 'ghc' version detected "
        ++ "(Cabal "
        ++ prettyShow cabalVersion
        ++ " supports 'ghc' version < 8.8): "
        ++ programPath ghcjsProg
        ++ " is based on GHC version "
        ++ prettyShow ghcjsGhcVersion

  let implInfo = ghcjsVersionImplInfo ghcjsVersion ghcjsGhcVersion

  languages <- Internal.getLanguages verbosity implInfo ghcjsProg
  extensions <- Internal.getExtensions verbosity implInfo ghcjsProg

  ghcjsInfo <- Internal.getGhcInfo verbosity implInfo ghcjsProg
  let ghcInfoMap = Map.fromList ghcjsInfo

  let comp =
        Compiler
          { compilerId = CompilerId GHCJS ghcjsVersion
          , compilerAbiTag =
              AbiTag $
                "ghc" ++ intercalate "_" (map show . versionNumbers $ ghcjsGhcVersion)
          , compilerCompat = [CompilerId GHC ghcjsGhcVersion]
          , compilerLanguages = languages
          , compilerExtensions = extensions
          , compilerProperties = ghcInfoMap
          }
      compPlatform = Internal.targetPlatform ghcjsInfo
  return (comp, compPlatform, progdb1)

-- | Given a configured @ghcjs@ program, configure auxiliary programs such
-- as @ghcjs-pkg@ or @haddock@, based on the location of the @ghcjs@ executable.
compilerProgramDb
  :: Verbosity
  -> Compiler
  -> ProgramDb
  -> Maybe FilePath
  -- ^ user-specified @ghc-pkg@ path (optional)
  -> IO ProgramDb
compilerProgramDb verbosity comp progdb1 hcPkgPath = do
  let
    ghcjsProg = fromJust $ lookupProgram ghcjsProgram progdb1
    ghcjsVersion = compilerVersion comp
    ghcjsGhcVersion = case compilerCompat comp of
      [CompilerId GHC ghcjsGhcVer] -> ghcjsGhcVer
      compat -> error $ "could not parse ghcjsGhcVersion:" ++ show compat

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcjsPkgProg, ghcjsPkgVersion, progdb2) <-
    requireProgramVersion
      verbosity
      ghcjsPkgProgram
        { programFindLocation = guessGhcjsPkgFromGhcjsPath ghcjsProg
        }
      anyVersion
      (userMaybeSpecifyPath "ghcjs-pkg" hcPkgPath progdb1)

  Just ghcjsPkgGhcjsVersion <-
    findGhcjsPkgGhcjsVersion
      verbosity
      (programPath ghcjsPkgProg)

  when (ghcjsVersion /= ghcjsPkgGhcjsVersion) $
    dieWithException verbosity $
      VersionMismatchJS
        (programPath ghcjsProg)
        ghcjsVersion
        (programPath ghcjsPkgProg)
        ghcjsPkgGhcjsVersion

  when (ghcjsGhcVersion /= ghcjsPkgVersion) $
    dieWithException verbosity $
      VersionMismatchGHCJS (programPath ghcjsProg) ghcjsGhcVersion (programPath ghcjsPkgProg) ghcjsPkgVersion

  -- Likewise we try to find the matching hsc2hs and haddock programs.
  let hsc2hsProgram' =
        hsc2hsProgram
          { programFindLocation =
              guessHsc2hsFromGhcjsPath ghcjsProg
          }
      haddockProgram' =
        haddockProgram
          { programFindLocation =
              guessHaddockFromGhcjsPath ghcjsProg
          }
      hpcProgram' =
        hpcProgram
          { programFindLocation = guessHpcFromGhcjsPath ghcjsProg
          }
      {-
      runghcProgram' = runghcProgram {
                        programFindLocation = guessRunghcFromGhcjsPath ghcjsProg
                    } -}
      progdb3 =
        addKnownProgram haddockProgram' $
          addKnownProgram hsc2hsProgram' $
            addKnownProgram hpcProgram' $
              {- addKnownProgram runghcProgram' -} progdb2

  return progdb3

guessGhcjsPkgFromGhcjsPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessGhcjsPkgFromGhcjsPath = guessToolFromGhcjsPath ghcjsPkgProgram

guessHsc2hsFromGhcjsPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessHsc2hsFromGhcjsPath = guessToolFromGhcjsPath hsc2hsProgram

guessHaddockFromGhcjsPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessHaddockFromGhcjsPath = guessToolFromGhcjsPath haddockProgram

guessHpcFromGhcjsPath
  :: ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessHpcFromGhcjsPath = guessToolFromGhcjsPath hpcProgram

guessToolFromGhcjsPath
  :: Program
  -> ConfiguredProgram
  -> Verbosity
  -> ProgramSearchPath
  -> IO (Maybe (FilePath, [FilePath]))
guessToolFromGhcjsPath tool ghcjsProg verbosity searchpath =
  do
    let toolname = programName tool
        given_path = programPath ghcjsProg
        given_dir = takeDirectory given_path
    real_path <- canonicalizePath given_path
    let real_dir = takeDirectory real_path
        versionSuffix path = takeVersionSuffix (dropExeExtension path)
        given_suf = versionSuffix given_path
        real_suf = versionSuffix real_path
        guessNormal dir = dir </> toolname <.> exeExtension buildPlatform
        guessGhcjs dir =
          dir
            </> (toolname ++ "-ghcjs")
              <.> exeExtension buildPlatform
        guessGhcjsVersioned dir suf =
          dir
            </> (toolname ++ "-ghcjs" ++ suf)
              <.> exeExtension buildPlatform
        guessVersioned dir suf =
          dir
            </> (toolname ++ suf)
              <.> exeExtension buildPlatform
        mkGuesses dir suf
          | null suf = [guessGhcjs dir, guessNormal dir]
          | otherwise =
              [ guessGhcjsVersioned dir suf
              , guessVersioned dir suf
              , guessGhcjs dir
              , guessNormal dir
              ]
        guesses =
          mkGuesses given_dir given_suf
            ++ if real_path == given_path
              then []
              else mkGuesses real_dir real_suf
    info verbosity $
      "looking for tool "
        ++ toolname
        ++ " near compiler in "
        ++ given_dir
    debug verbosity $ "candidate locations: " ++ show guesses
    exists <- traverse doesFileExist guesses
    case [file | (file, True) <- zip guesses exists] of
      -- If we can't find it near ghc, fall back to the usual
      -- method.
      [] -> programFindLocation tool verbosity searchpath
      (fp : _) -> do
        info verbosity $ "found " ++ toolname ++ " in " ++ fp
        let lookedAt =
              map fst
                . takeWhile (\(_file, exist) -> not exist)
                $ zip guesses exists
        return (Just (fp, lookedAt))
  where
    takeVersionSuffix :: FilePath -> String
    takeVersionSuffix = takeWhileEndLE isSuffixChar

    isSuffixChar :: Char -> Bool
    isSuffixChar c = isDigit c || c == '.' || c == '-'

getGhcInfo :: Verbosity -> ConfiguredProgram -> IO [(String, String)]
getGhcInfo verbosity ghcjsProg = Internal.getGhcInfo verbosity implInfo ghcjsProg
  where
    version = fromMaybe (error "GHCJS.getGhcInfo: no version") $ programVersion ghcjsProg
    implInfo = ghcVersionImplInfo version

-- | Given a single package DB, return all installed packages.
getPackageDBContents
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBX (SymbolicPath from (Dir PkgDB))
  -> ProgramDb
  -> IO InstalledPackageIndex
getPackageDBContents verbosity mbWorkDir packagedb progdb = do
  pkgss <- getInstalledPackages' verbosity mbWorkDir [packagedb] progdb
  toPackageIndex verbosity pkgss progdb

-- | Given a package DB stack, return all installed packages.
getInstalledPackages
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackX (SymbolicPath from (Dir PkgDB))
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity mbWorkDir packagedbs progdb = do
  checkPackageDbEnvVar verbosity
  checkPackageDbStack verbosity packagedbs
  pkgss <- getInstalledPackages' verbosity mbWorkDir packagedbs progdb
  index <- toPackageIndex verbosity pkgss progdb
  return $! index

toPackageIndex
  :: Verbosity
  -> [(PackageDBX a, [InstalledPackageInfo])]
  -> ProgramDb
  -> IO InstalledPackageIndex
toPackageIndex verbosity pkgss progdb = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- getLibDir' verbosity ghcjsProg
  let indices =
        [ PackageIndex.fromList (map (Internal.substTopDir topDir) pkgs)
        | (_, pkgs) <- pkgss
        ]
  return $! (mconcat indices)
  where
    ghcjsProg = fromMaybe (error "GHCJS.toPackageIndex no ghcjs program") $ lookupProgram ghcjsProgram progdb

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
  dropWhileEndLE isSpace
    `fmap` getDbProgramOutput
      verbosity
      ghcjsProgram
      (withPrograms lbi)
      ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity ghcjsProg =
  dropWhileEndLE isSpace
    `fmap` getProgramOutput verbosity ghcjsProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity ghcProg =
  dropWhileEndLE isSpace
    `fmap` getProgramOutput verbosity ghcProg ["--print-global-package-db"]

-- | Return the 'FilePath' to the per-user GHC package database.
getUserPackageDB :: Verbosity -> ConfiguredProgram -> Platform -> IO FilePath
getUserPackageDB _verbosity ghcjsProg platform = do
  -- It's rather annoying that we have to reconstruct this, because ghc
  -- hides this information from us otherwise. But for certain use cases
  -- like change monitoring it really can't remain hidden.
  appdir <- getAppUserDataDirectory "ghcjs"
  return (appdir </> platformAndVersion </> packageConfFileName)
  where
    platformAndVersion =
      Internal.ghcPlatformAndVersionString
        platform
        ghcjsVersion
    packageConfFileName = "package.conf.d"
    ghcjsVersion = fromMaybe (error "GHCJS.getUserPackageDB: no version") $ programVersion ghcjsProg

checkPackageDbEnvVar :: Verbosity -> IO ()
checkPackageDbEnvVar verbosity =
  Internal.checkPackageDbEnvVar verbosity "GHCJS" "GHCJS_PACKAGE_PATH"

checkPackageDbStack :: Eq fp => Verbosity -> PackageDBStackX fp -> IO ()
checkPackageDbStack _ (GlobalPackageDB : rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStack verbosity rest
  | GlobalPackageDB `notElem` rest =
      dieWithException verbosity GlobalPackageDBLimitation
checkPackageDbStack verbosity _ =
  dieWithException verbosity GlobalPackageDBSpecifiedFirst

getInstalledPackages'
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> [PackageDBX (SymbolicPath from (Dir PkgDB))]
  -> ProgramDb
  -> IO [(PackageDBX (SymbolicPath from (Dir PkgDB)), [InstalledPackageInfo])]
getInstalledPackages' verbosity mbWorkDir packagedbs progdb =
  sequenceA
    [ do
      pkgs <- HcPkg.dump (hcPkgInfo progdb) verbosity mbWorkDir packagedb
      return (packagedb, pkgs)
    | packagedb <- packagedbs
    ]

-- | Get the packages from specific PackageDBs, not cumulative.
getInstalledPackagesMonitorFiles
  :: Verbosity
  -> Platform
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> ProgramDb
  -> [PackageDB]
  -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity platform mbWorkDir progdb =
  traverse getPackageDBPath
  where
    getPackageDBPath :: PackageDB -> IO FilePath
    getPackageDBPath GlobalPackageDB =
      selectMonitorFile =<< getGlobalPackageDB verbosity ghcjsProg
    getPackageDBPath UserPackageDB =
      selectMonitorFile =<< getUserPackageDB verbosity ghcjsProg platform
    getPackageDBPath (SpecificPackageDB path) = selectMonitorFile (interpretSymbolicPath mbWorkDir path)

    -- GHC has old style file dbs, and new style directory dbs.
    -- Note that for dir style dbs, we only need to monitor the cache file, not
    -- the whole directory. The ghc program itself only reads the cache file
    -- so it's safe to only monitor this one file.
    selectMonitorFile path0 = do
      let path =
            if isRelative path0
              then interpretSymbolicPath mbWorkDir (makeRelativePathEx path0)
              else path0
      isFileStyle <- doesFileExist path
      if isFileStyle
        then return path
        else return (path </> "package.cache")

    ghcjsProg = fromMaybe (error "GHCJS.toPackageIndex no ghcjs program") $ lookupProgram ghcjsProgram progdb

toJSLibName :: String -> String
toJSLibName lib
  | takeExtension lib `elem` [".dll", ".dylib", ".so"] =
      replaceExtension lib "js_so"
  | takeExtension lib == ".a" = replaceExtension lib "js_a"
  | otherwise = lib <.> "js_a"

-- -----------------------------------------------------------------------------
-- Building a library

buildLib
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib = buildOrReplLib Nothing

replLib
  :: [String]
  -> Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
replLib = buildOrReplLib . Just

buildOrReplLib
  :: Maybe [String]
  -> Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildOrReplLib mReplFlags verbosity numJobs _pkg_descr lbi lib clbi = do
  let uid = componentUnitId clbi
      libTargetDir = componentBuildDir lbi clbi
      whenVanillaLib forceVanilla =
        when (forceVanilla || withVanillaLib lbi)
      whenProfLib = when (withProfLib lbi)
      whenSharedLib forceShared =
        when (forceShared || withSharedLib lbi)
      whenStaticLib forceStatic =
        when (forceStatic || withStaticLib lbi)
      -- whenGHCiLib = when (withGHCiLib lbi)
      forRepl = maybe False (const True) mReplFlags
      -- ifReplLib = when forRepl
      comp = compiler lbi
      implInfo = getImplInfo comp
      platform@(Platform _hostArch _hostOS) = hostPlatform lbi
      has_code = not (componentIsIndefinite clbi)
      mbWorkDir = mbWorkDirLBI lbi

      -- See Note [Symbolic paths] in Distribution.Utils.Path
      i = interpretSymbolicPathLBI lbi
      u :: SymbolicPathX allowAbs Pkg to -> FilePath
      u = getSymbolicPath

  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  let runGhcjsProg = runGHC verbosity ghcjsProg comp platform mbWorkDir

  let libBi = libBuildInfo lib

  -- fixme flags shouldn't depend on ghcjs being dynamic or not
  let isGhcjsDynamic = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      doingTH = usesTemplateHaskellOrQQ libBi
      forceVanillaLib = doingTH && not isGhcjsDynamic
      forceSharedLib = doingTH && isGhcjsDynamic
  -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = libCoverage lbi
      hpcdir way
        | forRepl = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir (coerceSymbolicPath libTargetDir </> coerceSymbolicPath extraCompilationArtifacts) way
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True $ i libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cLikeFiles = fromNubListR $ toNubListR (cSources libBi) <> toNubListR (cxxSources libBi)
      jsSrcs = jsSources libBi
      cObjs = map ((`replaceExtensionSymbolicPath` objExtension)) cLikeFiles
      baseOpts = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      linkJsLibOpts =
        mempty
          { ghcOptExtra =
              [ "-link-js-lib"
              , getHSLibraryName uid
              , "-js-lib-outputdir"
              , u libTargetDir
              ]
                ++ map u jsSrcs
          }
      vanillaOptsNoJsLib =
        baseOpts
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptNumJobs = numJobs
            , ghcOptInputModules = toNubListR $ allLibModules lib clbi
            , ghcOptHPCDir = hpcdir Hpc.Vanilla
            }
      vanillaOpts = vanillaOptsNoJsLib `mappend` linkJsLibOpts

      profOpts =
        adjustExts "p_hi" "p_o" vanillaOpts
          `mappend` mempty
            { ghcOptProfilingMode = toFlag True
            , ghcOptProfilingAuto =
                Internal.profDetailLevelFlag
                  True
                  (withProfLibDetail lbi)
            , --  ghcOptHiSuffix      = toFlag "p_hi",
              --  ghcOptObjSuffix     = toFlag "p_o",
              ghcOptExtra = hcProfOptions GHC libBi
            , ghcOptHPCDir = hpcdir Hpc.Prof
            }

      sharedOpts =
        adjustExts "dyn_hi" "dyn_o" vanillaOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcDynamicOnly
            , ghcOptFPic = toFlag True
            , --  ghcOptHiSuffix    = toFlag "dyn_hi",
              --  ghcOptObjSuffix   = toFlag "dyn_o",
              ghcOptExtra = hcOptions GHC libBi ++ hcSharedOptions GHC libBi
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }

      vanillaSharedOpts =
        vanillaOpts
          `mappend` mempty
            { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic
            , ghcOptDynHiSuffix = toFlag "js_dyn_hi"
            , ghcOptDynObjSuffix = toFlag "js_dyn_o"
            , ghcOptHPCDir = hpcdir Hpc.Dyn
            }

  unless (forRepl || null (allLibModules lib clbi) && null jsSrcs && null cObjs) $
    do
      let vanilla = whenVanillaLib forceVanillaLib (runGhcjsProg vanillaOpts)
          shared = whenSharedLib forceSharedLib (runGhcjsProg sharedOpts)
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
              runGhcjsProg vanillaSharedOpts
              case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
                (Flag dynDir, Flag vanillaDir) ->
                  -- When the vanilla and shared library builds are done
                  -- in one pass, only one set of HPC module interfaces
                  -- are generated. This set should suffice for both
                  -- static and dynamically linked executables. We copy
                  -- the modules interfaces so they are available under
                  -- both ways.
                  copyDirectoryRecursive verbosity (i dynDir) (i vanillaDir)
                _ -> return ()
            else
              if isGhcjsDynamic
                then do shared; vanilla
                else do vanilla; shared
      whenProfLib (runGhcjsProg profOpts)

  -- Build any C++ sources separately.
  {-
    unless (not has_code || null (cxxSources libBi) || not nativeToo) $ do
      info verbosity "Building C++ Sources..."
      sequence_
        [ do let baseCxxOpts    = Internal.componentCxxGhcOptions verbosity implInfo
                                  lbi libBi clbi libTargetDir filename
                 vanillaCxxOpts = if isGhcjsDynamic
                                  then baseCxxOpts { ghcOptFPic = toFlag True }
                                  else baseCxxOpts
                 profCxxOpts    = vanillaCxxOpts `mappend` mempty {
                                    ghcOptProfilingMode = toFlag True,
                                    ghcOptObjSuffix     = toFlag "p_o"
                                  }
                 sharedCxxOpts  = vanillaCxxOpts `mappend` mempty {
                                   ghcOptFPic        = toFlag True,
                                   ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                   ghcOptObjSuffix   = toFlag "dyn_o"
                                 }
                 odir           = fromFlag (ghcOptObjDir vanillaCxxOpts)
             createDirectoryIfMissingVerbose verbosity True odir
             let runGhcProgIfNeeded cxxOpts = do
                   needsRecomp <- checkNeedsRecompilation filename cxxOpts
                   when needsRecomp $ runGhcjsProg cxxOpts
             runGhcProgIfNeeded vanillaCxxOpts
             unless forRepl $
               whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedCxxOpts)
             unless forRepl $ whenProfLib   (runGhcProgIfNeeded   profCxxOpts)
        | filename <- cxxSources libBi]

    ifReplLib $ do
      when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
      ifReplLib (runGhcjsProg replOpts)
  -}
  -- build any C sources
  -- TODO: Add support for S and CMM files.
  {-
    unless (not has_code || null (cSources libBi) || not nativeToo) $ do
      info verbosity "Building C Sources..."
      sequence_
        [ do let baseCcOpts    = Internal.componentCcGhcOptions verbosity implInfo
                                 lbi libBi clbi libTargetDir filename
                 vanillaCcOpts = if isGhcjsDynamic
                                 -- Dynamic GHC requires C sources to be built
                                 -- with -fPIC for REPL to work. See #2207.
                                 then baseCcOpts { ghcOptFPic = toFlag True }
                                 else baseCcOpts
                 profCcOpts    = vanillaCcOpts `mappend` mempty {
                                   ghcOptProfilingMode = toFlag True,
                                   ghcOptObjSuffix     = toFlag "p_o"
                                 }
                 sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                   ghcOptFPic        = toFlag True,
                                   ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                   ghcOptObjSuffix   = toFlag "dyn_o"
                                 }
                 odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
             createDirectoryIfMissingVerbose verbosity True odir
             let runGhcProgIfNeeded ccOpts = do
                   needsRecomp <- checkNeedsRecompilation filename ccOpts
                   when needsRecomp $ runGhcjsProg ccOpts
             runGhcProgIfNeeded vanillaCcOpts
             unless forRepl $
               whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedCcOpts)
             unless forRepl $ whenProfLib (runGhcProgIfNeeded profCcOpts)
        | filename <- cSources libBi]
  -}
  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.

  -- link:

  when has_code . when False {- fixme nativeToo -} . unless forRepl $ do
    info verbosity "Linking..."
    let cSharedObjs =
          map
            ((`replaceExtensionSymbolicPath` ("dyn_" ++ objExtension)))
            (cSources libBi ++ cxxSources libBi)
        compiler_id = compilerId (compiler lbi)
        sharedLibFilePath = libTargetDir </> makeRelativePathEx (mkSharedLibName (hostPlatform lbi) compiler_id uid)
        staticLibFilePath = libTargetDir </> makeRelativePathEx (mkStaticLibName (hostPlatform lbi) compiler_id uid)

    let stubObjs = []
        stubSharedObjs = []

    {-
        stubObjs <- catMaybes <$> sequenceA
          [ findFileWithExtension [objExtension] [libTargetDir]
              (ModuleName.toFilePath x ++"_stub")
          | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
          , x <- allLibModules lib clbi ]
        stubProfObjs <- catMaybes <$> sequenceA
          [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
              (ModuleName.toFilePath x ++"_stub")
          | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
          , x <- allLibModules lib clbi ]
        stubSharedObjs <- catMaybes <$> sequenceA
          [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
              (ModuleName.toFilePath x ++"_stub")
          | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
          , x <- allLibModules lib clbi ]
    -}
    hObjs <-
      Internal.getHaskellObjects
        implInfo
        lib
        lbi
        clbi
        (coerceSymbolicPath libTargetDir)
        objExtension
        True
    hSharedObjs <-
      if withSharedLib lbi
        then
          Internal.getHaskellObjects
            implInfo
            lib
            lbi
            clbi
            (coerceSymbolicPath libTargetDir)
            ("dyn_" ++ objExtension)
            False
        else return []

    unless (null hObjs && null cObjs && null stubObjs) $ do
      rpaths <- getRPaths lbi clbi

      let staticObjectFiles =
            hObjs
              ++ map (makeSymbolicPath . (getSymbolicPath libTargetDir </>) . getSymbolicPath) cObjs
              ++ stubObjs
          dynamicObjectFiles =
            hSharedObjs
              ++ map (makeSymbolicPath . (getSymbolicPath libTargetDir </>) . getSymbolicPath) cSharedObjs
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
              , ghcOptExtra = hcOptions GHC libBi ++ hcSharedOptions GHC libBi
              , -- For dynamic libs, Mac OS/X needs to know the install location
                -- at build time. This only applies to GHC < 7.8 - see the
                -- discussion in #1660.
                {-
                    ghcOptDylibName          = if hostOS == OSX
                                                  && ghcVersion < mkVersion [7,8]
                                                then toFlag sharedLibInstallPath
                                                else mempty, -}
                ghcOptHideAllPackages = toFlag True
              , ghcOptNoAutoLinkPackages = toFlag True
              , ghcOptPackageDBs = withPackageDB lbi
              , ghcOptThisUnitId = case clbi of
                  LibComponentLocalBuildInfo{componentCompatPackageKey = pk} ->
                    toFlag pk
                  _ -> mempty
              , ghcOptThisComponentId = case clbi of
                  LibComponentLocalBuildInfo{componentInstantiatedWith = insts} ->
                    if null insts
                      then mempty
                      else toFlag (componentComponentId clbi)
                  _ -> mempty
              , ghcOptInstantiatedWith = case clbi of
                  LibComponentLocalBuildInfo{componentInstantiatedWith = insts} ->
                    insts
                  _ -> []
              , ghcOptPackages =
                  toNubListR $
                    Internal.mkGhcOptPackages mempty clbi
              , ghcOptLinkLibs = extraLibs libBi
              , ghcOptLinkLibPath = toNubListR $ extraLibDirs libBi
              , ghcOptLinkFrameworks = toNubListR $ map getSymbolicPath $ PD.frameworks libBi
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
                  LibComponentLocalBuildInfo{componentInstantiatedWith = insts} ->
                    if null insts
                      then mempty
                      else toFlag (componentComponentId clbi)
                  _ -> mempty
              , ghcOptInstantiatedWith = case clbi of
                  LibComponentLocalBuildInfo{componentInstantiatedWith = insts} ->
                    insts
                  _ -> []
              , ghcOptPackages =
                  toNubListR $
                    Internal.mkGhcOptPackages mempty clbi
              , ghcOptLinkLibs = extraLibs libBi
              , ghcOptLinkLibPath = toNubListR $ extraLibDirs libBi
              }

      info verbosity (show (ghcOptPackages ghcSharedLinkArgs))
      {-
            whenVanillaLib False $ do
              Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles
              whenGHCiLib $ do
                (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
                Ld.combineObjectFiles verbosity lbi ldProg
                  ghciLibFilePath staticObjectFiles
                  -}
      {-
            whenProfLib $ do
              Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles
              whenGHCiLib $ do
                (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
                Ld.combineObjectFiles verbosity lbi ldProg
                  ghciProfLibFilePath profObjectFiles
      -}
      whenSharedLib False $
        runGhcjsProg ghcSharedLinkArgs

      whenStaticLib False $
        runGhcjsProg ghcStaticLinkArgs

-- | Start a REPL without loading any source files.
startInterpreter
  :: Verbosity
  -> ProgramDb
  -> Compiler
  -> Platform
  -> PackageDBStack
  -> IO ()
startInterpreter verbosity progdb comp platform packageDBs = do
  let replOpts =
        mempty
          { ghcOptMode = toFlag GhcModeInteractive
          , ghcOptPackageDBs = packageDBs
          }
  checkPackageDbStack verbosity packageDBs
  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram progdb
  runGHC verbosity ghcjsProg comp platform Nothing replOpts

-- -----------------------------------------------------------------------------
-- Building an executable or foreign library

-- | Build a foreign library
buildFLib
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO ()
buildFLib v njobs pkg lbi = gbuild v njobs pkg lbi . GBuildFLib

replFLib
  :: [String]
  -> Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO ()
replFLib replFlags v njobs pkg lbi =
  gbuild v njobs pkg lbi . GReplFLib replFlags

-- | Build an executable with GHC.
buildExe
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe v njobs pkg lbi = gbuild v njobs pkg lbi . GBuildExe

replExe
  :: [String]
  -> Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
replExe replFlags v njobs pkg lbi =
  gbuild v njobs pkg lbi . GReplExe replFlags

-- | Building an executable, starting the REPL, and building foreign
-- libraries are all very similar and implemented in 'gbuild'. The
-- 'GBuildMode' distinguishes between the various kinds of operation.
data GBuildMode
  = GBuildExe Executable
  | GReplExe [String] Executable
  | GBuildFLib ForeignLib
  | GReplFLib [String] ForeignLib

gbuildInfo :: GBuildMode -> BuildInfo
gbuildInfo (GBuildExe exe) = buildInfo exe
gbuildInfo (GReplExe _ exe) = buildInfo exe
gbuildInfo (GBuildFLib flib) = foreignLibBuildInfo flib
gbuildInfo (GReplFLib _ flib) = foreignLibBuildInfo flib

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

exeTargetName :: Platform -> Executable -> String
exeTargetName platform exe = unUnqualComponentName (exeName exe) `withExt` exeExtension platform

-- | Target name for a foreign library (the actual file name)
--
-- We do not use mkLibName and co here because the naming for foreign libraries
-- is slightly different (we don't use "_p" or compiler version suffices, and we
-- don't want the "lib" prefix on Windows).
--
-- TODO: We do use `dllExtension` and co here, but really that's wrong: they
-- use the OS used to build cabal to determine which extension to use, rather
-- than the target OS (but this is wrong elsewhere in Cabal as well).
flibTargetName :: LocalBuildInfo -> ForeignLib -> String
flibTargetName lbi flib =
  case (os, foreignLibType flib) of
    (Windows, ForeignLibNativeShared) -> nm <.> "dll"
    (Windows, ForeignLibNativeStatic) -> nm <.> "lib"
    (Linux, ForeignLibNativeShared) -> "lib" ++ nm <.> versionedExt
    (_other, ForeignLibNativeShared) -> "lib" ++ nm <.> dllExtension (hostPlatform lbi)
    (_other, ForeignLibNativeStatic) -> "lib" ++ nm <.> staticLibExtension (hostPlatform lbi)
    (_any, ForeignLibTypeUnknown) -> cabalBug "unknown foreign lib type"
  where
    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

    os :: OS
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    -- If a foreign lib foo has lib-version-info 5:1:2 or
    -- lib-version-linux 3.2.1, it should be built as libfoo.so.3.2.1
    -- Libtool's version-info data is translated into library versions in a
    -- nontrivial way: so refer to libtool documentation.
    versionedExt :: String
    versionedExt =
      let nums = foreignLibVersion flib os
       in foldl (<.>) "so" (map show nums)

-- | Name for the library when building.
--
-- If the `lib-version-info` field or the `lib-version-linux` field of
-- a foreign library target is set, we need to incorporate that
-- version into the SONAME field.
--
-- If a foreign library foo has lib-version-info 5:1:2, it should be
-- built as libfoo.so.3.2.1.  We want it to get soname libfoo.so.3.
-- However, GHC does not allow overriding soname by setting linker
-- options, as it sets a soname of its own (namely the output
-- filename), after the user-supplied linker options.  Hence, we have
-- to compile the library with the soname as its filename.  We rename
-- the compiled binary afterwards.
--
-- This method allows to adjust the name of the library at build time
-- such that the correct soname can be set.
flibBuildName :: LocalBuildInfo -> ForeignLib -> String
flibBuildName lbi flib
  -- On linux, if a foreign-library has version data, the first digit is used
  -- to produce the SONAME.
  | (os, foreignLibType flib)
      == (Linux, ForeignLibNativeShared) =
      let nums = foreignLibVersion flib os
       in "lib" ++ nm <.> foldl (<.>) "so" (map show (take 1 nums))
  | otherwise = flibTargetName lbi flib
  where
    os :: OS
    os =
      let (Platform _ os') = hostPlatform lbi
       in os'

    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

gbuildIsRepl :: GBuildMode -> Bool
gbuildIsRepl (GBuildExe _) = False
gbuildIsRepl (GReplExe _ _) = True
gbuildIsRepl (GBuildFLib _) = False
gbuildIsRepl (GReplFLib _ _) = True

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

gbuildModDefFiles :: GBuildMode -> [RelativePath Source File]
gbuildModDefFiles (GBuildExe _) = []
gbuildModDefFiles (GReplExe _ _) = []
gbuildModDefFiles (GBuildFLib flib) = foreignLibModDefFile flib
gbuildModDefFiles (GReplFLib _ flib) = foreignLibModDefFile flib

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

-- | A collection of:
--    * C input files
--    * C++ input files
--    * GHC input files
--    * GHC input modules
--
-- Used to correctly build and link sources.
data BuildSources = BuildSources
  { cSourcesFiles :: [SymbolicPath Pkg File]
  , cxxSourceFiles :: [SymbolicPath Pkg File]
  , inputSourceFiles :: [SymbolicPath Pkg File]
  , inputSourceModules :: [ModuleName]
  }

-- | Locate and return the 'BuildSources' required to build and link.
gbuildSources
  :: Verbosity
  -> Maybe (SymbolicPath CWD ('Dir Pkg))
  -> PackageId
  -> CabalSpecVersion
  -> SymbolicPath Pkg (Dir Source)
  -> GBuildMode
  -> IO BuildSources
gbuildSources verbosity mbWorkDir pkgId specVer tmpDir bm =
  case bm of
    GBuildExe exe -> exeSources exe
    GReplExe _ exe -> exeSources exe
    GBuildFLib flib -> return $ flibSources flib
    GReplFLib _ flib -> return $ flibSources flib
  where
    exeSources :: Executable -> IO BuildSources
    exeSources exe@Executable{buildInfo = bnfo, modulePath = modPath} = do
      main <- findFileCwd verbosity mbWorkDir (tmpDir : hsSourceDirs bnfo) modPath
      let mainModName = fromMaybe ModuleName.main $ exeMainModuleName exe
          otherModNames = exeModules exe
          haskellMain = isHaskell (getSymbolicPath main)

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if haskellMain || pkgId == fakePackageId
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
                  , inputSourceFiles = [main]
                  , inputSourceModules = filter (/= mainModName) $ exeModules exe
                  }
            else
              return
                BuildSources
                  { cSourcesFiles = cSources bnfo
                  , cxxSourceFiles = cxxSources bnfo
                  , inputSourceFiles = [main]
                  , inputSourceModules = exeModules exe
                  }
        else
          let (csf, cxxsf)
                | isCxx (getSymbolicPath main) = (cSources bnfo, main : cxxSources bnfo)
                -- if main is not a Haskell source
                -- and main is not a C++ source
                -- then we assume that it is a C source
                | otherwise = (main : cSources bnfo, cxxSources bnfo)
           in return
                BuildSources
                  { cSourcesFiles = csf
                  , cxxSourceFiles = cxxsf
                  , inputSourceFiles = []
                  , inputSourceModules = exeModules exe
                  }

    flibSources :: ForeignLib -> BuildSources
    flibSources flib@ForeignLib{foreignLibBuildInfo = bnfo} =
      BuildSources
        { cSourcesFiles = cSources bnfo
        , cxxSourceFiles = cxxSources bnfo
        , inputSourceFiles = []
        , inputSourceModules = foreignLibModules flib
        }

    isCxx :: FilePath -> Bool
    isCxx fp = elem (takeExtension fp) [".cpp", ".cxx", ".c++"]

-- | FilePath has a Haskell extension: .hs or .lhs
isHaskell :: FilePath -> Bool
isHaskell fp = elem (takeExtension fp) [".hs", ".lhs"]

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
  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  let replFlags = case bm of
        GReplExe flags _ -> flags
        GReplFLib flags _ -> flags
        GBuildExe{} -> mempty
        GBuildFLib{} -> mempty
      comp = compiler lbi
      platform = hostPlatform lbi
      mbWorkDir = mbWorkDirLBI lbi
      runGhcProg = runGHC verbosity ghcjsProg comp platform mbWorkDir

  let (bnfo, threaded) = case bm of
        GBuildFLib _ -> popThreadedFlag (gbuildInfo bm)
        _ -> (gbuildInfo bm, False)

  -- the name that GHC really uses (e.g., with .exe on Windows for executables)
  let targetName = gbuildTargetName lbi bm
      targetDir = buildDir lbi </> makeRelativePathEx (gbuildName bm)
      tmpDir = targetDir </> makeRelativePathEx (gbuildName bm ++ "-tmp")

      -- See Note [Symbolic paths] in Distribution.Utils.Path
      i = interpretSymbolicPath mbWorkDir

  createDirectoryIfMissingVerbose verbosity True $ i targetDir
  createDirectoryIfMissingVerbose verbosity True $ i tmpDir

  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = exeCoverage lbi
      hpcdir way
        | gbuildIsRepl bm = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir (tmpDir </> coerceSymbolicPath extraCompilationArtifacts) way
        | otherwise = mempty

  rpaths <- getRPaths lbi clbi
  buildSources <- gbuildSources verbosity mbWorkDir (package pkg_descr) (specVersion pkg_descr) tmpDir bm

  let cSrcs = cSourcesFiles buildSources
      cxxSrcs = cxxSourceFiles buildSources
      inputFiles = inputSourceFiles buildSources
      inputModules = inputSourceModules buildSources
      isGhcDynamic = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      cObjs = map ((`replaceExtensionSymbolicPath` objExtension)) cSrcs
      cxxObjs = map ((`replaceExtensionSymbolicPath` objExtension)) cxxSrcs
      needDynamic = gbuildNeedDynamic lbi bm
      needProfiling = withProfExe lbi

      -- build executables
      buildRunner = case clbi of
        LibComponentLocalBuildInfo{} -> False
        FLibComponentLocalBuildInfo{} -> False
        ExeComponentLocalBuildInfo{} -> True
        TestComponentLocalBuildInfo{} -> True
        BenchComponentLocalBuildInfo{} -> True
      baseOpts =
        (componentGhcOptions verbosity lbi bnfo clbi tmpDir)
          `mappend` mempty
            { ghcOptMode = toFlag GhcModeMake
            , ghcOptInputFiles =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter (isHaskell . getSymbolicPath) inputFiles
                    else inputFiles
            , ghcOptInputScripts =
                toNubListR $
                  if package pkg_descr == fakePackageId
                    then filter (not . isHaskell . getSymbolicPath) inputFiles
                    else []
            , ghcOptInputModules = toNubListR inputModules
            , -- for all executable components (exe/test/bench),
              -- GHCJS must be passed the "-build-runner" option
              ghcOptExtra =
                if buildRunner
                  then ["-build-runner"]
                  else mempty
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
            , ghcOptExtra = hcOptions GHC bnfo ++ hcSharedOptions GHC bnfo
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
          { ghcOptLinkOptions = PD.ldOptions bnfo
          , ghcOptLinkLibs = extraLibs bnfo
          , ghcOptLinkLibPath = toNubListR $ extraLibDirs bnfo
          , ghcOptLinkFrameworks =
              toNubListR $
                map getSymbolicPath $
                  PD.frameworks bnfo
          , ghcOptLinkFrameworkDirs =
              toNubListR $
                PD.extraFrameworkDirs bnfo
          , ghcOptInputFiles =
              toNubListR
                [makeSymbolicPath $ getSymbolicPath tmpDir </> getSymbolicPath x | x <- cObjs ++ cxxObjs]
          }
      dynLinkerOpts =
        mempty
          { ghcOptRPaths = rpaths
          }
      replOpts =
        baseOpts
          { ghcOptExtra =
              Internal.filterGhciFlags
                (ghcOptExtra baseOpts)
                <> replFlags
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

  -- build any C++ sources
  unless (null cxxSrcs) $ do
    info verbosity "Building C++ Sources..."
    sequence_
      [ do
        let baseCxxOpts =
              (Internal.sourcesGhcOptions verbosity lbi bnfo clbi odir filename)
                { -- C++ compiler options: GHC >= 8.10 requires -optcxx, older requires -optc
                  -- we want to be able to support cxx-options and cc-options separately
                  -- https://gitlab.haskell.org/ghc/ghc/-/issues/16477
                  -- see example in cabal-testsuite/PackageTests/FFI/ForeignOptsCxx
                  ghcOptCcOptions =
                    Internal.separateGhcOptions
                      (mkVersion [8, 10])
                      (compiler lbi)
                      (Internal.defaultGhcOptCcOptions lbi bnfo)
                , -- there are problems with linking with versions below 9.4,
                  -- that's why we need this replacement for linkGhcOptions
                  -- see example in cabal-testsuite/PackageTests/ShowBuildInfo/Complex
                  ghcOptCcProgram = Internal.defaultGhcOptCcProgram lbi
                }
            vanillaCxxOpts =
              if isGhcDynamic
                then -- Dynamic GHC requires C++ sources to be built
                -- with -fPIC for REPL to work. See #2207.
                  baseCxxOpts{ghcOptFPic = toFlag True}
                else baseCxxOpts
            profCxxOpts =
              vanillaCxxOpts
                `mappend` mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedCxxOpts =
              vanillaCxxOpts
                `mappend` mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            opts
              | needProfiling = profCxxOpts
              | needDynamic = sharedCxxOpts
              | otherwise = vanillaCxxOpts
            -- TODO: Placing all Haskell, C, & C++ objects in a single directory
            --       Has the potential for file collisions. In general we would
            --       consider this a user error. However, we should strive to
            --       add a warning if this occurs.
            odir = fromFlag (ghcOptObjDir opts)
        createDirectoryIfMissingVerbose verbosity True (i odir)
        needsRecomp <- checkNeedsRecompilation mbWorkDir filename opts
        when needsRecomp $
          runGhcProg opts
      | filename <- cxxSrcs
      ]

  -- build any C sources
  unless (null cSrcs) $ do
    info verbosity "Building C Sources..."
    sequence_
      [ do
        let baseCcOpts =
              (Internal.sourcesGhcOptions verbosity lbi bnfo clbi tmpDir filename)
                { -- C++ compiler options: GHC >= 8.10 requires -optcxx, older requires -optc
                  -- we want to be able to support cxx-options and cc-options separately
                  -- https://gitlab.haskell.org/ghc/ghc/-/issues/16477
                  -- see example in cabal-testsuite/PackageTests/FFI/ForeignOptsC
                  ghcOptCxxOptions =
                    Internal.separateGhcOptions
                      (mkVersion [8, 10])
                      (compiler lbi)
                      (Internal.defaultGhcOptCxxOptions lbi bnfo)
                , -- there are problems with linking with versions below 9.4,
                  -- that's why we need this replacement for linkGhcOptions
                  -- see example in cabal-testsuite/PackageTests/ShowBuildInfo/Complex
                  ghcOptCcProgram = Internal.defaultGhcOptCcProgram lbi
                }
            vanillaCcOpts =
              if isGhcDynamic
                then -- Dynamic GHC requires C sources to be built
                -- with -fPIC for REPL to work. See #2207.
                  baseCcOpts{ghcOptFPic = toFlag True}
                else baseCcOpts
            profCcOpts =
              vanillaCcOpts
                `mappend` mempty
                  { ghcOptProfilingMode = toFlag True
                  }
            sharedCcOpts =
              vanillaCcOpts
                `mappend` mempty
                  { ghcOptFPic = toFlag True
                  , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                  }
            opts
              | needProfiling = profCcOpts
              | needDynamic = sharedCcOpts
              | otherwise = vanillaCcOpts
            odir = fromFlag (ghcOptObjDir opts)
        createDirectoryIfMissingVerbose verbosity True (i odir)
        needsRecomp <- checkNeedsRecompilation mbWorkDir filename opts
        when needsRecomp $
          runGhcProg opts
      | filename <- cSrcs
      ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  case bm of
    GReplExe _ _ -> runGhcProg replOpts
    GReplFLib _ _ -> runGhcProg replOpts
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
      let target = targetDir </> makeRelativePathEx targetName
      when (compilerVersion comp < mkVersion [7, 7]) $ do
        let targetPath = i target
        e <- doesFileExist targetPath
        when e (removeFile targetPath)
      runGhcProg linkOpts{ghcOptOutputFile = toFlag target}
    GBuildFLib flib -> do
      let rtsInfo = extractRtsInfo lbi
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
          linkOpts = case foreignLibType flib of
            ForeignLibNativeShared ->
              commonOpts
                `mappend` linkerOpts
                `mappend` dynLinkerOpts
                `mappend` mempty
                  { ghcOptLinkNoHsMain = toFlag True
                  , ghcOptShared = toFlag True
                  , ghcOptLinkLibs = rtsOptLinkLibs
                  , ghcOptLinkLibPath = toNubListR $ map makeSymbolicPath $ rtsLibPaths rtsInfo
                  , ghcOptFPic = toFlag True
                  , ghcOptLinkModDefFiles = toNubListR $ fmap getSymbolicPath $ gbuildModDefFiles bm
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
          buildFile = targetDir </> makeRelativePathEx buildName
      runGhcProg linkOpts{ghcOptOutputFile = toFlag buildFile}
      renameFile (i buildFile) (i targetDir </> targetName)

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
  case PackageIndex.lookupPackageName (installedPkgs lbi) (mkPackageName "rts") of
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

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg File
  -> GhcOptions
  -> IO Bool
checkNeedsRecompilation mbWorkDir filename opts =
  i filename `moreRecentFile` oname
  where
    oname = getObjectFileName mbWorkDir filename opts
    i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path

-- | Finds the object file name of the given source file
getObjectFileName
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg File
  -> GhcOptions
  -> FilePath
getObjectFileName mbWorkDir filename opts = oname
  where
    i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path
    odir = i $ fromFlag (ghcOptObjDir opts)
    oext = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
    -- NB: the filepath might be absolute, e.g. if it is the path to
    -- an autogenerated .hs file.
    oname = odir </> replaceExtension (getSymbolicPath filename) oext

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths
  :: LocalBuildInfo
  -> ComponentLocalBuildInfo
  -- ^ Component we are building
  -> IO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
  libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
  let hostPref = case hostOS of
        OSX -> "@loader_path"
        _ -> "$ORIGIN"
      relPath p = if isRelative p then hostPref </> p else p
      rpaths = toNubListR (map relPath libraryPaths)
  return rpaths
  where
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

getRPaths _ _ = return mempty

-- | Remove the "-threaded" flag when building a foreign library, as it has no
--   effect when used with "-shared". Returns the updated 'BuildInfo', along
--   with whether or not the flag was present, so we can use it to link against
--   the appropriate RTS on our own.
popThreadedFlag :: BuildInfo -> (BuildInfo, Bool)
popThreadedFlag bi =
  ( bi{options = filterHcOptions (/= "-threaded") (options bi)}
  , hasThreaded (options bi)
  )
  where
    filterHcOptions
      :: (String -> Bool)
      -> PerCompilerFlavor [String]
      -> PerCompilerFlavor [String]
    filterHcOptions p (PerCompilerFlavor ghc ghcjs) =
      PerCompilerFlavor (filter p ghc) ghcjs

    hasThreaded :: PerCompilerFlavor [String] -> Bool
    hasThreaded (PerCompilerFlavor ghc _) = elem "-threaded" ghc

-- | Extracts a String representing a hash of the ABI of a built
-- library.  It can fail if the library has not yet been built.
libAbiHash
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  let
    libBi = libBuildInfo lib
    comp = compiler lbi
    platform = hostPlatform lbi
    mbWorkDir = mbWorkDirLBI lbi
    vanillaArgs =
      (componentGhcOptions verbosity lbi libBi clbi (componentBuildDir lbi clbi))
        `mappend` mempty
          { ghcOptMode = toFlag GhcModeAbiHash
          , ghcOptInputModules = toNubListR $ exposedModules lib
          }
    sharedArgs =
      vanillaArgs
        `mappend` mempty
          { ghcOptDynLinkMode = toFlag GhcDynamicOnly
          , ghcOptFPic = toFlag True
          , ghcOptHiSuffix = toFlag "js_dyn_hi"
          , ghcOptObjSuffix = toFlag "js_dyn_o"
          , ghcOptExtra = hcOptions GHC libBi ++ hcSharedOptions GHC libBi
          }
    profArgs =
      vanillaArgs
        `mappend` mempty
          { ghcOptProfilingMode = toFlag True
          , ghcOptProfilingAuto =
              Internal.profDetailLevelFlag
                True
                (withProfLibDetail lbi)
          , ghcOptHiSuffix = toFlag "js_p_hi"
          , ghcOptObjSuffix = toFlag "js_p_o"
          , ghcOptExtra = hcProfOptions GHC libBi
          }
    ghcArgs
      | withVanillaLib lbi = vanillaArgs
      | withSharedLib lbi = sharedArgs
      | withProfLib lbi = profArgs
      | otherwise = error "libAbiHash: Can't find an enabled library way"

  (ghcjsProg, _) <- requireProgram verbosity ghcjsProgram (withPrograms lbi)
  hash <-
    getProgramInvocationOutput
      verbosity
      =<< ghcInvocation verbosity ghcjsProg comp platform mbWorkDir ghcArgs
  return (takeWhile (not . isSpace) hash)

componentGhcOptions
  :: Verbosity
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir build)
  -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let opts = Internal.componentGhcOptions verbosity lbi bi clbi odir
   in opts
        { ghcOptExtra = ghcOptExtra opts `mappend` hcOptions GHCJS bi
        }

-- -----------------------------------------------------------------------------
-- Installing

-- | Install executables for GHCJS.
installExe
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ Where to copy the files to
  -> FilePath
  -- ^ Build location
  -> (FilePath, FilePath)
  -- ^ Executable (prefix,suffix)
  -> PackageDescription
  -> Executable
  -> IO ()
installExe
  verbosity
  lbi
  binDir
  buildPref
  (progprefix, progsuffix)
  _pkg
  exe = do
    createDirectoryIfMissingVerbose verbosity True binDir
    let exeName' = unUnqualComponentName $ exeName exe
        exeFileName = exeName'
        fixedExeBaseName = progprefix ++ exeName' ++ progsuffix
        installBinary dest = do
          runDbProgramCwd verbosity (mbWorkDirLBI lbi) ghcjsProgram (withPrograms lbi) $
            [ "--install-executable"
            , buildPref </> exeName' </> exeFileName
            , "-o"
            , dest
            ]
              ++ case (stripExes lbi, lookupProgram stripProgram $ withPrograms lbi) of
                (True, Just strip) -> ["-strip-program", programPath strip]
                _ -> []
    installBinary (binDir </> fixedExeBaseName)

-- | Install foreign library for GHC.
installFLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ Build location
  -> PackageDescription
  -> ForeignLib
  -> IO ()
installFLib verbosity lbi targetDir builtDir _pkg flib =
  install
    (foreignLibIsShared flib)
    builtDir
    targetDir
    (flibTargetName lbi flib)
  where
    install _isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True targetDir
      installOrdinaryFile verbosity src dst

-- | Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ install location for dynamic libraries
  -> FilePath
  -- ^ Build location
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir _builtDir _pkg lib clbi = do
  whenVanilla $ copyModuleFiles $ Suffix "js_hi"
  whenProf $ copyModuleFiles $ Suffix "js_p_hi"
  whenShared $ copyModuleFiles $ Suffix "js_dyn_hi"

  -- whenVanilla $ installOrdinary builtDir targetDir $ toJSLibName vanillaLibName
  -- whenProf    $ installOrdinary builtDir targetDir $ toJSLibName profileLibName
  -- whenShared  $ installShared   builtDir dynlibTargetDir $ toJSLibName sharedLibName
  -- fixme do these make the correct lib names?
  whenHasCode $ do
    whenVanilla $ do
      sequence_
        [ installOrdinary builtDir' targetDir (toJSLibName $ mkGenericStaticLibName (l ++ f))
        | l <- getHSLibraryName (componentUnitId clbi) : (extraBundledLibs (libBuildInfo lib))
        , f <- "" : extraLibFlavours (libBuildInfo lib)
        ]
    -- whenGHCi $ installOrdinary builtDir targetDir (toJSLibName ghciLibName)
    whenProf $ do
      installOrdinary builtDir' targetDir (toJSLibName profileLibName)
    -- whenGHCi $ installOrdinary builtDir targetDir (toJSLibName ghciProfLibName)
    whenShared $
      sequence_
        [ installShared
          builtDir'
          dynlibTargetDir
          (toJSLibName $ mkGenericSharedLibName platform compiler_id (l ++ f))
        | l <- getHSLibraryName uid : extraBundledLibs (libBuildInfo lib)
        , f <- "" : extraDynLibFlavours (libBuildInfo lib)
        ]
  where
    i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path
    builtDir' = componentBuildDir lbi clbi
    mbWorkDir = mbWorkDirLBI lbi

    install isShared isJS srcDir dstDir name = do
      let src = i $ srcDir </> makeRelativePathEx name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True dstDir

      if isShared
        then installExecutableFile verbosity src dst
        else installOrdinaryFile verbosity src dst

      when (stripLibs lbi && not isJS) $
        Strip.stripLib
          verbosity
          (hostPlatform lbi)
          (withPrograms lbi)
          dst

    installOrdinary = install False True
    installShared = install True True

    copyModuleFiles ext = do
      files <- findModuleFilesCwd verbosity mbWorkDir [builtDir'] [ext] (allLibModules lib clbi)
      let files' = map (i *** getSymbolicPath) files
      installOrdinaryFiles verbosity targetDir files'

    compiler_id = compilerId (compiler lbi)
    platform = hostPlatform lbi
    uid = componentUnitId clbi
    -- vanillaLibName = mkLibName              uid
    profileLibName = mkProfLibName uid
    -- sharedLibName  = (mkSharedLibName (hostPlatform lbi) compiler_id)  uid

    hasLib =
      not $
        null (allLibModules lib clbi)
          && null (cSources (libBuildInfo lib))
          && null (cxxSources (libBuildInfo lib))
          && null (jsSources (libBuildInfo lib))
    has_code = not (componentIsIndefinite clbi)
    whenHasCode = when has_code
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf = when (hasLib && withProfLib lbi && has_code)
    -- whenGHCi    = when (hasLib && withGHCiLib    lbi && has_code)
    whenShared = when (hasLib && withSharedLib lbi && has_code)

adjustExts :: String -> String -> GhcOptions -> GhcOptions
adjustExts hiSuf objSuf opts =
  opts
    `mappend` mempty
      { ghcOptHiSuffix = toFlag hiSuf
      , ghcOptObjSuffix = toFlag objSuf
      }

isDynamic :: Compiler -> Bool
isDynamic = Internal.ghcLookupProperty "GHC Dynamic"

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

withExt :: FilePath -> String -> FilePath
withExt fp ext = fp <.> if takeExtension fp /= ('.' : ext) then ext else ""

findGhcjsGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
findGhcjsGhcVersion verbosity pgm =
  findProgramVersion "--numeric-ghc-version" id verbosity pgm

findGhcjsPkgGhcjsVersion :: Verbosity -> FilePath -> IO (Maybe Version)
findGhcjsPkgGhcjsVersion verbosity pgm =
  findProgramVersion "--numeric-ghcjs-version" id verbosity pgm

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramDb -> HcPkg.HcPkgInfo
hcPkgInfo progdb =
  HcPkg.HcPkgInfo
    { HcPkg.hcPkgProgram = ghcjsPkgProg
    , HcPkg.noPkgDbStack = False
    , HcPkg.noVerboseFlag = False
    , HcPkg.flagPackageConf = False
    , HcPkg.supportsDirDbs = True
    , HcPkg.requiresDirDbs = ver >= v7_10
    , HcPkg.nativeMultiInstance = ver >= v7_10
    , HcPkg.recacheMultiInstance = True
    , HcPkg.suppressFilesCheck = True
    }
  where
    v7_10 = mkVersion [7, 10]
    ghcjsPkgProg = fromMaybe (error "GHCJS.hcPkgInfo no ghcjs program") $ lookupProgram ghcjsPkgProgram progdb
    ver = fromMaybe (error "GHCJS.hcPkgInfo no ghcjs version") $ programVersion ghcjsPkgProg

registerPackage
  :: Verbosity
  -> ProgramDb
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackS from
  -> InstalledPackageInfo
  -> HcPkg.RegisterOptions
  -> IO ()
registerPackage verbosity progdb mbWorkDir packageDbs installedPkgInfo registerOptions =
  HcPkg.register
    (hcPkgInfo progdb)
    verbosity
    mbWorkDir
    packageDbs
    installedPkgInfo
    registerOptions

pkgRoot :: Verbosity -> LocalBuildInfo -> PackageDB -> IO FilePath
pkgRoot verbosity lbi = pkgRoot'
  where
    pkgRoot' GlobalPackageDB =
      let ghcjsProg = fromMaybe (error "GHCJS.pkgRoot: no ghcjs program") $ lookupProgram ghcjsProgram (withPrograms lbi)
       in fmap takeDirectory (getGlobalPackageDB verbosity ghcjsProg)
    pkgRoot' UserPackageDB = do
      appDir <- getAppUserDataDirectory "ghcjs"
      -- fixme correct this version
      let ver = compilerVersion (compiler lbi)
          subdir =
            System.Info.arch
              ++ '-'
              : System.Info.os
              ++ '-'
              : prettyShow ver
          rootDir = appDir </> subdir
      -- We must create the root directory for the user package database if it
      -- does not yet exists. Otherwise '${pkgroot}' will resolve to a
      -- directory at the time of 'ghc-pkg register', and registration will
      -- fail.
      createDirectoryIfMissing True rootDir
      return rootDir
    pkgRoot' (SpecificPackageDB fp) =
      return $
        takeDirectory $
          interpretSymbolicPathLBI lbi fp

-- | Get the JavaScript file name and command and arguments to run a
--   program compiled by GHCJS
--   the exe should be the base program name without exe extension
runCmd
  :: ProgramDb
  -> FilePath
  -> (FilePath, FilePath, [String])
runCmd progdb exe =
  ( script
  , programPath ghcjsProg
  , programDefaultArgs ghcjsProg ++ programOverrideArgs ghcjsProg ++ ["--run"]
  )
  where
    script = exe <.> "jsexe" </> "all" <.> "js"
    ghcjsProg = fromMaybe (error "GHCJS.runCmd: no ghcjs program") $ lookupProgram ghcjsProgram progdb
