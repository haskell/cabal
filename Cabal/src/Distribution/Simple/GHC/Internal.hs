{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.GHC.Internal
-- Copyright   :  Isaac Jones 2003-2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains functions shared by GHC (Distribution.Simple.GHC)
-- and GHC-derived compilers.
module Distribution.Simple.GHC.Internal
  ( configureToolchain
  , getLanguages
  , getExtensions
  , targetPlatform
  , getGhcInfo
  , componentGhcOptions
  , sourcesGhcOptions
  , mkGHCiLibName
  , mkGHCiProfLibName
  , filterGhciFlags
  , ghcLookupProperty
  , getHaskellObjects
  , mkGhcOptPackages
  , substTopDir
  , checkPackageDbEnvVar
  , profDetailLevelFlag
  , defaultGhcOptCcOptions
  , defaultGhcOptCxxOptions
  , defaultGhcOptCcProgram
  , separateGhcOptions
  , linkGhcOptions

    -- * GHC platform and version strings
  , ghcArchString
  , ghcOsString
  , ghcPlatformAndVersionString

    -- * Constructing GHC environment files
  , GhcEnvironmentFileEntry (..)
  , writeGhcEnvironmentFile
  , simpleGhcEnvironmentFile
  , ghcEnvironmentFileName
  , renderGhcEnvironmentFile
  , renderGhcEnvironmentFileEntry
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Backpack
import Distribution.Compat.Stack
import Distribution.Lex
import qualified Distribution.ModuleName as ModuleName
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import Distribution.Simple.GHC.ImplInfo
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup.Common (extraCompilationArtifacts)
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.BuildInfo
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.GivenComponent
import qualified Distribution.Types.InstalledPackageInfo as IPI
import Distribution.Types.Library
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ModuleRenaming
import Distribution.Types.PackageName
import Distribution.Types.TargetInfo
import Distribution.Types.UnitId
import Distribution.Types.Version
import Distribution.Utils.NubList (NubListR, toNubListR)
import Distribution.Utils.Path
import Distribution.Verbosity
import Language.Haskell.Extension
import System.Directory (getDirectoryContents)
import System.Environment (getEnv)
import System.FilePath
  ( takeDirectory
  , takeExtension
  , takeFileName
  )
import System.IO (hClose, hPutStrLn)

targetPlatform :: [(String, String)] -> Maybe Platform
targetPlatform ghcInfo = platformFromTriple =<< lookup "Target platform" ghcInfo

-- | Adjust the way we find and configure gcc and ld
configureToolchain
  :: GhcImplInfo
  -> ConfiguredProgram
  -> Map String String
  -> ProgramDb
  -> ProgramDb
configureToolchain _implInfo ghcProg ghcInfo =
  addKnownProgram
    gccProgram
      { programFindLocation = findProg gccProgramName extraGccPath
      , programPostConf = configureGcc
      }
    . addKnownProgram
      gppProgram
        { programFindLocation = findProg gppProgramName extraGppPath
        , programPostConf = configureGpp
        }
    . addKnownProgram
      ldProgram
        { programFindLocation = findProg ldProgramName extraLdPath
        , programPostConf = \v cp ->
            -- Call any existing configuration first and then add any new configuration
            configureLd v =<< programPostConf ldProgram v cp
        }
    . addKnownProgram
      arProgram
        { programFindLocation = findProg arProgramName extraArPath
        }
    . addKnownProgram
      stripProgram
        { programFindLocation = findProg stripProgramName extraStripPath
        }
  where
    compilerDir, base_dir, mingwBinDir :: FilePath
    compilerDir = takeDirectory (programPath ghcProg)
    base_dir = takeDirectory compilerDir
    mingwBinDir = base_dir </> "mingw" </> "bin"
    isWindows = case buildOS of Windows -> True; _ -> False
    binPrefix = ""

    maybeName :: Program -> Maybe FilePath -> String
    maybeName prog = maybe (programName prog) (dropExeExtension . takeFileName)

    gccProgramName = maybeName gccProgram mbGccLocation
    gppProgramName = maybeName gppProgram mbGppLocation
    ldProgramName = maybeName ldProgram mbLdLocation
    arProgramName = maybeName arProgram mbArLocation
    stripProgramName = maybeName stripProgram mbStripLocation

    mkExtraPath :: Maybe FilePath -> FilePath -> [FilePath]
    mkExtraPath mbPath mingwPath
      | isWindows = mbDir ++ [mingwPath]
      | otherwise = mbDir
      where
        mbDir = maybeToList . fmap takeDirectory $ mbPath

    extraGccPath = mkExtraPath mbGccLocation windowsExtraGccDir
    extraGppPath = mkExtraPath mbGppLocation windowsExtraGppDir
    extraLdPath = mkExtraPath mbLdLocation windowsExtraLdDir
    extraArPath = mkExtraPath mbArLocation windowsExtraArDir
    extraStripPath = mkExtraPath mbStripLocation windowsExtraStripDir

    -- on Windows finding and configuring ghc's gcc & binutils is a bit special
    ( windowsExtraGccDir
      , windowsExtraGppDir
      , windowsExtraLdDir
      , windowsExtraArDir
      , windowsExtraStripDir
      ) =
        let b = mingwBinDir </> binPrefix
         in (b, b, b, b, b)

    findProg
      :: String
      -> [FilePath]
      -> Verbosity
      -> ProgramSearchPath
      -> IO (Maybe (FilePath, [FilePath]))
    findProg progName extraPath v searchpath =
      findProgramOnSearchPath v searchpath' progName
      where
        searchpath' = (map ProgramSearchPathDir extraPath) ++ searchpath

    -- Read tool locations from the 'ghc --info' output. Useful when
    -- cross-compiling.
    mbGccLocation = Map.lookup "C compiler command" ghcInfo
    mbGppLocation = Map.lookup "C++ compiler command" ghcInfo
    mbLdLocation = Map.lookup "ld command" ghcInfo
    mbArLocation = Map.lookup "ar command" ghcInfo
    mbStripLocation = Map.lookup "strip command" ghcInfo

    ccFlags = getFlags "C compiler flags"
    cxxFlags = getFlags "C++ compiler flags"
    -- GHC 7.8 renamed "Gcc Linker flags" to "C compiler link flags"
    -- and "Ld Linker flags" to "ld flags" (GHC #4862).
    gccLinkerFlags = getFlags "Gcc Linker flags" ++ getFlags "C compiler link flags"
    ldLinkerFlags = getFlags "Ld Linker flags" ++ getFlags "ld flags"

    -- It appears that GHC 7.6 and earlier encode the tokenized flags as a
    -- [String] in these settings whereas later versions just encode the flags as
    -- String.
    --
    -- We first try to parse as a [String] and if this fails then tokenize the
    -- flags ourself.
    getFlags :: String -> [String]
    getFlags key =
      case Map.lookup key ghcInfo of
        Nothing -> []
        Just flags
          | (flags', "") : _ <- reads flags -> flags'
          | otherwise -> tokenizeQuotedWords flags

    configureGcc :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureGcc _v gccProg = do
      return
        gccProg
          { programDefaultArgs =
              programDefaultArgs gccProg
                ++ ccFlags
                ++ gccLinkerFlags
          }

    configureGpp :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureGpp _v gppProg = do
      return
        gppProg
          { programDefaultArgs =
              programDefaultArgs gppProg
                ++ cxxFlags
          }

    configureLd :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureLd v ldProg = do
      ldProg' <- configureLd' v ldProg
      return
        ldProg'
          { programDefaultArgs = programDefaultArgs ldProg' ++ ldLinkerFlags
          }

    -- we need to find out if ld supports the -x flag
    configureLd' :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
    configureLd' verbosity ldProg = do
      ldx <- withTempFile ".c" $ \testcfile testchnd ->
        withTempFile ".o" $ \testofile testohnd -> do
          hPutStrLn testchnd "int foo() { return 0; }"
          hClose testchnd
          hClose testohnd
          runProgram
            verbosity
            ghcProg
            [ "-hide-all-packages"
            , "-c"
            , testcfile
            , "-o"
            , testofile
            ]
          withTempFile ".o" $ \testofile' testohnd' ->
            do
              hClose testohnd'
              _ <-
                getProgramOutput
                  verbosity
                  ldProg
                  ["-x", "-r", testofile, "-o", testofile']
              return True
              `catchIO` (\_ -> return False)
              `catchExit` (\_ -> return False)
      if ldx
        then return ldProg{programDefaultArgs = ["-x"]}
        else return ldProg

getLanguages
  :: Verbosity
  -> GhcImplInfo
  -> ConfiguredProgram
  -> IO [(Language, String)]
getLanguages _ implInfo _
  -- TODO: should be using --supported-languages rather than hard coding
  | supportsGHC2024 implInfo =
      return
        [ (GHC2024, "-XGHC2024")
        , (GHC2021, "-XGHC2021")
        , (Haskell2010, "-XHaskell2010")
        , (Haskell98, "-XHaskell98")
        ]
  | supportsGHC2021 implInfo =
      return
        [ (GHC2021, "-XGHC2021")
        , (Haskell2010, "-XHaskell2010")
        , (Haskell98, "-XHaskell98")
        ]
  | supportsHaskell2010 implInfo =
      return
        [ (Haskell98, "-XHaskell98")
        , (Haskell2010, "-XHaskell2010")
        ]
  | otherwise = return [(Haskell98, "")]

getGhcInfo
  :: Verbosity
  -> GhcImplInfo
  -> ConfiguredProgram
  -> IO [(String, String)]
getGhcInfo verbosity _implInfo ghcProg = do
  xs <-
    getProgramOutput
      verbosity
      (suppressOverrideArgs ghcProg)
      ["--info"]
  case reads xs of
    [(i, ss)]
      | all isSpace ss ->
          return i
    _ ->
      dieWithException verbosity CantParseGHCOutput

getExtensions
  :: Verbosity
  -> GhcImplInfo
  -> ConfiguredProgram
  -> IO [(Extension, Maybe String)]
getExtensions verbosity implInfo ghcProg = do
  str <-
    getProgramOutput
      verbosity
      (suppressOverrideArgs ghcProg)
      ["--supported-languages"]
  let extStrs =
        if reportsNoExt implInfo
          then lines str
          else -- Older GHCs only gave us either Foo or NoFoo,
          -- so we have to work out the other one ourselves

            [ extStr''
            | extStr <- lines str
            , let extStr' = case extStr of
                    'N' : 'o' : xs -> xs
                    _ -> "No" ++ extStr
            , extStr'' <- [extStr, extStr']
            ]
  let extensions0 =
        [ (ext, Just $ "-X" ++ prettyShow ext)
        | Just ext <- map simpleParsec extStrs
        ]
      extensions1 =
        if alwaysNondecIndent implInfo
          then -- ghc-7.2 split NondecreasingIndentation off
          -- into a proper extension. Before that it
          -- was always on.
          -- Since it was not a proper extension, it could
          -- not be turned off, hence we omit a
          -- DisableExtension entry here.

            (EnableExtension NondecreasingIndentation, Nothing)
              : extensions0
          else extensions0
  return extensions1

includePaths
  :: LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg p
  -> NubListR (SymbolicPath Pkg (Dir Include))
includePaths lbi bi clbi odir =
  toNubListR $
    [ coerceSymbolicPath $ autogenComponentModulesDir lbi clbi
    , coerceSymbolicPath $ autogenPackageModulesDir lbi
    , coerceSymbolicPath odir
    ]
      -- includes relative to the package
      ++ includeDirs bi
      -- potential includes generated by `configure'
      -- in the build directory
      ++ [ buildDir lbi </> dir
         | dir <- mapMaybe (symbolicPathRelative_maybe . unsafeCoerceSymbolicPath) $ includeDirs bi
         ]

sourcesGhcOptions
  :: Verbosity
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir Artifacts)
  -> SymbolicPath Pkg File
  -> GhcOptions
sourcesGhcOptions verbosity lbi bi clbi odir filename =
  (componentGhcOptions verbosity lbi bi clbi odir)
    { ghcOptMode = toFlag GhcModeCompile
    , ghcOptInputFiles = toNubListR [filename]
    , ghcOptObjDir = toFlag odir
    , ghcOptPackages = toNubListR $ mkGhcOptPackages (promisedPkgs lbi) clbi
    }

optimizationCFlags :: LocalBuildInfo -> [String]
optimizationCFlags lbi =
  ( case withOptimization lbi of
      -- see --disable-optimization
      NoOptimisation -> []
      -- '*-options: -O[n]' is generally not needed. When building with
      -- optimisations Cabal automatically adds '-O2' for * code. Setting it
      -- yourself interferes with the --disable-optimization flag.
      -- see https://github.com/haskell/cabal/pull/8250
      NormalOptimisation -> ["-O2"]
      -- see --enable-optimization
      MaximumOptimisation -> ["-O2"]
  )
    ++ ( case withDebugInfo lbi of
          NoDebugInfo -> []
          MinimalDebugInfo -> ["-g1"]
          NormalDebugInfo -> ["-g"]
          MaximalDebugInfo -> ["-g3"]
       )

defaultGhcOptCcOptions :: LocalBuildInfo -> BuildInfo -> [String]
defaultGhcOptCcOptions lbi bi = optimizationCFlags lbi ++ ccOptions bi

defaultGhcOptCxxOptions :: LocalBuildInfo -> BuildInfo -> [String]
defaultGhcOptCxxOptions lbi bi = optimizationCFlags lbi ++ cxxOptions bi

defaultGhcOptCcProgram :: LocalBuildInfo -> Flag FilePath
defaultGhcOptCcProgram lbi =
  maybeToFlag $ programPath <$> lookupProgram gccProgram (withPrograms lbi)

-- Since the GHС is sensitive to what is given to it, we sometimes need to
-- be able to pass options only to new versions
-- We want to be able to support C++ and C separately in older ghc
-- See example in buildExtraSources "C++ Sources" or "C Sources"
separateGhcOptions :: Monoid a => Version -> Compiler -> a -> a
separateGhcOptions ver comp defaultOptions =
  case compilerCompatVersion GHC comp of
    Just v
      | v >= ver -> defaultOptions
      | otherwise -> mempty
    Nothing -> mempty

componentGhcOptions
  :: Verbosity
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir build)
  -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let implInfo = getImplInfo $ compiler lbi
   in (linkGhcOptions verbosity lbi bi clbi)
        { ghcOptSourcePath =
            toNubListR $
              (hsSourceDirs bi)
                ++ [coerceSymbolicPath odir]
                ++ [autogenComponentModulesDir lbi clbi]
                ++ [autogenPackageModulesDir lbi]
        , ghcOptCppIncludePath = includePaths lbi bi clbi odir
        , ghcOptObjDir = toFlag $ coerceSymbolicPath odir
        , ghcOptHiDir = toFlag $ coerceSymbolicPath odir
        , ghcOptHieDir = bool NoFlag (toFlag $ coerceSymbolicPath odir </> (extraCompilationArtifacts </> makeRelativePathEx "hie")) $ flagHie implInfo
        , ghcOptStubDir = toFlag $ coerceSymbolicPath odir
        , ghcOptOutputDir = toFlag $ coerceSymbolicPath odir
        }

linkGhcOptions
  :: Verbosity
  -> LocalBuildInfo
  -> BuildInfo
  -> ComponentLocalBuildInfo
  -> GhcOptions
linkGhcOptions verbosity lbi bi clbi =
  let implInfo = getImplInfo $ compiler lbi
   in mempty
        { -- Respect -v0, but don't crank up verbosity on GHC if
          -- Cabal verbosity is requested. For that, use --ghc-option=-v instead!
          ghcOptVerbosity = toFlag (min verbosity normal)
        , ghcOptCcOptions = defaultGhcOptCcOptions lbi bi
        , ghcOptCxxOptions = defaultGhcOptCxxOptions lbi bi
        , ghcOptAsmOptions = optimizationCFlags lbi ++ asmOptions bi
        , ghcOptLinkOptions = ldOptions bi
        , ghcOptCppOptions = cppOptions bi
        , ghcOptJSppOptions = jsppOptions bi
        , ghcOptExtra = hcOptions GHC bi <> cmmOptions bi
        , ghcOptCabal = toFlag True
        , ghcOptCcProgram =
            separateGhcOptions
              (mkVersion [9, 4])
              (compiler lbi)
              (defaultGhcOptCcProgram lbi)
        , ghcOptThisUnitId = case clbi of
            LibComponentLocalBuildInfo{componentCompatPackageKey = pk} ->
              toFlag pk
            _ | not (unitIdForExes implInfo) -> mempty
            ExeComponentLocalBuildInfo{componentUnitId = uid} ->
              toFlag (unUnitId uid)
            TestComponentLocalBuildInfo{componentUnitId = uid} ->
              toFlag (unUnitId uid)
            BenchComponentLocalBuildInfo{componentUnitId = uid} ->
              toFlag (unUnitId uid)
            FLibComponentLocalBuildInfo{componentUnitId = uid} ->
              toFlag (unUnitId uid)
        , ghcOptThisComponentId = case clbi of
            LibComponentLocalBuildInfo
              { componentComponentId = cid
              , componentInstantiatedWith = insts
              } ->
                if null insts
                  then mempty
                  else toFlag cid
            _ -> mempty
        , ghcOptInstantiatedWith = case clbi of
            LibComponentLocalBuildInfo{componentInstantiatedWith = insts} ->
              insts
            _ -> []
        , ghcOptNoCode = toFlag $ componentIsIndefinite clbi
        , ghcOptHideAllPackages = toFlag True
        , ghcOptWarnMissingHomeModules = toFlag $ flagWarnMissingHomeModules implInfo
        , ghcOptPackageDBs = withPackageDB lbi
        , ghcOptPackages = toNubListR $ mkGhcOptPackages mempty clbi
        , ghcOptSplitSections = toFlag (splitSections lbi)
        , ghcOptSplitObjs = toFlag (splitObjs lbi)
        , ghcOptSourcePathClear = toFlag True
        , ghcOptCppIncludes =
            toNubListR $
              [coerceSymbolicPath (autogenComponentModulesDir lbi clbi </> makeRelativePathEx cppHeaderName)]
        , ghcOptFfiIncludes = toNubListR $ map getSymbolicPath $ includes bi
        , ghcOptOptimisation = toGhcOptimisation (withOptimization lbi)
        , ghcOptDebugInfo = toFlag (withDebugInfo lbi)
        , ghcOptExtraPath = toNubListR exe_paths
        , ghcOptLanguage = toFlag (fromMaybe Haskell98 (defaultLanguage bi))
        , -- Unsupported extensions have already been checked by configure
          ghcOptExtensions = toNubListR $ usedExtensions bi
        , ghcOptExtensionMap = Map.fromList . compilerExtensions $ (compiler lbi)
        }
  where
    exe_paths =
      [ componentBuildDir lbi (targetCLBI exe_tgt)
      | uid <- componentExeDeps clbi
      , -- TODO: Ugh, localPkgDescr
      Just exe_tgt <- [unitIdTarget' (localPkgDescr lbi) lbi uid]
      ]

toGhcOptimisation :: OptimisationLevel -> Flag GhcOptimisation
toGhcOptimisation NoOptimisation = mempty -- TODO perhaps override?
toGhcOptimisation NormalOptimisation = toFlag GhcNormalOptimisation
toGhcOptimisation MaximumOptimisation = toFlag GhcMaximumOptimisation

-- | Strip out flags that are not supported in ghci
filterGhciFlags :: [String] -> [String]
filterGhciFlags = filter supported
  where
    supported ('-' : 'O' : _) = False
    supported "-debug" = False
    supported "-threaded" = False
    supported "-ticky" = False
    supported "-eventlog" = False
    supported "-prof" = False
    supported "-unreg" = False
    supported _ = True

mkGHCiLibName :: UnitId -> String
mkGHCiLibName lib = getHSLibraryName lib <.> "o"

mkGHCiProfLibName :: UnitId -> String
mkGHCiProfLibName lib = getHSLibraryName lib <.> "p_o"

ghcLookupProperty :: String -> Compiler -> Bool
ghcLookupProperty prop comp =
  case Map.lookup prop (compilerProperties comp) of
    Just "YES" -> True
    _ -> False

-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects
  :: GhcImplInfo
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir Artifacts)
  -> String
  -> Bool
  -> IO [SymbolicPath Pkg File]
getHaskellObjects _implInfo lib lbi clbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
      let splitSuffix = "_" ++ wanted_obj_ext ++ "_split"
          dirs =
            [ pref </> makeRelativePathEx (ModuleName.toFilePath x ++ splitSuffix)
            | x <- allLibModules lib clbi
            ]
      objss <- traverse (getDirectoryContents . i) dirs
      let objs =
            [ dir </> makeRelativePathEx obj
            | (objs', dir) <- zip objss dirs
            , obj <- objs'
            , let obj_ext = takeExtension obj
            , '.' : wanted_obj_ext == obj_ext
            ]
      return objs
  | otherwise =
      return
        [ pref </> makeRelativePathEx (ModuleName.toFilePath x <.> wanted_obj_ext)
        | x <- allLibModules lib clbi
        ]
  where
    i = interpretSymbolicPathLBI lbi

-- | Create the required packaged arguments, but filtering out package arguments which
-- aren't yet built, but promised. This filtering is used when compiling C/Cxx/Asm files,
-- and is a hack to avoid passing bogus `-package` arguments to GHC. The assumption being that
-- in 99% of cases we will include the right `-package` so that the C file finds the right headers.
mkGhcOptPackages
  :: Map (PackageName, ComponentName) PromisedComponent
  -> ComponentLocalBuildInfo
  -> [(OpenUnitId, ModuleRenaming)]
mkGhcOptPackages promisedPkgsMap clbi =
  [ i | i@(uid, _) <- componentIncludes clbi, abstractUnitId uid `Set.notMember` promised_cids
  ]
  where
    -- Promised deps are going to be simple UnitIds
    promised_cids = Set.fromList (map (newSimpleUnitId . promisedComponentId) (Map.elems promisedPkgsMap))

substTopDir :: FilePath -> IPI.InstalledPackageInfo -> IPI.InstalledPackageInfo
substTopDir topDir ipo =
  ipo
    { IPI.importDirs = map f (IPI.importDirs ipo)
    , IPI.libraryDirs = map f (IPI.libraryDirs ipo)
    , IPI.libraryDirsStatic = map f (IPI.libraryDirsStatic ipo)
    , IPI.includeDirs = map f (IPI.includeDirs ipo)
    , IPI.frameworkDirs = map f (IPI.frameworkDirs ipo)
    , IPI.haddockInterfaces = map f (IPI.haddockInterfaces ipo)
    , IPI.haddockHTMLs = map f (IPI.haddockHTMLs ipo)
    }
  where
    f ('$' : 't' : 'o' : 'p' : 'd' : 'i' : 'r' : rest) = topDir ++ rest
    f x = x

-- Cabal does not use the environment variable GHC{,JS}_PACKAGE_PATH; let
-- users know that this is the case. See ticket #335. Simply ignoring it is
-- not a good idea, since then ghc and cabal are looking at different sets
-- of package DBs and chaos is likely to ensue.
--
-- An exception to this is when running cabal from within a `cabal exec`
-- environment. In this case, `cabal exec` will set the
-- CABAL_SANDBOX_PACKAGE_PATH to the same value that it set
-- GHC{,JS}_PACKAGE_PATH to. If that is the case it is OK to allow
-- GHC{,JS}_PACKAGE_PATH.
checkPackageDbEnvVar :: Verbosity -> String -> String -> IO ()
checkPackageDbEnvVar verbosity compilerName packagePathEnvVar = do
  mPP <- lookupEnv packagePathEnvVar
  when (isJust mPP) $ do
    mcsPP <- lookupEnv "CABAL_SANDBOX_PACKAGE_PATH"
    unless (mPP == mcsPP) abort
  where
    lookupEnv :: String -> IO (Maybe String)
    lookupEnv name =
      (Just `fmap` getEnv name)
        `catchIO` const (return Nothing)
    abort =
      dieWithException verbosity $ IncompatibleWithCabal compilerName packagePathEnvVar
    _ = callStack -- TODO: output stack when erroring

profDetailLevelFlag :: Bool -> ProfDetailLevel -> Flag GhcProfAuto
profDetailLevelFlag forLib mpl =
  case mpl of
    ProfDetailNone -> mempty
    ProfDetailDefault
      | forLib -> toFlag GhcProfAutoExported
      | otherwise -> toFlag GhcProfAutoToplevel
    ProfDetailExportedFunctions -> toFlag GhcProfAutoExported
    ProfDetailToplevelFunctions -> toFlag GhcProfAutoToplevel
    ProfDetailAllFunctions -> toFlag GhcProfAutoAll
    ProfDetailTopLate -> toFlag GhcProfLate
    ProfDetailOther _ -> mempty

-- -----------------------------------------------------------------------------
-- GHC platform and version strings

-- | GHC's rendering of its host or target 'Arch' as used in its platform
-- strings and certain file locations (such as user package db location).
ghcArchString :: Arch -> String
ghcArchString PPC = "powerpc"
ghcArchString PPC64 = "powerpc64"
ghcArchString PPC64LE = "powerpc64le"
ghcArchString other = prettyShow other

-- | GHC's rendering of its host or target 'OS' as used in its platform
-- strings and certain file locations (such as user package db location).
ghcOsString :: OS -> String
ghcOsString Windows = "mingw32"
ghcOsString OSX = "darwin"
ghcOsString Solaris = "solaris2"
ghcOsString Hurd = "gnu"
ghcOsString other = prettyShow other

-- | GHC's rendering of its platform and compiler version string as used in
-- certain file locations (such as user package db location).
-- For example @x86_64-linux-7.10.4@
ghcPlatformAndVersionString :: Platform -> Version -> String
ghcPlatformAndVersionString (Platform arch os) version =
  intercalate "-" [ghcArchString arch, ghcOsString os, prettyShow version]

-- -----------------------------------------------------------------------------
-- Constructing GHC environment files

-- | The kinds of entries we can stick in a @.ghc.environment@ file.
data GhcEnvironmentFileEntry fp
  = -- | @-- a comment@
    GhcEnvFileComment String
  | -- | @package-id foo-1.0-4fe301a...@
    GhcEnvFilePackageId UnitId
  | -- | @global-package-db@,
    --   @user-package-db@ or
    --   @package-db blah/package.conf.d/@
    GhcEnvFilePackageDb (PackageDBX fp)
  | -- | @clear-package-db@
    GhcEnvFileClearPackageDbStack
  deriving (Eq, Ord, Show)

-- | Make entries for a GHC environment file based on a 'PackageDBStack' and
-- a bunch of package (unit) ids.
--
-- If you need to do anything more complicated then either use this as a basis
-- and add more entries, or just make all the entries directly.
simpleGhcEnvironmentFile
  :: PackageDBStackX fp
  -> [UnitId]
  -> [GhcEnvironmentFileEntry fp]
simpleGhcEnvironmentFile packageDBs pkgids =
  GhcEnvFileClearPackageDbStack
    : map GhcEnvFilePackageDb packageDBs
    ++ map GhcEnvFilePackageId pkgids

-- | Write a @.ghc.environment-$arch-$os-$ver@ file in the given directory.
--
-- The 'Platform' and GHC 'Version' are needed as part of the file name.
--
-- Returns the name of the file written.
writeGhcEnvironmentFile
  :: FilePath
  -- ^ directory in which to put it
  -> Platform
  -- ^ the GHC target platform
  -> Version
  -- ^ the GHC version
  -> [GhcEnvironmentFileEntry FilePath]
  -- ^ the content
  -> IO FilePath
writeGhcEnvironmentFile directory platform ghcversion entries = do
  writeFileAtomic envfile . BS.pack . renderGhcEnvironmentFile $ entries
  return envfile
  where
    envfile = directory </> ghcEnvironmentFileName platform ghcversion

-- | The @.ghc.environment-$arch-$os-$ver@ file name
ghcEnvironmentFileName :: Platform -> Version -> FilePath
ghcEnvironmentFileName platform ghcversion =
  ".ghc.environment." ++ ghcPlatformAndVersionString platform ghcversion

-- | Render a bunch of GHC environment file entries
renderGhcEnvironmentFile :: [GhcEnvironmentFileEntry FilePath] -> String
renderGhcEnvironmentFile =
  unlines . map renderGhcEnvironmentFileEntry

-- | Render an individual GHC environment file entry
renderGhcEnvironmentFileEntry :: GhcEnvironmentFileEntry FilePath -> String
renderGhcEnvironmentFileEntry entry = case entry of
  GhcEnvFileComment comment -> format comment
    where
      format = intercalate "\n" . map ("--" <++>) . lines
      pref <++> "" = pref
      pref <++> str = pref ++ " " ++ str
  GhcEnvFilePackageId pkgid -> "package-id " ++ prettyShow pkgid
  GhcEnvFilePackageDb pkgdb ->
    case pkgdb of
      GlobalPackageDB -> "global-package-db"
      UserPackageDB -> "user-package-db"
      SpecificPackageDB dbfile -> "package-db " ++ dbfile
  GhcEnvFileClearPackageDbStack -> "clear-package-db"
