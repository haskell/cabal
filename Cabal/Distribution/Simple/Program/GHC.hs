module Distribution.Simple.Program.GHC (
    GhcOptions(..),
    GhcMode(..),
    GhcOptimisation(..),
    GhcDynLinkMode(..),
    GhcProfAuto(..),

    ghcInvocation,
    renderGhcOptions,

    runGHC,

  ) where

import Distribution.Compat.Semigroup as Semi
import Distribution.Simple.GHC.ImplInfo ( getImplInfo, GhcImplInfo(..) )
import Distribution.Package
import Distribution.PackageDescription hiding (Flag)
import Distribution.ModuleName
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.Setup    ( Flag(..), flagToMaybe, fromFlagOrDefault,
                                      flagToList )
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Run
import Distribution.Text
import Distribution.Verbosity
import Distribution.Utils.NubList   ( NubListR, fromNubListR )
import Language.Haskell.Extension   ( Language(..), Extension(..) )

import qualified Data.Map as M
import Data.List ( intercalate )

-- | A structured set of GHC options/flags
--
data GhcOptions = GhcOptions {

  -- | The major mode for the ghc invocation.
  ghcOptMode          :: Flag GhcMode,

  -- | Any extra options to pass directly to ghc. These go at the end and hence
  -- override other stuff.
  ghcOptExtra         :: NubListR String,

  -- | Extra default flags to pass directly to ghc. These go at the beginning
  -- and so can be overridden by other stuff.
  ghcOptExtraDefault  :: NubListR String,

  -----------------------
  -- Inputs and outputs

  -- | The main input files; could be .hs, .hi, .c, .o, depending on mode.
  ghcOptInputFiles    :: NubListR FilePath,

  -- | The names of input Haskell modules, mainly for @--make@ mode.
  ghcOptInputModules  :: NubListR ModuleName,

  -- | Location for output file; the @ghc -o@ flag.
  ghcOptOutputFile    :: Flag FilePath,

  -- | Location for dynamic output file in 'GhcStaticAndDynamic' mode;
  -- the @ghc -dyno@ flag.
  ghcOptOutputDynFile :: Flag FilePath,

  -- | Start with an empty search path for Haskell source files;
  -- the @ghc -i@ flag (@-i@ on it's own with no path argument).
  ghcOptSourcePathClear :: Flag Bool,

  -- | Search path for Haskell source files; the @ghc -i@ flag.
  ghcOptSourcePath    :: NubListR FilePath,

  -------------
  -- Packages

  -- | The package key the modules will belong to; the @ghc -this-package-key@
  -- flag.
  ghcOptComponentId   :: Flag ComponentId,

  -- | GHC package databases to use, the @ghc -package-conf@ flag.
  ghcOptPackageDBs    :: PackageDBStack,

  -- | The GHC packages to use. For compatability with old and new ghc, this
  -- requires both the short and long form of the package id;
  -- the @ghc -package@ or @ghc -package-id@ flags.
  ghcOptPackages      ::
    NubListR (ComponentId, PackageId, ModuleRenaming),

  -- | Start with a clean package set; the @ghc -hide-all-packages@ flag
  ghcOptHideAllPackages :: Flag Bool,

  -- | Don't automatically link in Haskell98 etc; the @ghc
  -- -no-auto-link-packages@ flag.
  ghcOptNoAutoLinkPackages :: Flag Bool,

  -- | What packages are implementing the signatures
  ghcOptSigOf :: [(ModuleName, (ComponentId, ModuleName))],

  -----------------
  -- Linker stuff

  -- | Names of libraries to link in; the @ghc -l@ flag.
  ghcOptLinkLibs      :: NubListR FilePath,

  -- | Search path for libraries to link in; the @ghc -L@ flag.
  ghcOptLinkLibPath  :: NubListR FilePath,

  -- | Options to pass through to the linker; the @ghc -optl@ flag.
  ghcOptLinkOptions   :: NubListR String,

  -- | OSX only: frameworks to link in; the @ghc -framework@ flag.
  ghcOptLinkFrameworks :: NubListR String,

  -- | Don't do the link step, useful in make mode; the @ghc -no-link@ flag.
  ghcOptNoLink :: Flag Bool,

  -- | Don't link in the normal RTS @main@ entry point; the @ghc -no-hs-main@
  -- flag.
  ghcOptLinkNoHsMain :: Flag Bool,

  --------------------
  -- C and CPP stuff

  -- | Options to pass through to the C compiler; the @ghc -optc@ flag.
  ghcOptCcOptions     :: NubListR String,

  -- | Options to pass through to CPP; the @ghc -optP@ flag.
  ghcOptCppOptions    :: NubListR String,

  -- | Search path for CPP includes like header files; the @ghc -I@ flag.
  ghcOptCppIncludePath :: NubListR FilePath,

  -- | Extra header files to include at CPP stage; the @ghc -optP-include@ flag.
  ghcOptCppIncludes    :: NubListR FilePath,

  -- | Extra header files to include for old-style FFI; the @ghc -#include@ flag.
  ghcOptFfiIncludes    :: NubListR FilePath,

  ----------------------------
  -- Language and extensions

  -- | The base language; the @ghc -XHaskell98@ or @-XHaskell2010@ flag.
  ghcOptLanguage      :: Flag Language,

  -- | The language extensions; the @ghc -X@ flag.
  ghcOptExtensions    :: NubListR Extension,

  -- | A GHC version-dependent mapping of extensions to flags. This must be
  -- set to be able to make use of the 'ghcOptExtensions'.
  ghcOptExtensionMap    :: M.Map Extension String,

  ----------------
  -- Compilation

  -- | What optimisation level to use; the @ghc -O@ flag.
  ghcOptOptimisation  :: Flag GhcOptimisation,

    -- | Emit debug info; the @ghc -g@ flag.
  ghcOptDebugInfo  :: Flag Bool,

  -- | Compile in profiling mode; the @ghc -prof@ flag.
  ghcOptProfilingMode :: Flag Bool,

  -- | Automatically add profiling cost centers; the @ghc -fprof-auto*@ flags.
  ghcOptProfilingAuto :: Flag GhcProfAuto,

  -- | Use the \"split object files\" feature; the @ghc -split-objs@ flag.
  ghcOptSplitObjs     :: Flag Bool,

  -- | Run N jobs simultaneously (if possible).
  ghcOptNumJobs       :: Flag (Maybe Int),

  -- | Enable coverage analysis; the @ghc -fhpc -hpcdir@ flags.
  ghcOptHPCDir        :: Flag FilePath,

  ----------------
  -- GHCi

  -- | Extra GHCi startup scripts; the @-ghci-script@ flag
  ghcOptGHCiScripts    :: NubListR FilePath,

  ------------------------
  -- Redirecting outputs

  ghcOptHiSuffix      :: Flag String,
  ghcOptObjSuffix     :: Flag String,
  ghcOptDynHiSuffix   :: Flag String,   -- ^ only in 'GhcStaticAndDynamic' mode
  ghcOptDynObjSuffix  :: Flag String,   -- ^ only in 'GhcStaticAndDynamic' mode
  ghcOptHiDir         :: Flag FilePath,
  ghcOptObjDir        :: Flag FilePath,
  ghcOptOutputDir     :: Flag FilePath,
  ghcOptStubDir       :: Flag FilePath,

  --------------------
  -- Dynamic linking

  ghcOptDynLinkMode   :: Flag GhcDynLinkMode,
  ghcOptShared        :: Flag Bool,
  ghcOptFPic          :: Flag Bool,
  ghcOptDylibName     :: Flag String,
  ghcOptRPaths        :: NubListR FilePath,

  ---------------
  -- Misc flags

  -- | Get GHC to be quiet or verbose with what it's doing; the @ghc -v@ flag.
  ghcOptVerbosity     :: Flag Verbosity,

  -- | Let GHC know that it is Cabal that's calling it.
  -- Modifies some of the GHC error messages.
  ghcOptCabal         :: Flag Bool

} deriving Show


data GhcMode = GhcModeCompile     -- ^ @ghc -c@
             | GhcModeLink        -- ^ @ghc@
             | GhcModeMake        -- ^ @ghc --make@
             | GhcModeInteractive -- ^ @ghci@ \/ @ghc --interactive@
             | GhcModeAbiHash     -- ^ @ghc --abi-hash@
--             | GhcModeDepAnalysis -- ^ @ghc -M@
--             | GhcModeEvaluate    -- ^ @ghc -e@
 deriving (Show, Eq)

data GhcOptimisation = GhcNoOptimisation             -- ^ @-O0@
                     | GhcNormalOptimisation         -- ^ @-O@
                     | GhcMaximumOptimisation        -- ^ @-O2@
                     | GhcSpecialOptimisation String -- ^ e.g. @-Odph@
 deriving (Show, Eq)

data GhcDynLinkMode = GhcStaticOnly       -- ^ @-static@
                    | GhcDynamicOnly      -- ^ @-dynamic@
                    | GhcStaticAndDynamic -- ^ @-static -dynamic-too@
 deriving (Show, Eq)

data GhcProfAuto = GhcProfAutoAll       -- ^ @-fprof-auto@
                 | GhcProfAutoToplevel  -- ^ @-fprof-auto-top@
                 | GhcProfAutoExported  -- ^ @-fprof-auto-exported@
 deriving (Show, Eq)

runGHC :: Verbosity -> ConfiguredProgram -> Compiler -> GhcOptions -> IO ()
runGHC verbosity ghcProg comp opts = do
  runProgramInvocation verbosity (ghcInvocation ghcProg comp opts)


ghcInvocation :: ConfiguredProgram -> Compiler -> GhcOptions -> ProgramInvocation
ghcInvocation prog comp opts =
    programInvocation prog (renderGhcOptions comp opts)

renderGhcOptions :: Compiler -> GhcOptions -> [String]
renderGhcOptions comp opts
  | compilerFlavor comp `notElem` [GHC, GHCJS] =
    error $ "Distribution.Simple.Program.GHC.renderGhcOptions: "
    ++ "compiler flavor must be 'GHC' or 'GHCJS'!"
  | otherwise =
  concat
  [ case flagToMaybe (ghcOptMode opts) of
       Nothing                 -> []
       Just GhcModeCompile     -> ["-c"]
       Just GhcModeLink        -> []
       Just GhcModeMake        -> ["--make"]
       Just GhcModeInteractive -> ["--interactive"]
       Just GhcModeAbiHash     -> ["--abi-hash"]
--     Just GhcModeDepAnalysis -> ["-M"]
--     Just GhcModeEvaluate    -> ["-e", expr]

  , flags ghcOptExtraDefault

  , [ "-no-link" | flagBool ghcOptNoLink ]

  ---------------
  -- Misc flags

  , maybe [] verbosityOpts (flagToMaybe (ghcOptVerbosity opts))

  , [ "-fbuilding-cabal-package" | flagBool ghcOptCabal
                                 , flagBuildingCabalPkg implInfo ]

  ----------------
  -- Compilation

  , case flagToMaybe (ghcOptOptimisation opts) of
      Nothing                         -> []
      Just GhcNoOptimisation          -> ["-O0"]
      Just GhcNormalOptimisation      -> ["-O"]
      Just GhcMaximumOptimisation     -> ["-O2"]
      Just (GhcSpecialOptimisation s) -> ["-O" ++ s] -- eg -Odph

  , [ "-g" | flagDebugInfo implInfo && flagBool ghcOptDebugInfo ]

  , [ "-prof" | flagBool ghcOptProfilingMode ]

  , case flagToMaybe (ghcOptProfilingAuto opts) of
      _ | not (flagBool ghcOptProfilingMode)
                                -> []
      Nothing                   -> []
      Just GhcProfAutoAll
        | flagProfAuto implInfo -> ["-fprof-auto"]
        | otherwise             -> ["-auto-all"] -- not the same, but close
      Just GhcProfAutoToplevel
        | flagProfAuto implInfo -> ["-fprof-auto-top"]
        | otherwise             -> ["-auto-all"]
      Just GhcProfAutoExported
        | flagProfAuto implInfo -> ["-fprof-auto-exported"]
        | otherwise             -> ["-auto"]

  , [ "-split-objs" | flagBool ghcOptSplitObjs ]

  , case flagToMaybe (ghcOptHPCDir opts) of
      Nothing -> []
      Just hpcdir -> ["-fhpc", "-hpcdir", hpcdir]

  , if parmakeSupported comp
    then case ghcOptNumJobs opts of
      NoFlag  -> []
      Flag n  -> ["-j" ++ maybe "" show n]
    else []

  --------------------
  -- Dynamic linking

  , [ "-shared"  | flagBool ghcOptShared  ]
  , case flagToMaybe (ghcOptDynLinkMode opts) of
      Nothing                  -> []
      Just GhcStaticOnly       -> ["-static"]
      Just GhcDynamicOnly      -> ["-dynamic"]
      Just GhcStaticAndDynamic -> ["-static", "-dynamic-too"]
  , [ "-fPIC"    | flagBool ghcOptFPic ]

  , concat [ ["-dylib-install-name", libname] | libname <- flag ghcOptDylibName ]

  ------------------------
  -- Redirecting outputs

  , concat [ ["-osuf",    suf] | suf <- flag ghcOptObjSuffix ]
  , concat [ ["-hisuf",   suf] | suf <- flag ghcOptHiSuffix  ]
  , concat [ ["-dynosuf", suf] | suf <- flag ghcOptDynObjSuffix ]
  , concat [ ["-dynhisuf",suf] | suf <- flag ghcOptDynHiSuffix  ]
  , concat [ ["-outputdir", dir] | dir <- flag ghcOptOutputDir
                                 , flagOutputDir implInfo ]
  , concat [ ["-odir",    dir] | dir <- flag ghcOptObjDir ]
  , concat [ ["-hidir",   dir] | dir <- flag ghcOptHiDir  ]
  , concat [ ["-stubdir", dir] | dir <- flag ghcOptStubDir
                               , flagStubdir implInfo ]

  -----------------------
  -- Source search path

  , [ "-i"        | flagBool ghcOptSourcePathClear ]
  , [ "-i" ++ dir | dir <- flags ghcOptSourcePath ]

  --------------------
  -- C and CPP stuff

  , [ "-I"    ++ dir | dir <- flags ghcOptCppIncludePath ]
  , [ "-optP" ++ opt | opt <- flags ghcOptCppOptions ]
  , concat [ [ "-optP-include", "-optP" ++ inc]
           | inc <- flags ghcOptCppIncludes ]
  , [ "-#include \"" ++ inc ++ "\""
    | inc <- flags ghcOptFfiIncludes, flagFfiIncludes implInfo ]
  , [ "-optc" ++ opt | opt <- flags ghcOptCcOptions ]

  -----------------
  -- Linker stuff

  , [ "-optl" ++ opt | opt <- flags ghcOptLinkOptions ]
  , ["-l" ++ lib     | lib <- flags ghcOptLinkLibs ]
  , ["-L" ++ dir     | dir <- flags ghcOptLinkLibPath ]
  , concat [ ["-framework", fmwk] | fmwk <- flags ghcOptLinkFrameworks ]
  , [ "-no-hs-main"  | flagBool ghcOptLinkNoHsMain ]
  , [ "-dynload deploy" | not (null (flags ghcOptRPaths)) ]
  , concat [ [ "-optl-Wl,-rpath," ++ dir]
           | dir <- flags ghcOptRPaths ]

  -------------
  -- Packages

  , concat [ [if packageKeySupported comp
                then "-this-package-key"
                else "-package-name", display pkgid]
             | pkgid <- flag ghcOptComponentId ]

  , [ "-hide-all-packages"     | flagBool ghcOptHideAllPackages ]
  , [ "-no-auto-link-packages" | flagBool ghcOptNoAutoLinkPackages ]

  , packageDbArgs implInfo (ghcOptPackageDBs opts)

  , if null (ghcOptSigOf opts)
        then []
        else "-sig-of"
             : intercalate "," (map (\(n,(p,m)) -> display n ++ " is "
                                                ++ display p ++ ":"
                                                ++ display m)
                                    (ghcOptSigOf opts))
             : []

  , concat $ if flagPackageId implInfo
      then let space "" = ""
               space xs = ' ' : xs
           in [ ["-package-id", display ipkgid ++ space (display rns)]
              | (ipkgid,_,rns) <- flags ghcOptPackages ]
      else [ ["-package",    display  pkgid]
           | (_,pkgid,_)  <- flags ghcOptPackages ]

  ----------------------------
  -- Language and extensions

  , if supportsHaskell2010 implInfo
    then [ "-X" ++ display lang | lang <- flag ghcOptLanguage ]
    else []

  , [ case M.lookup ext (ghcOptExtensionMap opts) of
        Just arg -> arg
        Nothing  -> error $ "Distribution.Simple.Program.GHC.renderGhcOptions: "
                          ++ display ext ++ " not present in ghcOptExtensionMap."
    | ext <- flags ghcOptExtensions ]

  ----------------
  -- GHCi

  , concat [ [ "-ghci-script", script ] | script <- flags  ghcOptGHCiScripts
                                        , flagGhciScript implInfo ]

  ---------------
  -- Inputs

  , [ display modu | modu <- flags ghcOptInputModules ]
  , flags ghcOptInputFiles

  , concat [ [ "-o",    out] | out <- flag ghcOptOutputFile ]
  , concat [ [ "-dyno", out] | out <- flag ghcOptOutputDynFile ]

  ---------------
  -- Extra

  , flags ghcOptExtra

  ]


  where
    implInfo     = getImplInfo comp
    flag     flg = flagToList (flg opts)
    flags    flg = fromNubListR . flg $ opts
    flagBool flg = fromFlagOrDefault False (flg opts)

verbosityOpts :: Verbosity -> [String]
verbosityOpts verbosity
  | verbosity >= deafening = ["-v"]
  | verbosity >= normal    = []
  | otherwise              = ["-w", "-v0"]


-- | GHC <7.6 uses '-package-conf' instead of '-package-db'.
packageDbArgsConf :: PackageDBStack -> [String]
packageDbArgsConf dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
  (GlobalPackageDB:dbs)               -> ("-no-user-package-conf")
                                       : concatMap specific dbs
  _ -> ierror
  where
    specific (SpecificPackageDB db) = [ "-package-conf", db ]
    specific _                      = ierror
    ierror = error $ "internal error: unexpected package db stack: "
                  ++ show dbstack

-- | GHC >= 7.6 uses the '-package-db' flag. See
-- https://ghc.haskell.org/trac/ghc/ticket/5977.
packageDbArgsDb :: PackageDBStack -> [String]
-- special cases to make arguments prettier in common scenarios
packageDbArgsDb dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs)
    | all isSpecific dbs              -> concatMap single dbs
  (GlobalPackageDB:dbs)
    | all isSpecific dbs              -> "-no-user-package-db"
                                       : concatMap single dbs
  dbs                                 -> "-clear-package-db"
                                       : concatMap single dbs
 where
   single (SpecificPackageDB db) = [ "-package-db", db ]
   single GlobalPackageDB        = [ "-global-package-db" ]
   single UserPackageDB          = [ "-user-package-db" ]
   isSpecific (SpecificPackageDB _) = True
   isSpecific _                     = False

packageDbArgs :: GhcImplInfo -> PackageDBStack -> [String]
packageDbArgs implInfo
  | flagPackageConf implInfo = packageDbArgsConf
  | otherwise                = packageDbArgsDb

-- -----------------------------------------------------------------------------
-- Boilerplate Monoid instance for GhcOptions

instance Monoid GhcOptions where
  mempty = GhcOptions {
    ghcOptMode               = mempty,
    ghcOptExtra              = mempty,
    ghcOptExtraDefault       = mempty,
    ghcOptInputFiles         = mempty,
    ghcOptInputModules       = mempty,
    ghcOptOutputFile         = mempty,
    ghcOptOutputDynFile      = mempty,
    ghcOptSourcePathClear    = mempty,
    ghcOptSourcePath         = mempty,
    ghcOptComponentId        = mempty,
    ghcOptPackageDBs         = mempty,
    ghcOptPackages           = mempty,
    ghcOptHideAllPackages    = mempty,
    ghcOptNoAutoLinkPackages = mempty,
    ghcOptSigOf              = mempty,
    ghcOptLinkLibs           = mempty,
    ghcOptLinkLibPath        = mempty,
    ghcOptLinkOptions        = mempty,
    ghcOptLinkFrameworks     = mempty,
    ghcOptNoLink             = mempty,
    ghcOptLinkNoHsMain       = mempty,
    ghcOptCcOptions          = mempty,
    ghcOptCppOptions         = mempty,
    ghcOptCppIncludePath     = mempty,
    ghcOptCppIncludes        = mempty,
    ghcOptFfiIncludes        = mempty,
    ghcOptLanguage           = mempty,
    ghcOptExtensions         = mempty,
    ghcOptExtensionMap       = mempty,
    ghcOptOptimisation       = mempty,
    ghcOptDebugInfo          = mempty,
    ghcOptProfilingMode      = mempty,
    ghcOptProfilingAuto      = mempty,
    ghcOptSplitObjs          = mempty,
    ghcOptNumJobs            = mempty,
    ghcOptHPCDir             = mempty,
    ghcOptGHCiScripts        = mempty,
    ghcOptHiSuffix           = mempty,
    ghcOptObjSuffix          = mempty,
    ghcOptDynHiSuffix        = mempty,
    ghcOptDynObjSuffix       = mempty,
    ghcOptHiDir              = mempty,
    ghcOptObjDir             = mempty,
    ghcOptOutputDir          = mempty,
    ghcOptStubDir            = mempty,
    ghcOptDynLinkMode        = mempty,
    ghcOptShared             = mempty,
    ghcOptFPic               = mempty,
    ghcOptDylibName          = mempty,
    ghcOptRPaths             = mempty,
    ghcOptVerbosity          = mempty,
    ghcOptCabal              = mempty
  }
  mappend = (Semi.<>)

instance Semigroup GhcOptions where
  a <> b = GhcOptions {
    ghcOptMode               = combine ghcOptMode,
    ghcOptExtra              = combine ghcOptExtra,
    ghcOptExtraDefault       = combine ghcOptExtraDefault,
    ghcOptInputFiles         = combine ghcOptInputFiles,
    ghcOptInputModules       = combine ghcOptInputModules,
    ghcOptOutputFile         = combine ghcOptOutputFile,
    ghcOptOutputDynFile      = combine ghcOptOutputDynFile,
    ghcOptSourcePathClear    = combine ghcOptSourcePathClear,
    ghcOptSourcePath         = combine ghcOptSourcePath,
    ghcOptComponentId         = combine ghcOptComponentId,
    ghcOptPackageDBs         = combine ghcOptPackageDBs,
    ghcOptPackages           = combine ghcOptPackages,
    ghcOptHideAllPackages    = combine ghcOptHideAllPackages,
    ghcOptNoAutoLinkPackages = combine ghcOptNoAutoLinkPackages,
    ghcOptSigOf              = combine ghcOptSigOf,
    ghcOptLinkLibs           = combine ghcOptLinkLibs,
    ghcOptLinkLibPath        = combine ghcOptLinkLibPath,
    ghcOptLinkOptions        = combine ghcOptLinkOptions,
    ghcOptLinkFrameworks     = combine ghcOptLinkFrameworks,
    ghcOptNoLink             = combine ghcOptNoLink,
    ghcOptLinkNoHsMain       = combine ghcOptLinkNoHsMain,
    ghcOptCcOptions          = combine ghcOptCcOptions,
    ghcOptCppOptions         = combine ghcOptCppOptions,
    ghcOptCppIncludePath     = combine ghcOptCppIncludePath,
    ghcOptCppIncludes        = combine ghcOptCppIncludes,
    ghcOptFfiIncludes        = combine ghcOptFfiIncludes,
    ghcOptLanguage           = combine ghcOptLanguage,
    ghcOptExtensions         = combine ghcOptExtensions,
    ghcOptExtensionMap       = combine ghcOptExtensionMap,
    ghcOptOptimisation       = combine ghcOptOptimisation,
    ghcOptDebugInfo          = combine ghcOptDebugInfo,
    ghcOptProfilingMode      = combine ghcOptProfilingMode,
    ghcOptProfilingAuto      = combine ghcOptProfilingAuto,
    ghcOptSplitObjs          = combine ghcOptSplitObjs,
    ghcOptNumJobs            = combine ghcOptNumJobs,
    ghcOptHPCDir             = combine ghcOptHPCDir,
    ghcOptGHCiScripts        = combine ghcOptGHCiScripts,
    ghcOptHiSuffix           = combine ghcOptHiSuffix,
    ghcOptObjSuffix          = combine ghcOptObjSuffix,
    ghcOptDynHiSuffix        = combine ghcOptDynHiSuffix,
    ghcOptDynObjSuffix       = combine ghcOptDynObjSuffix,
    ghcOptHiDir              = combine ghcOptHiDir,
    ghcOptObjDir             = combine ghcOptObjDir,
    ghcOptOutputDir          = combine ghcOptOutputDir,
    ghcOptStubDir            = combine ghcOptStubDir,
    ghcOptDynLinkMode        = combine ghcOptDynLinkMode,
    ghcOptShared             = combine ghcOptShared,
    ghcOptFPic               = combine ghcOptFPic,
    ghcOptDylibName          = combine ghcOptDylibName,
    ghcOptRPaths             = combine ghcOptRPaths,
    ghcOptVerbosity          = combine ghcOptVerbosity,
    ghcOptCabal              = combine ghcOptCabal
  }
    where
      combine field = field a `mappend` field b
