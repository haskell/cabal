module Distribution.Simple.Program.GHC (
    GhcOptions(..),
    GhcMode(..),
    GhcOptimisation(..),
    GhcDynLinkMode(..),

    ghcInvocation,
    renderGhcOptions,

    runGHC,

  ) where

import Distribution.Package
import Distribution.ModuleName
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.Setup    ( Flag(..), flagToMaybe, fromFlagOrDefault,
                                      flagToList )
--import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Run
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension   ( Language(..), Extension(..) )

import Data.Monoid

-- | A structured set of GHC options/flags
--
data GhcOptions = GhcOptions {

  -- | The major mode for the ghc invocation.
  ghcOptMode          :: Flag GhcMode,

  -- | Any extra options to pass directly to ghc. These go at the end and hence
  -- override other stuff.
  ghcOptExtra         :: [String],

  -- | Extra default flags to pass directly to ghc. These go at the beginning
  -- and so can be overridden by other stuff.
  ghcOptExtraDefault  :: [String],

  -----------------------
  -- Inputs and outputs

  -- | The main input files; could be .hs, .hi, .c, .o, depending on mode.
  ghcOptInputFiles    :: [FilePath],

  -- | The names of input Haskell modules, mainly for @--make@ mode.
  ghcOptInputModules  :: [ModuleName],

  -- | Location for output file; the @ghc -o@ flag.
  ghcOptOutputFile    :: Flag FilePath,

  -- | Location for dynamic output file in 'GhcStaticAndDynamic' mode;
  -- the @ghc -dyno@ flag.
  ghcOptOutputDynFile :: Flag FilePath,

  -- | Start with an empty search path for Haskell source files;
  -- the @ghc -i@ flag (@-i@ on it's own with no path argument).
  ghcOptSourcePathClear :: Flag Bool,

  -- | Search path for Haskell source files; the @ghc -i@ flag.
  ghcOptSourcePath    :: [FilePath],

  -------------
  -- Packages

  -- | The package name the modules will belong to; the @ghc -package-name@ flag
  ghcOptPackageName   :: Flag PackageId,

  -- | GHC package databases to use, the @ghc -package-conf@ flag
  ghcOptPackageDBs    :: PackageDBStack,

  -- | The GHC packages to use. For compatability with old and new ghc, this
  -- requires both the short and long form of the package id;
  -- the @ghc -package@ or @ghc -package-id@ flags.
  ghcOptPackages      :: [(InstalledPackageId, PackageId)],

  -- | Start with a clean package set; the @ghc -hide-all-packages@ flag
  ghcOptHideAllPackages :: Flag Bool,

  -- | Don't automatically link in Haskell98 etc; the @ghc -no-auto-link-packages@ flag.
  ghcOptNoAutoLinkPackages :: Flag Bool,

  -----------------
  -- Linker stuff

  -- | Names of libraries to link in; the @ghc -l@ flag.
  ghcOptLinkLibs      :: [FilePath],

  -- | Search path for libraries to link in; the @ghc -L@ flag.
  ghcOptLinkLibPath  :: [FilePath],

  -- | Options to pass through to the linker; the @ghc -optl@ flag.
  ghcOptLinkOptions   :: [String],

  -- | OSX only: frameworks to link in; the @ghc -framework@ flag.
  ghcOptLinkFrameworks :: [String],

  -- | Don't do the link step, useful in make mode; the @ghc -no-link@ flag.
  ghcOptNoLink :: Flag Bool,

  -- | Don't link in the normal RTS @main@ entry point; the @ghc -no-hs-main@ flag.
  ghcOptLinkNoHsMain :: Flag Bool,

  --------------------
  -- C and CPP stuff

  -- | Options to pass through to the C compiler; the @ghc -optc@ flag.
  ghcOptCcOptions     :: [String],

  -- | Options to pass through to CPP; the @ghc -optP@ flag.
  ghcOptCppOptions    :: [String],

  -- | Search path for CPP includes like header files; the @ghc -I@ flag.
  ghcOptCppIncludePath :: [FilePath],

  -- | Extra header files to include at CPP stage; the @ghc -optP-include@ flag.
  ghcOptCppIncludes    :: [FilePath],

  -- | Extra header files to include for old-style FFI; the @ghc -#include@ flag.
  ghcOptFfiIncludes    :: [FilePath],

  ----------------------------
  -- Language and extensions

  -- | The base language; the @ghc -XHaskell98@ or @-XHaskell2010@ flag.
  ghcOptLanguage      :: Flag Language,

  -- | The language extensions; the @ghc -X@ flag.
  ghcOptExtensions    :: [Extension],

  -- | A GHC version-dependent mapping of extensions to flags. This must be
  -- set to be able to make use of the 'ghcOptExtensions'.
  ghcOptExtensionMap    :: [(Extension, String)],

  ----------------
  -- Compilation

  -- | What optimisation level to use; the @ghc -O@ flag.
  ghcOptOptimisation  :: Flag GhcOptimisation,

  -- | Compile in profiling mode; the @ghc -prof@ flag.
  ghcOptProfilingMode :: Flag Bool,

  -- | Use the \"split object files\" feature; the @ghc -split-objs@ flag.
  ghcOptSplitObjs     :: Flag Bool,

  -- | Run N jobs simultaneously (if possible).
  ghcOptNumJobs       :: Flag Int,

  ----------------
  -- GHCi

  -- | Extra GHCi startup scripts; the @-ghci-script@ flag
  ghcOptGHCiScripts    :: [FilePath],

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


runGHC :: Verbosity -> ConfiguredProgram -> Compiler -> GhcOptions -> IO ()
runGHC verbosity ghcProg comp opts = do
  runProgramInvocation verbosity (ghcInvocation ghcProg comp opts)


ghcInvocation :: ConfiguredProgram -> Compiler -> GhcOptions -> ProgramInvocation
ghcInvocation prog comp opts =
    programInvocation prog (renderGhcOptions comp opts)


renderGhcOptions :: Compiler -> GhcOptions -> [String]
renderGhcOptions comp opts
  | compilerFlavor comp /= GHC =
    error $ "Distribution.Simple.Program.GHC.renderGhcOptions: "
    ++ "compiler flavor must be 'GHC'!"
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

  , [ "-fbuilding-cabal-package" | flagBool ghcOptCabal, ver >= [6,11] ]

  ----------------
  -- Compilation

  , case flagToMaybe (ghcOptOptimisation opts) of
      Nothing                         -> []
      Just GhcNoOptimisation          -> ["-O0"]
      Just GhcNormalOptimisation      -> ["-O"]
      Just GhcMaximumOptimisation     -> ["-O2"]
      Just (GhcSpecialOptimisation s) -> ["-O" ++ s] -- eg -Odph

  , [ "-prof" | flagBool ghcOptProfilingMode ]

  , [ "-split-objs" | flagBool ghcOptSplitObjs ]


  , if parmakeSupported comp
    then
      let numJobs = fromFlagOrDefault 1 (ghcOptNumJobs opts)
      in if numJobs > 1 then ["-j" ++ show numJobs] else []
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
  , concat [ ["-outputdir", dir] | dir <- flag ghcOptOutputDir, ver >= [6,10] ]
  , concat [ ["-odir",    dir] | dir <- flag ghcOptObjDir ]
  , concat [ ["-hidir",   dir] | dir <- flag ghcOptHiDir  ]
  , concat [ ["-stubdir", dir] | dir <- flag ghcOptStubDir, ver >= [6,8] ]

  -----------------------
  -- Source search path

  , [ "-i"        | flagBool ghcOptSourcePathClear ]
  , [ "-i" ++ dir | dir <- flags ghcOptSourcePath ]

  --------------------
  -- C and CPP stuff

  , [ "-I"    ++ dir | dir <- flags ghcOptCppIncludePath ]
  , [ "-optP" ++ opt | opt <- flags ghcOptCppOptions ]
  , concat [ [ "-optP-include", "-optP" ++ inc] | inc <- flags ghcOptCppIncludes ]
  , [ "-#include \"" ++ inc ++ "\"" | inc <- flags ghcOptFfiIncludes, ver < [6,11] ]
  , [ "-optc" ++ opt | opt <- flags ghcOptCcOptions ]

  -----------------
  -- Linker stuff

  , [ "-optl" ++ opt | opt <- flags ghcOptLinkOptions ]
  , ["-l" ++ lib     | lib <- flags ghcOptLinkLibs ]
  , ["-L" ++ dir     | dir <- flags ghcOptLinkLibPath ]
  , concat [ ["-framework", fmwk] | fmwk <- flags ghcOptLinkFrameworks ]
  , [ "-no-hs-main"  | flagBool ghcOptLinkNoHsMain ]

  -------------
  -- Packages

  , concat [ ["-package-name", display pkgid] | pkgid <- flag ghcOptPackageName ]

  , [ "-hide-all-packages"     | flagBool ghcOptHideAllPackages ]
  , [ "-no-auto-link-packages" | flagBool ghcOptNoAutoLinkPackages ]

  , packageDbArgs version (flags ghcOptPackageDBs)

  , concat $ if ver >= [6,11]
      then [ ["-package-id", display ipkgid] | (ipkgid,_) <- flags ghcOptPackages ]
      else [ ["-package",    display  pkgid] | (_,pkgid)  <- flags ghcOptPackages ]

  ----------------------------
  -- Language and extensions

  , if ver >= [7]
    then [ "-X" ++ display lang | lang <- flag ghcOptLanguage ]
    else []

  , [ case lookup ext (ghcOptExtensionMap opts) of
        Just arg -> arg
        Nothing  -> error $ "Distribution.Simple.Program.GHC.renderGhcOptions: "
                          ++ display ext ++ " not present in ghcOptExtensionMap."
    | ext <- ghcOptExtensions opts ]

  ----------------
  -- GHCi

  , concat [ [ "-ghci-script", script ] | script <- flags  ghcOptGHCiScripts
                                        , ver >= [7,2] ]

  ---------------
  -- Inputs

  , [ display modu | modu <- flags ghcOptInputModules ]
  , ghcOptInputFiles opts

  , concat [ [ "-o",    out] | out <- flag ghcOptOutputFile ]
  , concat [ [ "-dyno", out] | out <- flag ghcOptOutputDynFile ]

  ---------------
  -- Extra

  , ghcOptExtra opts

  ]


  where
    flag     flg = flagToList (flg opts)
    flags    flg = flg opts
    flagBool flg = fromFlagOrDefault False (flg opts)

    version@(Version ver _) = compilerVersion comp

verbosityOpts :: Verbosity -> [String]
verbosityOpts verbosity
  | verbosity >= deafening = ["-v"]
  | verbosity >= normal    = []
  | otherwise              = ["-w", "-v0"]


packageDbArgs :: Version -> PackageDBStack -> [String]
packageDbArgs (Version ver _) dbstack = case dbstack of
  (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
  (GlobalPackageDB:dbs)               -> ("-no-user-" ++ packageDbFlag)
                                       : concatMap specific dbs
  _ -> ierror
  where
    specific (SpecificPackageDB db) = [ '-':packageDbFlag , db ]
    specific _ = ierror
    ierror     = error $ "internal error: unexpected package db stack: "
                      ++ show dbstack

    packageDbFlag
      | ver < [7,5]
      = "package-conf"
      | otherwise
      = "package-db"


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
    ghcOptPackageName        = mempty,
    ghcOptPackageDBs         = mempty,
    ghcOptPackages           = mempty,
    ghcOptHideAllPackages    = mempty,
    ghcOptNoAutoLinkPackages = mempty,
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
    ghcOptProfilingMode      = mempty,
    ghcOptSplitObjs          = mempty,
    ghcOptNumJobs            = mempty,
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
    ghcOptVerbosity          = mempty,
    ghcOptCabal              = mempty
  }
  mappend a b = GhcOptions {
    ghcOptMode               = combine ghcOptMode,
    ghcOptExtra              = combine ghcOptExtra,
    ghcOptExtraDefault       = combine ghcOptExtraDefault,
    ghcOptInputFiles         = combine ghcOptInputFiles,
    ghcOptInputModules       = combine ghcOptInputModules,
    ghcOptOutputFile         = combine ghcOptOutputFile,
    ghcOptOutputDynFile      = combine ghcOptOutputDynFile,
    ghcOptSourcePathClear    = combine ghcOptSourcePathClear,
    ghcOptSourcePath         = combine ghcOptSourcePath,
    ghcOptPackageName        = combine ghcOptPackageName,
    ghcOptPackageDBs         = combine ghcOptPackageDBs,
    ghcOptPackages           = combine ghcOptPackages,
    ghcOptHideAllPackages    = combine ghcOptHideAllPackages,
    ghcOptNoAutoLinkPackages = combine ghcOptNoAutoLinkPackages,
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
    ghcOptProfilingMode      = combine ghcOptProfilingMode,
    ghcOptSplitObjs          = combine ghcOptSplitObjs,
    ghcOptNumJobs            = combine ghcOptNumJobs,
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
    ghcOptVerbosity          = combine ghcOptVerbosity,
    ghcOptCabal              = combine ghcOptCabal
  }
    where
      combine field = field a `mappend` field b
