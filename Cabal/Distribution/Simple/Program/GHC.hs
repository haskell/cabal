{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Simple.Program.GHC (
    GhcOptions(..),
    GhcMode(..),
    GhcOptimisation(..),
    GhcDynLinkMode(..),
    GhcProfAuto(..),

    ghcInvocation,
    renderGhcOptions,

    runGHC,

    packageDbArgsDb,
    normaliseGhcArgs

  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Backpack
import Distribution.Simple.GHC.ImplInfo
import Distribution.PackageDescription hiding (Flag)
import Distribution.ModuleName
import Distribution.Simple.Compiler hiding (Flag)
import qualified Distribution.Simple.Compiler as Compiler (Flag)
import Distribution.Simple.Flag
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Run
import Distribution.System
import Distribution.Pretty
import Distribution.Types.ComponentId
import Distribution.Verbosity
import Distribution.Version
import Distribution.Utils.NubList
import Language.Haskell.Extension

import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Monoid (All(..), Any(..), Endo(..), First(..), Last(..))
import Data.Set (Set)
import qualified Data.Set as Set

normaliseGhcArgs :: Maybe Version -> PackageDescription -> [String] -> [String]
normaliseGhcArgs (Just ghcVersion) PackageDescription{..} ghcArgs
   | ghcVersion `withinRange` supportedGHCVersions
   = argumentFilters . filter simpleFilters . filterRtsOpts $ ghcArgs
  where
    supportedGHCVersions :: VersionRange
    supportedGHCVersions = intersectVersionRanges
        (orLaterVersion (mkVersion [8,0]))
        (earlierVersion (mkVersion [8,7]))

    from :: Monoid m => [Int] -> m -> m
    from version flags
      | ghcVersion `withinRange` orLaterVersion (mkVersion version) = flags
      | otherwise = mempty

    to :: Monoid m => [Int] -> m -> m
    to version flags
      | ghcVersion `withinRange` earlierVersion (mkVersion version) = flags
      | otherwise = mempty

    checkGhcFlags :: forall m . Monoid m => ([String] -> m) -> m
    checkGhcFlags fun = mconcat
        [ fun ghcArgs
        , checkComponentFlags libBuildInfo pkgLibs
        , checkComponentFlags buildInfo executables
        , checkComponentFlags testBuildInfo testSuites
        , checkComponentFlags benchmarkBuildInfo benchmarks
        ]
      where
        pkgLibs = maybeToList library ++ subLibraries

        checkComponentFlags :: (a -> BuildInfo) -> [a] -> m
        checkComponentFlags getInfo = foldMap (checkComponent . getInfo)
          where
            checkComponent :: BuildInfo -> m
            checkComponent = foldMap fun . filterGhcOptions . allGhcOptions

            allGhcOptions :: BuildInfo -> [(CompilerFlavor, [String])]
            allGhcOptions = foldMap (perCompilerFlavorToList .)
                [options, profOptions, sharedOptions, staticOptions]

            filterGhcOptions :: [(CompilerFlavor, [String])] -> [[String]]
            filterGhcOptions l = [opts | (GHC, opts) <- l]

    safeToFilterWarnings :: Bool
    safeToFilterWarnings = getAll $ checkGhcFlags checkWarnings
      where
        checkWarnings :: [String] -> All
        checkWarnings = All . Set.null . foldr alter Set.empty

        alter :: String -> Set String -> Set String
        alter flag = appEndo $ mconcat
            [ \s -> Endo $ if s == "-Werror" then Set.insert s else id
            , \s -> Endo $ if s == "-Wwarn" then const Set.empty else id
            , \s -> from [8,6] . Endo $
                    if s == "-Werror=compat"
                    then Set.union compatWarningSet else id
            , \s -> from [8,6] . Endo $
                    if s == "-Wno-error=compat"
                    then (`Set.difference` compatWarningSet) else id
            , \s -> from [8,6] . Endo $
                    if s == "-Wwarn=compat"
                    then (`Set.difference` compatWarningSet) else id
            , from [8,4] $ markFlag "-Werror=" Set.insert
            , from [8,4] $ markFlag "-Wwarn=" Set.delete
            , from [8,4] $ markFlag "-Wno-error=" Set.delete
            ] flag

        markFlag
            :: String
            -> (String -> Set String -> Set String)
            -> String
            -> Endo (Set String)
        markFlag name update flag = Endo $ case stripPrefix name flag of
            Just rest | not (null rest) && rest /= "compat" -> update rest
            _ -> id

    flagArgumentFilter :: [String] -> [String] -> [String]
    flagArgumentFilter flags = go
      where
        makeFilter :: String -> String -> First ([String] -> [String])
        makeFilter flag arg = First $ filterRest <$> stripPrefix flag arg
          where
            filterRest leftOver = case dropEq leftOver of
                [] -> drop 1
                _ -> id

        checkFilter :: String -> Maybe ([String] -> [String])
        checkFilter = getFirst . mconcat (map makeFilter flags)

        go :: [String] -> [String]
        go [] = []
        go (arg:args) = case checkFilter arg of
            Just f -> go (f args)
            Nothing -> arg : go args

    argumentFilters :: [String] -> [String]
    argumentFilters = flagArgumentFilter
        ["-ghci-script", "-H", "-interactive-print"]

    filterRtsOpts :: [String] -> [String]
    filterRtsOpts = go False
      where
        go :: Bool -> [String] -> [String]
        go _ [] = []
        go _ ("+RTS":opts) = go True opts
        go _ ("-RTS":opts) = go False opts
        go isRTSopts (opt:opts) = addOpt $ go isRTSopts opts
          where
            addOpt | isRTSopts = id
                   | otherwise = (opt:)

    simpleFilters :: String -> Bool
    simpleFilters = not . getAny . mconcat
      [ flagIn simpleFlags
      , Any . isPrefixOf "-ddump-"
      , Any . isPrefixOf "-dsuppress-"
      , Any . isPrefixOf "-dno-suppress-"
      , flagIn $ invertibleFlagSet "-" ["ignore-dot-ghci"]
      , flagIn . invertibleFlagSet "-f" . mconcat $
            [ [ "reverse-errors", "warn-unused-binds", "break-on-error"
              , "break-on-exception", "print-bind-result"
              , "print-bind-contents", "print-evld-with-show"
              , "implicit-import-qualified", "error-spans"
              ]
            , from [8,2]
                [ "diagnostics-show-caret", "local-ghci-history"
                , "show-warning-groups", "hide-source-paths"
                , "show-hole-constraints"
                ]
            , from [8,4] ["show-loaded-modules"]
            , from [8,6] [ "ghci-leak-check", "no-it" ]
            ]
      , flagIn . invertibleFlagSet "-d" $ [ "ppr-case-as-let", "ppr-ticks" ]
      , isOptIntFlag
      , isIntFlag
      , if safeToFilterWarnings
           then isWarning <> (Any . ("-w"==))
           else mempty
      , from [8,6] $
        if safeToFilterHoles
           then isTypedHoleFlag
           else mempty
      ]

    flagIn :: Set String -> String -> Any
    flagIn set flag = Any $ Set.member flag set

    isWarning :: String -> Any
    isWarning = mconcat $ map ((Any .) . isPrefixOf)
        ["-fwarn-", "-fno-warn-", "-W", "-Wno-"]

    simpleFlags :: Set String
    simpleFlags = Set.fromList . mconcat $
      [ [ "-n", "-#include", "-Rghc-timing", "-dsuppress-all", "-dstg-stats"
        , "-dth-dec-file", "-dsource-stats", "-dverbose-core2core"
        , "-dverbose-stg2stg", "-dcore-lint", "-dstg-lint", "-dcmm-lint"
        , "-dasm-lint", "-dannot-lint", "-dshow-passes", "-dfaststring-stats"
        , "-fno-max-relevant-binds", "-recomp", "-no-recomp", "-fforce-recomp"
        , "-fno-force-recomp"
        ]

      , from [8,2]
          [ "-fno-max-errors", "-fdiagnostics-color=auto"
          , "-fdiagnostics-color=always", "-fdiagnostics-color=never"
          , "-dppr-debug", "-dno-debug-output"
          ]

      , from [8,4] [ "-ddebug-output" ]
      , from [8,4] $ to [8,6] [ "-fno-max-valid-substitutions" ]
      , from [8,6] [ "-dhex-word-literals" ]
      ]

    isOptIntFlag :: String -> Any
    isOptIntFlag = mconcat . map (dropIntFlag True) $ ["-v", "-j"]

    isIntFlag :: String -> Any
    isIntFlag = mconcat . map (dropIntFlag False) . mconcat $
        [ [ "-fmax-relevant-binds", "-ddpr-user-length", "-ddpr-cols"
          , "-dtrace-level", "-fghci-hist-size" ]
        , from [8,2] ["-fmax-uncovered-patterns", "-fmax-errors"]
        , from [8,4] $ to [8,6] ["-fmax-valid-substitutions"]
        ]

    dropIntFlag :: Bool -> String -> String -> Any
    dropIntFlag isOpt flag input = Any $ case stripPrefix flag input of
        Nothing -> False
        Just rest | isOpt && null rest -> True
                  | otherwise -> case parseInt rest of
                        Just _ -> True
                        Nothing -> False
      where
        parseInt :: String -> Maybe Int
        parseInt = readMaybe . dropEq

    dropEq :: String -> String
    dropEq ('=':s) = s
    dropEq s = s

    invertibleFlagSet :: String -> [String] -> Set String
    invertibleFlagSet prefix flagNames =
      Set.fromList $ (++) <$> [prefix, prefix ++ "no-"] <*> flagNames

    compatWarningSet :: Set String
    compatWarningSet = Set.fromList $ mconcat
        [ from [8,6]
            [ "missing-monadfail-instances", "semigroup"
            , "noncanonical-monoid-instances", "implicit-kind-vars" ]
        ]

    safeToFilterHoles :: Bool
    safeToFilterHoles = getAll . checkGhcFlags $ fromLast . foldMap notDeferred
      where
        fromLast :: Last All -> All
        fromLast = fromMaybe (All True) . getLast

        notDeferred :: String -> Last All
        notDeferred "-fdefer-typed-holes" = Last . Just . All $ False
        notDeferred "-fno-defer-typed-holes" = Last . Just . All $ True
        notDeferred _ = Last Nothing

    isTypedHoleFlag :: String -> Any
    isTypedHoleFlag = mconcat
        [ flagIn . invertibleFlagSet "-f" $
            [ "show-hole-constraints", "show-valid-substitutions"
            , "show-valid-hole-fits", "sort-valid-hole-fits"
            , "sort-by-size-hole-fits", "sort-by-subsumption-hole-fits"
            , "abstract-refinement-hole-fits", "show-provenance-of-hole-fits"
            , "show-hole-matches-of-hole-fits", "show-type-of-hole-fits"
            , "show-type-app-of-hole-fits", "show-type-app-vars-of-hole-fits"
            , "unclutter-valid-hole-fits"
            ]
        , flagIn . Set.fromList $
            [ "-fno-max-valid-hole-fits", "-fno-max-refinement-hole-fits"
            , "-fno-refinement-level-hole-fits" ]
        , mconcat . map (dropIntFlag False) $
            [ "-fmax-valid-hole-fits", "-fmax-refinement-hole-fits"
            , "-frefinement-level-hole-fits" ]
        ]

normaliseGhcArgs _ _ args = args

-- | A structured set of GHC options/flags
--
-- Note that options containing lists fall into two categories:
--
--  * options that can be safely deduplicated, e.g. input modules or
--    enabled extensions;
--  * options that cannot be deduplicated in general without changing
--    semantics, e.g. extra ghc options or linking options.
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
  ghcOptInputFiles    :: NubListR FilePath,

  -- | The names of input Haskell modules, mainly for @--make@ mode.
  ghcOptInputModules  :: NubListR ModuleName,

  -- | Location for output file; the @ghc -o@ flag.
  ghcOptOutputFile    :: Flag FilePath,

  -- | Location for dynamic output file in 'GhcStaticAndDynamic' mode;
  -- the @ghc -dyno@ flag.
  ghcOptOutputDynFile :: Flag FilePath,

  -- | Start with an empty search path for Haskell source files;
  -- the @ghc -i@ flag (@-i@ on its own with no path argument).
  ghcOptSourcePathClear :: Flag Bool,

  -- | Search path for Haskell source files; the @ghc -i@ flag.
  ghcOptSourcePath    :: NubListR FilePath,

  -------------
  -- Packages

  -- | The unit ID the modules will belong to; the @ghc -this-unit-id@
  -- flag (or @-this-package-key@ or @-package-name@ on older
  -- versions of GHC).  This is a 'String' because we assume you've
  -- already figured out what the correct format for this string is
  -- (we need to handle backwards compatibility.)
  ghcOptThisUnitId   :: Flag String,

  -- | GHC doesn't make any assumptions about the format of
  -- definite unit ids, so when we are instantiating a package it
  -- needs to be told explicitly what the component being instantiated
  -- is.  This only gets set when 'ghcOptInstantiatedWith' is non-empty
  ghcOptThisComponentId :: Flag ComponentId,

  -- | How the requirements of the package being compiled are to
  -- be filled.  When typechecking an indefinite package, the 'OpenModule'
  -- is always a 'OpenModuleVar'; otherwise, it specifies the installed module
  -- that instantiates a package.
  ghcOptInstantiatedWith :: [(ModuleName, OpenModule)],

  -- | No code? (But we turn on interface writing
  ghcOptNoCode :: Flag Bool,

  -- | GHC package databases to use, the @ghc -package-conf@ flag.
  ghcOptPackageDBs    :: PackageDBStack,

  -- | The GHC packages to bring into scope when compiling,
  -- the @ghc -package-id@ flags.
  ghcOptPackages      ::
    NubListR (OpenUnitId, ModuleRenaming),

  -- | Start with a clean package set; the @ghc -hide-all-packages@ flag
  ghcOptHideAllPackages :: Flag Bool,

  -- | Warn about modules, not listed in command line
  ghcOptWarnMissingHomeModules :: Flag Bool,

  -- | Don't automatically link in Haskell98 etc; the @ghc
  -- -no-auto-link-packages@ flag.
  ghcOptNoAutoLinkPackages :: Flag Bool,

  -----------------
  -- Linker stuff

  -- | Names of libraries to link in; the @ghc -l@ flag.
  ghcOptLinkLibs      :: [FilePath],

  -- | Search path for libraries to link in; the @ghc -L@ flag.
  ghcOptLinkLibPath  :: NubListR FilePath,

  -- | Options to pass through to the linker; the @ghc -optl@ flag.
  ghcOptLinkOptions   :: [String],

  -- | OSX only: frameworks to link in; the @ghc -framework@ flag.
  ghcOptLinkFrameworks :: NubListR String,

  -- | OSX only: Search path for frameworks to link in; the
  -- @ghc -framework-path@ flag.
  ghcOptLinkFrameworkDirs :: NubListR String,

  -- | Don't do the link step, useful in make mode; the @ghc -no-link@ flag.
  ghcOptNoLink :: Flag Bool,

  -- | Don't link in the normal RTS @main@ entry point; the @ghc -no-hs-main@
  -- flag.
  ghcOptLinkNoHsMain :: Flag Bool,

  -- | Module definition files (Windows specific)
  ghcOptLinkModDefFiles :: NubListR FilePath,

  --------------------
  -- C and CPP stuff

  -- | Options to pass through to the C compiler; the @ghc -optc@ flag.
  ghcOptCcOptions     :: [String],

  -- | Options to pass through to the C++ compiler.
  ghcOptCxxOptions     :: [String],

  -- | Options to pass through to CPP; the @ghc -optP@ flag.
  ghcOptCppOptions    :: [String],

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
  ghcOptExtensionMap    :: Map Extension (Maybe Compiler.Flag),

  ----------------
  -- Compilation

  -- | What optimisation level to use; the @ghc -O@ flag.
  ghcOptOptimisation  :: Flag GhcOptimisation,

    -- | Emit debug info; the @ghc -g@ flag.
  ghcOptDebugInfo     :: Flag DebugInfoLevel,

  -- | Compile in profiling mode; the @ghc -prof@ flag.
  ghcOptProfilingMode :: Flag Bool,

  -- | Automatically add profiling cost centers; the @ghc -fprof-auto*@ flags.
  ghcOptProfilingAuto :: Flag GhcProfAuto,

  -- | Use the \"split sections\" feature; the @ghc -split-sections@ flag.
  ghcOptSplitSections :: Flag Bool,

  -- | Use the \"split object files\" feature; the @ghc -split-objs@ flag.
  ghcOptSplitObjs     :: Flag Bool,

  -- | Run N jobs simultaneously (if possible).
  ghcOptNumJobs       :: Flag (Maybe Int),

  -- | Enable coverage analysis; the @ghc -fhpc -hpcdir@ flags.
  ghcOptHPCDir        :: Flag FilePath,

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
  -- Creating libraries

  ghcOptDynLinkMode   :: Flag GhcDynLinkMode,
  ghcOptStaticLib     :: Flag Bool,
  ghcOptShared        :: Flag Bool,
  ghcOptFPic          :: Flag Bool,
  ghcOptDylibName     :: Flag String,
  ghcOptRPaths        :: NubListR FilePath,

  ---------------
  -- Misc flags

  -- | Get GHC to be quiet or verbose with what it's doing; the @ghc -v@ flag.
  ghcOptVerbosity     :: Flag Verbosity,

  -- | Put the extra folders in the PATH environment variable we invoke
  -- GHC with
  ghcOptExtraPath     :: NubListR FilePath,

  -- | Let GHC know that it is Cabal that's calling it.
  -- Modifies some of the GHC error messages.
  ghcOptCabal         :: Flag Bool

} deriving (Show, Generic)


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

runGHC :: Verbosity -> ConfiguredProgram -> Compiler -> Platform  -> GhcOptions
       -> IO ()
runGHC verbosity ghcProg comp platform opts = do
  runProgramInvocation verbosity (ghcInvocation ghcProg comp platform opts)


ghcInvocation :: ConfiguredProgram -> Compiler -> Platform -> GhcOptions
              -> ProgramInvocation
ghcInvocation prog comp platform opts =
    (programInvocation prog (renderGhcOptions comp platform opts)) {
        progInvokePathEnv = fromNubListR (ghcOptExtraPath opts)
    }

renderGhcOptions :: Compiler -> Platform -> GhcOptions -> [String]
renderGhcOptions comp _platform@(Platform _arch os) opts
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

  , ghcOptExtraDefault opts

  , [ "-no-link" | flagBool ghcOptNoLink ]

  ---------------
  -- Misc flags

  , maybe [] verbosityOpts (flagToMaybe (ghcOptVerbosity opts))

  , [ "-fbuilding-cabal-package" | flagBool ghcOptCabal ]

  ----------------
  -- Compilation

  , case flagToMaybe (ghcOptOptimisation opts) of
      Nothing                         -> []
      Just GhcNoOptimisation          -> ["-O0"]
      Just GhcNormalOptimisation      -> ["-O"]
      Just GhcMaximumOptimisation     -> ["-O2"]
      Just (GhcSpecialOptimisation s) -> ["-O" ++ s] -- eg -Odph

  , case flagToMaybe (ghcOptDebugInfo opts) of
      Nothing                                -> []
      Just NoDebugInfo                       -> []
      Just MinimalDebugInfo                  -> ["-g1"]
      Just NormalDebugInfo                   -> ["-g2"]
      Just MaximalDebugInfo                  -> ["-g3"]

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

  , [ "-split-sections" | flagBool ghcOptSplitSections ]
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
  -- Creating libraries

  , [ "-staticlib" | flagBool ghcOptStaticLib ]
  , [ "-shared"    | flagBool ghcOptShared    ]
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
  , concat [ ["-outputdir", dir] | dir <- flag ghcOptOutputDir ]
  , concat [ ["-odir",    dir] | dir <- flag ghcOptObjDir ]
  , concat [ ["-hidir",   dir] | dir <- flag ghcOptHiDir  ]
  , concat [ ["-stubdir", dir] | dir <- flag ghcOptStubDir ]

  -----------------------
  -- Source search path

  , [ "-i"        | flagBool ghcOptSourcePathClear ]
  , [ "-i" ++ dir | dir <- flags ghcOptSourcePath ]

  --------------------

  --------------------
  -- CPP, C, and C++ stuff

  , [ "-I"    ++ dir | dir <- flags ghcOptCppIncludePath ]
  , [ "-optP" ++ opt | opt <- ghcOptCppOptions opts]
  , concat [ [ "-optP-include", "-optP" ++ inc]
           | inc <- flags ghcOptCppIncludes ]
  , [ "-optc" ++ opt | opt <- ghcOptCcOptions opts]
  , [ "-optc" ++ opt | opt <- ghcOptCxxOptions opts]

  -----------------
  -- Linker stuff

  , [ "-optl" ++ opt | opt <- ghcOptLinkOptions opts]
  , ["-l" ++ lib     | lib <- ghcOptLinkLibs opts]
  , ["-L" ++ dir     | dir <- flags ghcOptLinkLibPath ]
  , if isOSX
    then concat [ ["-framework", fmwk]
                | fmwk <- flags ghcOptLinkFrameworks ]
    else []
  , if isOSX
    then concat [ ["-framework-path", path]
                | path <- flags ghcOptLinkFrameworkDirs ]
    else []
  , [ "-no-hs-main"  | flagBool ghcOptLinkNoHsMain ]
  , [ "-dynload deploy" | not (null (flags ghcOptRPaths)) ]
  , concat [ [ "-optl-Wl,-rpath," ++ dir]
           | dir <- flags ghcOptRPaths ]
  , [ modDefFile | modDefFile <- flags ghcOptLinkModDefFiles ]

  -------------
  -- Packages

  , concat [ [ case () of
                _ | unitIdSupported comp     -> "-this-unit-id"
                  | packageKeySupported comp -> "-this-package-key"
                  | otherwise                -> "-package-name"
             , this_arg ]
             | this_arg <- flag ghcOptThisUnitId ]

  , concat [ ["-this-component-id", prettyShow this_cid ]
           | this_cid <- flag ghcOptThisComponentId ]

  , if null (ghcOptInstantiatedWith opts)
        then []
        else "-instantiated-with"
             : intercalate "," (map (\(n,m) -> prettyShow n ++ "="
                                            ++ prettyShow m)
                                    (ghcOptInstantiatedWith opts))
             : []

  , concat [ ["-fno-code", "-fwrite-interface"] | flagBool ghcOptNoCode ]

  , [ "-hide-all-packages"     | flagBool ghcOptHideAllPackages ]
  , [ "-Wmissing-home-modules" | flagBool ghcOptWarnMissingHomeModules ]
  , [ "-no-auto-link-packages" | flagBool ghcOptNoAutoLinkPackages ]

  , packageDbArgs implInfo (ghcOptPackageDBs opts)

  , concat $ let space "" = ""
                 space xs = ' ' : xs
             in [ ["-package-id", prettyShow ipkgid ++ space (prettyShow rns)]
                | (ipkgid,rns) <- flags ghcOptPackages ]

  ----------------------------
  -- Language and extensions

  , if supportsHaskell2010 implInfo
    then [ "-X" ++ prettyShow lang | lang <- flag ghcOptLanguage ]
    else []

  , [ ext'
    | ext  <- flags ghcOptExtensions
    , ext' <- case Map.lookup ext (ghcOptExtensionMap opts) of
        Just (Just arg) -> [arg]
        Just Nothing    -> []
        Nothing         ->
            error $ "Distribution.Simple.Program.GHC.renderGhcOptions: "
                  ++ prettyShow ext ++ " not present in ghcOptExtensionMap."
    ]

  ----------------
  -- GHCi

  , concat [ [ "-ghci-script", script ] | script <- ghcOptGHCiScripts opts
                                        , flagGhciScript implInfo ]

  ---------------
  -- Inputs

  , [ prettyShow modu | modu <- flags ghcOptInputModules ]
  , flags ghcOptInputFiles

  , concat [ [ "-o",    out] | out <- flag ghcOptOutputFile ]
  , concat [ [ "-dyno", out] | out <- flag ghcOptOutputDynFile ]

  ---------------
  -- Extra

  , ghcOptExtra opts

  ]


  where
    implInfo     = getImplInfo comp
    isOSX        = os == OSX
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
  mempty = gmempty
  mappend = (<>)

instance Semigroup GhcOptions where
  (<>) = gmappend
