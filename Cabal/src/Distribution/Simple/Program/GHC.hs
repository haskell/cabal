{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Simple.Program.GHC
  ( GhcOptions (..)
  , GhcMode (..)
  , GhcOptimisation (..)
  , GhcDynLinkMode (..)
  , GhcProfAuto (..)
  , ghcInvocation
  , renderGhcOptions
  , runGHC
  , runGHCWithResponseFile
  , packageDbArgsDb
  , normaliseGhcArgs
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.Compat.Semigroup (First' (..), Last' (..), Option' (..))
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.GHC.ImplInfo
import Distribution.Simple.Program.Find (getExtraPathEnv)
import Distribution.Simple.Program.ResponseFile
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils (TempFileOptions, infoNoWrap)
import Distribution.System
import Distribution.Types.ComponentId
import Distribution.Types.ParStrat
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version

import GHC.IO.Encoding (TextEncoding)
import Language.Haskell.Extension

import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Monoid (All (..), Any (..), Endo (..))
import qualified Data.Set as Set
import qualified System.Process as Process

normaliseGhcArgs :: Maybe Version -> PackageDescription -> [String] -> [String]
normaliseGhcArgs (Just ghcVersion) PackageDescription{..} ghcArgs
  | ghcVersion `withinRange` supportedGHCVersions =
      argumentFilters . filter simpleFilters . filterRtsArgs $ ghcArgs
  where
    supportedGHCVersions :: VersionRange
    supportedGHCVersions = orLaterVersion (mkVersion [8, 0])
    -- we (weakly) support unknown future GHC versions for the purpose
    -- of filtering GHC arguments

    from :: Monoid m => [Int] -> m -> m
    from version flags
      | ghcVersion `withinRange` orLaterVersion (mkVersion version) = flags
      | otherwise = mempty

    to :: Monoid m => [Int] -> m -> m
    to version flags
      | ghcVersion `withinRange` earlierVersion (mkVersion version) = flags
      | otherwise = mempty

    checkGhcFlags :: forall m. Monoid m => ([String] -> m) -> m
    checkGhcFlags fun =
      mconcat
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
            allGhcOptions =
              foldMap
                (perCompilerFlavorToList .)
                [options, profOptions, sharedOptions, staticOptions]

            filterGhcOptions :: [(CompilerFlavor, [String])] -> [[String]]
            filterGhcOptions l = [opts | (GHC, opts) <- l]

    safeToFilterWarnings :: Bool
    safeToFilterWarnings = getAll $ checkGhcFlags checkWarnings
      where
        checkWarnings :: [String] -> All
        checkWarnings = All . Set.null . foldr alter Set.empty

        alter :: String -> Set String -> Set String
        alter flag =
          appEndo $
            mconcat
              [ \s -> Endo $ if s == "-Werror" then Set.insert s else id
              , \s -> Endo $ if s == "-Wwarn" then const Set.empty else id
              , \s ->
                  from [8, 6] . Endo $
                    if s == "-Werror=compat"
                      then Set.union compatWarningSet
                      else id
              , \s ->
                  from [8, 6] . Endo $
                    if s == "-Wno-error=compat"
                      then (`Set.difference` compatWarningSet)
                      else id
              , \s ->
                  from [8, 6] . Endo $
                    if s == "-Wwarn=compat"
                      then (`Set.difference` compatWarningSet)
                      else id
              , from [8, 4] $ markFlag "-Werror=" Set.insert
              , from [8, 4] $ markFlag "-Wwarn=" Set.delete
              , from [8, 4] $ markFlag "-Wno-error=" Set.delete
              ]
              flag

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
        makeFilter :: String -> String -> Option' (First' ([String] -> [String]))
        makeFilter flag arg = Option' $ First' . filterRest <$> stripPrefix flag arg
          where
            filterRest leftOver = case dropEq leftOver of
              [] -> drop 1
              _ -> id

        checkFilter :: String -> Maybe ([String] -> [String])
        checkFilter = fmap getFirst' . getOption' . foldMap makeFilter flags

        go :: [String] -> [String]
        go [] = []
        go (arg : args) = case checkFilter arg of
          Just f -> go (f args)
          Nothing -> arg : go args

    argumentFilters :: [String] -> [String]
    argumentFilters =
      flagArgumentFilter
        ["-ghci-script", "-H", "-interactive-print"]

    -- \| Remove RTS arguments from a list.
    filterRtsArgs :: [String] -> [String]
    filterRtsArgs = snd . splitRTSArgs

    simpleFilters :: String -> Bool
    simpleFilters =
      not
        . getAny
        . mconcat
          [ flagIn simpleFlags
          , Any . isPrefixOf "-ddump-"
          , Any . isPrefixOf "-dsuppress-"
          , Any . isPrefixOf "-dno-suppress-"
          , flagIn $ invertibleFlagSet "-" ["ignore-dot-ghci"]
          , flagIn . invertibleFlagSet "-f" . mconcat $
              [
                [ "reverse-errors"
                , "warn-unused-binds"
                , "break-on-error"
                , "break-on-exception"
                , "print-bind-result"
                , "print-bind-contents"
                , "print-evld-with-show"
                , "implicit-import-qualified"
                , "error-spans"
                ]
              , from
                  [7, 8]
                  [ "print-explicit-foralls" -- maybe also earlier, but GHC-7.6 doesn't have --show-options
                  , "print-explicit-kinds"
                  ]
              , from
                  [8, 0]
                  [ "print-explicit-coercions"
                  , "print-explicit-runtime-reps"
                  , "print-equality-relations"
                  , "print-unicode-syntax"
                  , "print-expanded-synonyms"
                  , "print-potential-instances"
                  , "print-typechecker-elaboration"
                  ]
              , from
                  [8, 2]
                  [ "diagnostics-show-caret"
                  , "local-ghci-history"
                  , "show-warning-groups"
                  , "hide-source-paths"
                  , "show-hole-constraints"
                  ]
              , from [8, 4] ["show-loaded-modules"]
              , from [8, 6] ["ghci-leak-check", "no-it"]
              , from
                  [8, 10]
                  [ "defer-diagnostics" -- affects printing of diagnostics
                  , "keep-going" -- try harder, the build will still fail if it's erroneous
                  , "print-axiom-incomps" -- print more debug info for closed type families
                  ]
              , from
                  [9, 2]
                  [ "family-application-cache"
                  ]
              , from
                  [9, 6]
                  [ "print-redundant-promotion-ticks"
                  , "show-error-context"
                  ]
              , from
                  [9, 8]
                  [ "unoptimized-core-for-interpreter"
                  ]
              , from
                  [9, 10]
                  [ "diagnostics-as-json"
                  , "print-error-index-links"
                  , "break-points"
                  ]
              ]
          , flagIn $ invertibleFlagSet "-d" ["ppr-case-as-let", "ppr-ticks"]
          , isOptIntFlag
          , isIntFlag
          , if safeToFilterWarnings
              then isWarning <> (Any . ("-w" ==))
              else mempty
          , from [8, 6] $
              if safeToFilterHoles
                then isTypedHoleFlag
                else mempty
          ]

    flagIn :: Set String -> String -> Any
    flagIn set flag = Any $ Set.member flag set

    isWarning :: String -> Any
    isWarning =
      mconcat $
        map
          ((Any .) . isPrefixOf)
          ["-fwarn-", "-fno-warn-", "-W", "-Wno-"]

    simpleFlags :: Set String
    simpleFlags =
      Set.fromList . mconcat $
        [
          [ "-n"
          , "-#include"
          , "-Rghc-timing"
          , "-dstg-stats"
          , "-dth-dec-file"
          , "-dsource-stats"
          , "-dverbose-core2core"
          , "-dverbose-stg2stg"
          , "-dcore-lint"
          , "-dstg-lint"
          , "-dcmm-lint"
          , "-dasm-lint"
          , "-dannot-lint"
          , "-dshow-passes"
          , "-dfaststring-stats"
          , "-fno-max-relevant-binds"
          , "-recomp"
          , "-no-recomp"
          , "-fforce-recomp"
          , "-fno-force-recomp"
          ]
        , from
            [8, 2]
            [ "-fno-max-errors"
            , "-fdiagnostics-color=auto"
            , "-fdiagnostics-color=always"
            , "-fdiagnostics-color=never"
            , "-dppr-debug"
            , "-dno-debug-output"
            ]
        , from [8, 4] ["-ddebug-output"]
        , from [8, 4] $ to [8, 6] ["-fno-max-valid-substitutions"]
        , from [8, 6] ["-dhex-word-literals"]
        , from [8, 8] ["-fshow-docs-of-hole-fits", "-fno-show-docs-of-hole-fits"]
        , from [9, 0] ["-dlinear-core-lint"]
        , from [9, 10] ["-dipe-stats"]
        ]

    isOptIntFlag :: String -> Any
    isOptIntFlag = mconcat . map (dropIntFlag True) $ ["-v", "-j"]

    isIntFlag :: String -> Any
    isIntFlag =
      mconcat . map (dropIntFlag False) . mconcat $
        [
          [ "-fmax-relevant-binds"
          , "-ddpr-user-length"
          , "-ddpr-cols"
          , "-dtrace-level"
          , "-fghci-hist-size"
          , "-dinitial-unique"
          , "-dunique-increment"
          ]
        , from [8, 2] ["-fmax-uncovered-patterns", "-fmax-errors"]
        , from [8, 4] $ to [8, 6] ["-fmax-valid-substitutions"]
        , from [9, 12] ["-fmax-forced-spec-args", "-fwrite-if-compression"]
        ]

    dropIntFlag :: Bool -> String -> String -> Any
    dropIntFlag isOpt flag input = Any $ case stripPrefix flag input of
      Nothing -> False
      Just rest
        | isOpt && null rest -> True
        | otherwise -> case parseInt rest of
            Just _ -> True
            Nothing -> False
      where
        parseInt :: String -> Maybe Int
        parseInt = readMaybe . dropEq

    dropEq :: String -> String
    dropEq ('=' : s) = s
    dropEq s = s

    invertibleFlagSet :: String -> [String] -> Set String
    invertibleFlagSet prefix flagNames =
      Set.fromList $ (++) <$> [prefix, prefix ++ "no-"] <*> flagNames

    compatWarningSet :: Set String
    compatWarningSet =
      Set.fromList $
        mconcat
          [ from
              [8, 6]
              [ "missing-monadfail-instances"
              , "semigroup"
              , "noncanonical-monoid-instances"
              , "implicit-kind-vars"
              ]
          ]

    safeToFilterHoles :: Bool
    safeToFilterHoles =
      getAll . checkGhcFlags $
        All . fromMaybe True . fmap getLast' . getOption' . foldMap notDeferred
      where
        notDeferred :: String -> Option' (Last' Bool)
        notDeferred "-fdefer-typed-holes" = Option' . Just . Last' $ False
        notDeferred "-fno-defer-typed-holes" = Option' . Just . Last' $ True
        notDeferred _ = Option' Nothing

    isTypedHoleFlag :: String -> Any
    isTypedHoleFlag =
      mconcat
        [ flagIn . invertibleFlagSet "-f" $
            [ "show-hole-constraints"
            , "show-valid-substitutions"
            , "show-valid-hole-fits"
            , "sort-valid-hole-fits"
            , "sort-by-size-hole-fits"
            , "sort-by-subsumption-hole-fits"
            , "abstract-refinement-hole-fits"
            , "show-provenance-of-hole-fits"
            , "show-hole-matches-of-hole-fits"
            , "show-type-of-hole-fits"
            , "show-type-app-of-hole-fits"
            , "show-type-app-vars-of-hole-fits"
            , "unclutter-valid-hole-fits"
            ]
        , flagIn . Set.fromList $
            [ "-fno-max-valid-hole-fits"
            , "-fno-max-refinement-hole-fits"
            , "-fno-refinement-level-hole-fits"
            ]
        , mconcat . map (dropIntFlag False) $
            [ "-fmax-valid-hole-fits"
            , "-fmax-refinement-hole-fits"
            , "-frefinement-level-hole-fits"
            ]
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
data GhcOptions = GhcOptions
  { ghcOptMode :: Flag GhcMode
  -- ^ The major mode for the ghc invocation.
  , ghcOptExtra :: [String]
  -- ^ Any extra options to pass directly to ghc. These go at the end and hence
  -- override other stuff.
  , ghcOptExtraDefault :: [String]
  -- ^ Extra default flags to pass directly to ghc. These go at the beginning
  -- and so can be overridden by other stuff.
  , -----------------------
    -- Inputs and outputs

    ghcOptInputFiles :: NubListR (SymbolicPath Pkg File)
  -- ^ The main input files; could be .hs, .hi, .c, .o, depending on mode.
  , ghcOptInputScripts :: NubListR (SymbolicPath Pkg File)
  -- ^ Script files with irregular extensions that need -x hs.
  , ghcOptInputModules :: NubListR ModuleName
  -- ^ The names of input Haskell modules, mainly for @--make@ mode.
  , ghcOptOutputFile :: Flag (SymbolicPath Pkg File)
  -- ^ Location for output file; the @ghc -o@ flag.
  , ghcOptOutputDynFile :: Flag FilePath
  -- ^ Location for dynamic output file in 'GhcStaticAndDynamic' mode;
  -- the @ghc -dyno@ flag.
  , ghcOptSourcePathClear :: Flag Bool
  -- ^ Start with an empty search path for Haskell source files;
  -- the @ghc -i@ flag (@-i@ on its own with no path argument).
  , ghcOptSourcePath :: NubListR (SymbolicPath Pkg (Dir Source))
  -- ^ Search path for Haskell source files; the @ghc -i@ flag.
  , ghcOptUnitFiles :: [FilePath]
  -- ^ Unit files to load; the @ghc -unit@ flag.
  , -------------
    -- Packages

    ghcOptThisUnitId :: Flag String
  -- ^ The unit ID the modules will belong to; the @ghc -this-unit-id@
  -- flag (or @-this-package-key@ or @-package-name@ on older
  -- versions of GHC).  This is a 'String' because we assume you've
  -- already figured out what the correct format for this string is
  -- (we need to handle backwards compatibility.)
  , ghcOptThisComponentId :: Flag ComponentId
  -- ^ GHC doesn't make any assumptions about the format of
  -- definite unit ids, so when we are instantiating a package it
  -- needs to be told explicitly what the component being instantiated
  -- is.  This only gets set when 'ghcOptInstantiatedWith' is non-empty
  , ghcOptInstantiatedWith :: [(ModuleName, OpenModule)]
  -- ^ How the requirements of the package being compiled are to
  -- be filled.  When typechecking an indefinite package, the 'OpenModule'
  -- is always a 'OpenModuleVar'; otherwise, it specifies the installed module
  -- that instantiates a package.
  , ghcOptNoCode :: Flag Bool
  -- ^ No code? (But we turn on interface writing
  , ghcOptPackageDBs :: PackageDBStack
  -- ^ GHC package databases to use, the @ghc -package-conf@ flag.
  , ghcOptPackages
      :: NubListR (OpenUnitId, ModuleRenaming)
  -- ^ The GHC packages to bring into scope when compiling,
  -- the @ghc -package-id@ flags.
  , ghcOptHideAllPackages :: Flag Bool
  -- ^ Start with a clean package set; the @ghc -hide-all-packages@ flag
  , ghcOptWarnMissingHomeModules :: Flag Bool
  -- ^ Warn about modules, not listed in command line
  , ghcOptNoAutoLinkPackages :: Flag Bool
  -- ^ Don't automatically link in Haskell98 etc; the @ghc
  -- -no-auto-link-packages@ flag.
  , -----------------
    -- Linker stuff

    ghcOptLinkLibs :: [FilePath]
  -- ^ Names of libraries to link in; the @ghc -l@ flag.
  , ghcOptLinkLibPath :: NubListR (SymbolicPath Pkg (Dir Lib))
  -- ^ Search path for libraries to link in; the @ghc -L@ flag.
  , ghcOptLinkOptions :: [String]
  -- ^ Options to pass through to the linker; the @ghc -optl@ flag.
  , ghcOptLinkFrameworks :: NubListR String
  -- ^ OSX only: frameworks to link in; the @ghc -framework@ flag.
  , ghcOptLinkFrameworkDirs :: NubListR (SymbolicPath Pkg (Dir Framework))
  -- ^ OSX only: Search path for frameworks to link in; the
  -- @ghc -framework-path@ flag.
  , ghcOptLinkRts :: Flag Bool
  -- ^ Instruct GHC to link against @libHSrts@ when producing a shared library.
  , ghcOptNoLink :: Flag Bool
  -- ^ Don't do the link step, useful in make mode; the @ghc -no-link@ flag.
  , ghcOptLinkNoHsMain :: Flag Bool
  -- ^ Don't link in the normal RTS @main@ entry point; the @ghc -no-hs-main@
  -- flag.
  , ghcOptLinkModDefFiles :: NubListR FilePath
  -- ^ Module definition files (Windows specific)
  , --------------------
    -- C and CPP stuff

    ghcOptCcOptions :: [String]
  -- ^ Options to pass through to the C compiler; the @ghc -optc@ flag.
  , ghcOptCxxOptions :: [String]
  -- ^ Options to pass through to the C++ compiler.
  , ghcOptAsmOptions :: [String]
  -- ^ Options to pass through to the Assembler.
  , ghcOptCppOptions :: [String]
  -- ^ Options to pass through to CPP; the @ghc -optP@ flag.
  , ghcOptJSppOptions :: [String]
  -- ^ Options to pass through to CPP; the @ghc -optJSP@ flag. @since 3.16.0.0
  , ghcOptCppIncludePath :: NubListR (SymbolicPath Pkg (Dir Include))
  -- ^ Search path for CPP includes like header files; the @ghc -I@ flag.
  , ghcOptCppIncludes :: NubListR (SymbolicPath Pkg File)
  -- ^ Extra header files to include at CPP stage; the @ghc -optP-include@ flag.
  , ghcOptFfiIncludes :: NubListR FilePath
  -- ^ Extra header files to include for old-style FFI; the @ghc -#include@ flag.
  , ghcOptCcProgram :: Flag FilePath
  -- ^ Program to use for the C and C++ compiler; the @ghc -pgmc@ flag.
  , ----------------------------
    -- Language and extensions

    ghcOptLanguage :: Flag Language
  -- ^ The base language; the @ghc -XHaskell98@ or @-XHaskell2010@ flag.
  , ghcOptExtensions :: NubListR Extension
  -- ^ The language extensions; the @ghc -X@ flag.
  , ghcOptExtensionMap :: Map Extension (Maybe CompilerFlag)
  -- ^ A GHC version-dependent mapping of extensions to flags. This must be
  -- set to be able to make use of the 'ghcOptExtensions'.
  , ----------------
    -- Compilation

    ghcOptOptimisation :: Flag GhcOptimisation
  -- ^ What optimisation level to use; the @ghc -O@ flag.
  , ghcOptDebugInfo :: Flag DebugInfoLevel
  -- ^ Emit debug info; the @ghc -g@ flag.
  , ghcOptProfilingMode :: Flag Bool
  -- ^ Compile in profiling mode; the @ghc -prof@ flag.
  , ghcOptProfilingAuto :: Flag GhcProfAuto
  -- ^ Automatically add profiling cost centers; the @ghc -fprof-auto*@ flags.
  , ghcOptSplitSections :: Flag Bool
  -- ^ Use the \"split sections\" feature; the @ghc -split-sections@ flag.
  , ghcOptSplitObjs :: Flag Bool
  -- ^ Use the \"split object files\" feature; the @ghc -split-objs@ flag.
  , ghcOptNumJobs :: Flag ParStrat
  -- ^ Run N jobs simultaneously (if possible).
  , ghcOptHPCDir :: Flag (SymbolicPath Pkg (Dir Mix))
  -- ^ Enable coverage analysis; the @ghc -fhpc -hpcdir@ flags.
  , ----------------
    -- GHCi

    ghcOptGHCiScripts :: [FilePath]
  -- ^ Extra GHCi startup scripts; the @-ghci-script@ flag
  , ------------------------
    -- Redirecting outputs

    ghcOptHiSuffix :: Flag String
  , ghcOptObjSuffix :: Flag String
  , ghcOptDynHiSuffix :: Flag String
  -- ^ only in 'GhcStaticAndDynamic' mode
  , ghcOptDynObjSuffix :: Flag String
  -- ^ only in 'GhcStaticAndDynamic' mode
  , ghcOptHiDir :: Flag (SymbolicPath Pkg (Dir Artifacts))
  , ghcOptHieDir :: Flag (SymbolicPath Pkg (Dir Artifacts))
  , ghcOptObjDir :: Flag (SymbolicPath Pkg (Dir Artifacts))
  , ghcOptOutputDir :: Flag (SymbolicPath Pkg (Dir Artifacts))
  , ghcOptStubDir :: Flag (SymbolicPath Pkg (Dir Artifacts))
  , --------------------
    -- Creating libraries

    ghcOptDynLinkMode :: Flag GhcDynLinkMode
  , ghcOptStaticLib :: Flag Bool
  , ghcOptShared :: Flag Bool
  , ghcOptFPic :: Flag Bool
  , ghcOptDylibName :: Flag String
  , ghcOptRPaths :: NubListR FilePath
  , ---------------
    -- Misc flags

    ghcOptVerbosity :: Flag Verbosity
  -- ^ Get GHC to be quiet or verbose with what it's doing; the @ghc -v@ flag.
  , ghcOptExtraPath :: NubListR (SymbolicPath Pkg (Dir Build))
  -- ^ Put the extra folders in the PATH environment variable we invoke
  -- GHC with
  , ghcOptCabal :: Flag Bool
  -- ^ Let GHC know that it is Cabal that's calling it.
  -- Modifies some of the GHC error messages.
  }
  deriving (Show, Generic)

data GhcMode
  = -- | @ghc -c@
    GhcModeCompile
  | -- | @ghc@
    GhcModeLink
  | -- | @ghc --make@
    GhcModeMake
  | -- | @ghci@ \/ @ghc --interactive@
    GhcModeInteractive
  | -- | @ghc --abi-hash@
    --             | GhcModeDepAnalysis -- ^ @ghc -M@
    --             | GhcModeEvaluate    -- ^ @ghc -e@
    GhcModeAbiHash
  deriving (Show, Eq)

data GhcOptimisation
  = -- | @-O0@
    GhcNoOptimisation
  | -- | @-O@
    GhcNormalOptimisation
  | -- | @-O2@
    GhcMaximumOptimisation
  | -- | e.g. @-Odph@
    GhcSpecialOptimisation String
  deriving (Show, Eq)

data GhcDynLinkMode
  = -- | @-static@
    GhcStaticOnly
  | -- | @-dynamic@
    GhcDynamicOnly
  | -- | @-static -dynamic-too@
    GhcStaticAndDynamic
  deriving (Show, Eq)

data GhcProfAuto
  = -- | @-fprof-auto@
    GhcProfAutoAll
  | -- | @-fprof-auto-top@
    GhcProfAutoToplevel
  | -- | @-fprof-auto-exported@
    GhcProfAutoExported
  | -- | @-fprof-late
    GhcProfLate
  deriving (Show, Eq)

runGHC
  :: Verbosity
  -> ConfiguredProgram
  -> Compiler
  -> Platform
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> GhcOptions
  -> IO ()
runGHC verbosity ghcProg comp platform mbWorkDir opts = do
  runProgramInvocation verbosity
    =<< ghcInvocation verbosity ghcProg comp platform mbWorkDir opts

runGHCWithResponseFile
  :: FilePath
  -> Maybe TextEncoding
  -> TempFileOptions
  -> Verbosity
  -> ConfiguredProgram
  -> Compiler
  -> Platform
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> GhcOptions
  -> IO ()
runGHCWithResponseFile fileNameTemplate encoding tempFileOptions verbosity ghcProg comp platform maybeWorkDir opts = do
  invocation <- ghcInvocation verbosity ghcProg comp platform maybeWorkDir opts

  let compilerSupportsResponseFiles =
        case compilerCompatVersion GHC comp of
          -- GHC 9.4 is the first version which supports response files.
          Just version -> version >= mkVersion [9, 4]
          Nothing -> False

      args = progInvokeArgs invocation

      -- Don't use response files if the first argument is `--interactive`, for
      -- two related reasons.
      --
      -- `hie-bios` relies on a hack to intercept the command-line that `Cabal`
      -- supplies to `ghc`.  Specifically, `hie-bios` creates a script around
      -- `ghc` that detects if the first option is `--interactive` and if so then
      -- instead of running `ghc` it prints the command-line that `ghc` was given
      -- instead of running the command:
      --
      -- https://github.com/haskell/hie-bios/blob/ce863dba7b57ded20160b4f11a487e4ff8372c08/wrappers/cabal#L7
      --
      -- … so we can't store that flag in the response file, otherwise that will
      -- break.  However, even if we were to add a special-case to keep that flag
      -- out of the response file things would still break because `hie-bios`
      -- stores the arguments to `ghc` that the wrapper script outputs and reuses
      -- them later.  That breaks if you use a response file because it will
      -- store an argument like `@…/ghc36000-0.rsp` which is a temporary path
      -- that no longer exists after the wrapper script completes.
      --
      -- The work-around here is that we don't use a response file at all if the
      -- first argument (and only the first argument) to `ghc` is
      -- `--interactive`.  This ensures that `hie-bios` and all downstream
      -- utilities (e.g. `haskell-language-server`) continue working.
      --
      --
      useResponseFile =
        case args of
          "--interactive" : _ -> False
          _ -> compilerSupportsResponseFiles

  if not useResponseFile
    then runProgramInvocation verbosity invocation
    else do
      let (rtsArgs, otherArgs) = splitRTSArgs args

      withResponseFile
        verbosity
        tempFileOptions
        fileNameTemplate
        encoding
        otherArgs
        $ \responseFile -> do
          let newInvocation =
                invocation{progInvokeArgs = ('@' : responseFile) : rtsArgs}

          infoNoWrap verbosity $
            "GHC response file arguments: "
              <> case otherArgs of
                [] -> ""
                arg : args' -> Process.showCommandForUser arg args'

          runProgramInvocation verbosity newInvocation

ghcInvocation
  :: Verbosity
  -> ConfiguredProgram
  -> Compiler
  -> Platform
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> GhcOptions
  -> IO ProgramInvocation
ghcInvocation verbosity ghcProg comp platform mbWorkDir opts = do
  -- NOTE: GHC is the only program whose path we modify with more values than
  -- the standard @extra-prog-path@, namely the folders of the executables in
  -- the components, see @componentGhcOptions@.
  let envOverrides = programOverrideEnv ghcProg
  extraPath <-
    getExtraPathEnv verbosity envOverrides $
      map getSymbolicPath $
        fromNubListR $
          ghcOptExtraPath opts
  let ghcProg' = ghcProg{programOverrideEnv = envOverrides ++ extraPath}
  return $
    programInvocationCwd mbWorkDir ghcProg' $
      renderGhcOptions comp platform opts

-- TODO: use the -working-dir GHC flag instead of setting the process
-- working directory, as this improves error messages.

renderGhcOptions :: Compiler -> Platform -> GhcOptions -> [String]
renderGhcOptions comp _platform@(Platform _arch os) opts
  | compilerFlavor comp `notElem` [GHC, GHCJS] =
      error $
        "Distribution.Simple.Program.GHC.renderGhcOptions: "
          ++ "compiler flavor must be 'GHC' or 'GHCJS'!"
  | otherwise =
      concat
        [ case flagToMaybe (ghcOptMode opts) of
            Nothing -> []
            Just GhcModeCompile -> ["-c"]
            Just GhcModeLink -> []
            Just GhcModeMake -> ["--make"]
            Just GhcModeInteractive -> ["--interactive"]
            Just GhcModeAbiHash -> ["--abi-hash"]
        , --     Just GhcModeDepAnalysis -> ["-M"]
          --     Just GhcModeEvaluate    -> ["-e", expr]

          ghcOptExtraDefault opts
        , ["-no-link" | flagBool ghcOptNoLink]
        , ["-flink-rts" | flagBool ghcOptLinkRts]
        , ---------------
          -- Misc flags

          maybe [] verbosityOpts (flagToMaybe (ghcOptVerbosity opts))
        , ["-fbuilding-cabal-package" | flagBool ghcOptCabal]
        , ----------------
          -- Compilation

          case flagToMaybe (ghcOptOptimisation opts) of
            Nothing -> []
            Just GhcNoOptimisation -> ["-O0"]
            Just GhcNormalOptimisation -> ["-O"]
            Just GhcMaximumOptimisation -> ["-O2"]
            Just (GhcSpecialOptimisation s) -> ["-O" ++ s] -- eg -Odph
        , case flagToMaybe (ghcOptDebugInfo opts) of
            Nothing -> []
            Just NoDebugInfo -> []
            Just MinimalDebugInfo -> ["-g1"]
            Just NormalDebugInfo -> ["-g2"]
            Just MaximalDebugInfo -> ["-g3"]
        , ["-prof" | flagBool ghcOptProfilingMode]
        , case flagToMaybe (ghcOptProfilingAuto opts) of
            _
              | not (flagBool ghcOptProfilingMode) ->
                  []
            Nothing -> []
            Just GhcProfAutoAll
              | flagProfAuto implInfo -> ["-fprof-auto"]
              | otherwise -> ["-auto-all"] -- not the same, but close
            Just GhcProfLate
              | flagProfLate implInfo -> ["-fprof-late"]
              | otherwise -> ["-fprof-auto-top"] -- not the same, not very close, but what we have.
            Just GhcProfAutoToplevel
              | flagProfAuto implInfo -> ["-fprof-auto-top"]
              | otherwise -> ["-auto-all"]
            Just GhcProfAutoExported
              | flagProfAuto implInfo -> ["-fprof-auto-exported"]
              | otherwise -> ["-auto"]
        , ["-split-sections" | flagBool ghcOptSplitSections]
        , case compilerCompatVersion GHC comp of
            -- the -split-objs flag was removed in GHC 9.8
            Just ver | ver >= mkVersion [9, 8] -> []
            _ -> ["-split-objs" | flagBool ghcOptSplitObjs]
        , case flagToMaybe (ghcOptHPCDir opts) of
            Nothing -> []
            Just hpcdir -> ["-fhpc", "-hpcdir", u hpcdir]
        , if parmakeSupported comp
            then case ghcOptNumJobs opts of
              NoFlag -> []
              Flag Serial -> []
              Flag (UseSem name) ->
                if jsemSupported comp
                  then ["-jsem " ++ name]
                  else []
              Flag (NumJobs n) -> ["-j" ++ maybe "" show n]
            else []
        , --------------------
          -- Creating libraries

          ["-staticlib" | flagBool ghcOptStaticLib]
        , ["-shared" | flagBool ghcOptShared]
        , case flagToMaybe (ghcOptDynLinkMode opts) of
            Nothing -> []
            Just GhcStaticOnly -> ["-static"]
            Just GhcDynamicOnly -> ["-dynamic"]
            Just GhcStaticAndDynamic -> ["-static", "-dynamic-too"]
        , ["-fPIC" | flagBool ghcOptFPic]
        , concat [["-dylib-install-name", libname] | libname <- flag ghcOptDylibName]
        , ------------------------
          -- Redirecting outputs

          concat [["-osuf", suf] | suf <- flag ghcOptObjSuffix]
        , concat [["-hisuf", suf] | suf <- flag ghcOptHiSuffix]
        , concat [["-dynosuf", suf] | suf <- flag ghcOptDynObjSuffix]
        , concat [["-dynhisuf", suf] | suf <- flag ghcOptDynHiSuffix]
        , concat [["-outputdir", u dir] | dir <- flag ghcOptOutputDir]
        , concat [["-odir", u dir] | dir <- flag ghcOptObjDir]
        , concat [["-hidir", u dir] | dir <- flag ghcOptHiDir]
        , concat [["-hiedir", u dir] | dir <- flag ghcOptHieDir]
        , concat [["-stubdir", u dir] | dir <- flag ghcOptStubDir]
        , -----------------------
          -- Source search path

          ["-i" | flagBool ghcOptSourcePathClear]
        , ["-i" ++ u dir | dir <- flags ghcOptSourcePath]
        , --------------------

          --------------------
          -- CPP, C, and C++ stuff

          ["-I" ++ u dir | dir <- flags ghcOptCppIncludePath]
        , ["-optP" ++ opt | opt <- ghcOptCppOptions opts]
        , ["-optJSP" ++ opt | opt <- ghcOptJSppOptions opts]
        , concat
            [ ["-optP-include", "-optP" ++ u inc]
            | inc <- flags ghcOptCppIncludes
            ]
        , ["-optc" ++ opt | opt <- ghcOptCcOptions opts]
        , -- C++ compiler options: GHC >= 8.10 requires -optcxx, older requires -optc
          -- https://gitlab.haskell.org/ghc/ghc/-/issues/16477
          let cxxflag = case compilerCompatVersion GHC comp of
                Just v | v >= mkVersion [8, 10] -> "-optcxx"
                _ -> "-optc"
           in [cxxflag ++ opt | opt <- ghcOptCxxOptions opts]
        , ["-opta" ++ opt | opt <- ghcOptAsmOptions opts]
        , concat [["-pgmc", cc] | cc <- flag ghcOptCcProgram]
        , -----------------
          -- Linker stuff

          ["-optl" ++ opt | opt <- ghcOptLinkOptions opts]
        , ["-l" ++ lib | lib <- ghcOptLinkLibs opts]
        , ["-L" ++ u dir | dir <- flags ghcOptLinkLibPath]
        , if isOSX
            then
              concat
                [ ["-framework", fmwk]
                | fmwk <- flags ghcOptLinkFrameworks
                ]
            else []
        , if isOSX
            then
              concat
                [ ["-framework-path", u path]
                | path <- flags ghcOptLinkFrameworkDirs
                ]
            else []
        , ["-no-hs-main" | flagBool ghcOptLinkNoHsMain]
        , ["-dynload deploy" | not (null (flags ghcOptRPaths))]
        , ["-optl-Wl,-rpath," ++ dir | dir <- flags ghcOptRPaths]
        , flags ghcOptLinkModDefFiles
        , -------------
          -- Packages

          concat
            [ [ if
                    | unitIdSupported comp -> "-this-unit-id"
                    | packageKeySupported comp -> "-this-package-key"
                    | otherwise -> "-package-name"
              , this_arg
              ]
            | this_arg <- flag ghcOptThisUnitId
            ]
        , concat
            [ ["-this-component-id", prettyShow this_cid]
            | this_cid <- flag ghcOptThisComponentId
            ]
        , if null (ghcOptInstantiatedWith opts)
            then []
            else
              "-instantiated-with"
                : intercalate
                  ","
                  ( map
                      ( \(n, m) ->
                          prettyShow n
                            ++ "="
                            ++ prettyShow m
                      )
                      (ghcOptInstantiatedWith opts)
                  )
                : []
        , concat [["-fno-code", "-fwrite-interface"] | flagBool ghcOptNoCode]
        , ["-hide-all-packages" | flagBool ghcOptHideAllPackages]
        , ["-Wmissing-home-modules" | flagBool ghcOptWarnMissingHomeModules]
        , ["-no-auto-link-packages" | flagBool ghcOptNoAutoLinkPackages]
        , packageDbArgs implInfo (interpretPackageDBStack Nothing (ghcOptPackageDBs opts))
        , concat $
            let space "" = ""
                space xs = ' ' : xs
             in [ ["-package-id", prettyShow ipkgid ++ space (prettyShow rns)]
                | (ipkgid, rns) <- flags ghcOptPackages
                ]
        , ----------------------------
          -- Language and extensions

          if supportsHaskell2010 implInfo
            then ["-X" ++ prettyShow lang | lang <- flag ghcOptLanguage]
            else []
        , [ ext'
          | ext <- flags ghcOptExtensions
          , ext' <- case Map.lookup ext (ghcOptExtensionMap opts) of
              Just (Just arg) -> [arg]
              Just Nothing -> []
              Nothing ->
                error $
                  "Distribution.Simple.Program.GHC.renderGhcOptions: "
                    ++ prettyShow ext
                    ++ " not present in ghcOptExtensionMap."
          ]
        , ----------------
          -- GHCi

          concat
            [ ["-ghci-script", script] | script <- ghcOptGHCiScripts opts, flagGhciScript implInfo
            ]
        , ---------------
          -- Inputs

          -- Specify the input file(s) first, so that in ghci the `main-is` module is
          -- in scope instead of the first module defined in `other-modules`.
          map u $ flags ghcOptInputFiles
        , concat [["-x", "hs", u script] | script <- flags ghcOptInputScripts]
        , [prettyShow modu | modu <- flags ghcOptInputModules]
        , concat [["-o", u out] | out <- flag ghcOptOutputFile]
        , concat [["-dyno", out] | out <- flag ghcOptOutputDynFile]
        , -- unit files
          concat [["-unit", "@" ++ unit] | unit <- ghcOptUnitFiles opts]
        , ---------------
          -- Extra

          ghcOptExtra opts
        ]
  where
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    u :: SymbolicPath Pkg to -> FilePath
    u = interpretSymbolicPathCWD
    implInfo = getImplInfo comp
    isOSX = os == OSX
    flag flg = flagToList (flg opts)
    flags flg = fromNubListR . flg $ opts
    flagBool flg = fromFlagOrDefault False (flg opts)

verbosityOpts :: Verbosity -> [String]
verbosityOpts verbosity
  | verbosity >= deafening = ["-v"]
  | verbosity >= normal = []
  | otherwise = ["-w", "-v0"]

-- | GHC <7.6 uses '-package-conf' instead of '-package-db'.
packageDbArgsConf :: PackageDBStackCWD -> [String]
packageDbArgsConf dbstack = case dbstack of
  (GlobalPackageDB : UserPackageDB : dbs) -> concatMap specific dbs
  (GlobalPackageDB : dbs) ->
    ("-no-user-package-conf")
      : concatMap specific dbs
  _ -> ierror
  where
    specific (SpecificPackageDB db) = ["-package-conf", db]
    specific _ = ierror
    ierror =
      error $
        "internal error: unexpected package db stack: "
          ++ show dbstack

-- | GHC >= 7.6 uses the '-package-db' flag. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5977.
packageDbArgsDb :: PackageDBStackCWD -> [String]
-- special cases to make arguments prettier in common scenarios
packageDbArgsDb dbstack = case dbstack of
  (GlobalPackageDB : UserPackageDB : dbs)
    | all isSpecific dbs -> concatMap single dbs
  (GlobalPackageDB : dbs)
    | all isSpecific dbs ->
        "-no-user-package-db"
          : concatMap single dbs
  dbs ->
    "-clear-package-db"
      : concatMap single dbs
  where
    single (SpecificPackageDB db) = ["-package-db", db]
    single GlobalPackageDB = ["-global-package-db"]
    single UserPackageDB = ["-user-package-db"]
    isSpecific (SpecificPackageDB _) = True
    isSpecific _ = False

packageDbArgs :: GhcImplInfo -> PackageDBStackCWD -> [String]
packageDbArgs implInfo
  | flagPackageConf implInfo = packageDbArgsConf
  | otherwise = packageDbArgsDb

-- | Split a list of command-line arguments into RTS arguments and non-RTS
-- arguments.
splitRTSArgs :: [String] -> ([String], [String])
splitRTSArgs args =
  let addRTSArg arg ~(rtsArgs, nonRTSArgs) = (arg : rtsArgs, nonRTSArgs)
      addNonRTSArg arg ~(rtsArgs, nonRTSArgs) = (rtsArgs, arg : nonRTSArgs)

      go _ [] = ([], [])
      go isRTSArg (arg : rest) =
        case arg of
          "+RTS" -> addRTSArg arg $ go True rest
          "-RTS" -> addRTSArg arg $ go False rest
          "--RTS" -> ([arg], rest)
          "--" -> ([], arg : rest)
          _ ->
            if isRTSArg
              then addRTSArg arg $ go isRTSArg rest
              else addNonRTSArg arg $ go isRTSArg rest
   in go False args

-- -----------------------------------------------------------------------------
-- Boilerplate Monoid instance for GhcOptions

instance Monoid GhcOptions where
  mempty = gmempty
  mappend = (<>)

instance Semigroup GhcOptions where
  (<>) = gmappend
