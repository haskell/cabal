{-# LANGUAGE ViewPatterns #-}

-- | Command line options for nix-style / v2 commands.
--
-- The commands take a lot of the same options, which affect how install plan
-- is constructed.
module Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , nixStyleOptions
  , defaultNixStyleFlags
  , updNixStyleCommonSetupFlags
  , cfgVerbosity

    -- * Option filtering/grouping predicates
  , keepUnsupportedOptions
  , keepInstallOptions
  , keepIrrelevantOptions
  , keepHaddockOptions
  , keepTestOptions
  , keepBenchOptions
  , keepProfilingOptions
  , keepSolvingOptions
  , keepExeOptions
  , keepLibOptions
  , keepCoverageOptions
  , keepOutputOptions
  , keepConfigureOptions
  , keepPhaseOptions
  , keepCompilerOptions
  , keepLoggingOptions
  , keepIncludeOptions
  , keepProgOptions
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Command (OptionField (..), ShowOrParseArgs)
import Distribution.Simple.Setup
  ( BenchmarkFlags (benchmarkCommonFlags)
  , CommonSetupFlags (..)
  , HaddockFlags (..)
  , TestFlags (testCommonFlags)
  , fromFlagOrDefault
  )
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))

import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  )
import Distribution.Client.Setup
  ( ConfigExFlags
  , ConfigFlags (..)
  , InstallFlags (..)
  , benchmarkOptions
  , configureExOptions
  , configureOptions
  , haddockOptions
  , installOptions
  , liftOptions
  , testOptions
  )
import Distribution.Simple.Utils (isInfixOf)
import Distribution.Verbosity (VerbosityFlags, defaultVerbosityHandles, mkVerbosity)

data NixStyleFlags a = NixStyleFlags
  { configFlags :: ConfigFlags
  , configExFlags :: ConfigExFlags
  , installFlags :: InstallFlags
  , haddockFlags :: HaddockFlags
  , testFlags :: TestFlags
  , benchmarkFlags :: BenchmarkFlags
  , projectFlags :: ProjectFlags
  , extraFlags :: a
  }

nixStyleOptions
  :: (ShowOrParseArgs -> [OptionField a])
  -> ShowOrParseArgs
  -> [OptionField (NixStyleFlags a)]
nixStyleOptions commandOptions showOrParseArgs =
  liftOptions
    configFlags
    set1
    -- Note: [Hidden Flags]
    -- We reuse the configure options from v1 commands which on their turn
    -- reuse the ones from Cabal) but we hide some of them in v2 commands.
    ( filter
        ( ( `notElem`
              [ "cabal-file"
              , "constraint"
              , "dependency"
              , "promised-dependency"
              , "exact-configuration"
              ]
          )
            . optionName
        )
        $ configureOptions showOrParseArgs
    )
    ++ liftOptions
      configExFlags
      set2
      ( configureExOptions
          showOrParseArgs
          ConstraintSourceCommandlineFlag
      )
    ++ liftOptions
      installFlags
      set3
      -- hide "target-package-db" and "symlink-bindir" flags from the
      -- install options.
      -- "symlink-bindir" is obsoleted by "installdir" in ClientInstallFlags
      ( filter
          ( (`notElem` ["target-package-db", "symlink-bindir"])
              . optionName
          )
          $ installOptions showOrParseArgs
      )
    ++ liftOptions
      haddockFlags
      set4
      -- hide "verbose" and "builddir" flags from the
      -- haddock options.
      ( filter
          ( (`notElem` ["v", "verbose", "builddir"])
              . optionName
          )
          $ haddockOptions showOrParseArgs
      )
    ++ liftOptions testFlags set5 (testOptions showOrParseArgs)
    ++ liftOptions benchmarkFlags set6 (benchmarkOptions showOrParseArgs)
    ++ liftOptions projectFlags set7 (projectFlagsOptions showOrParseArgs)
    ++ liftOptions extraFlags set8 (commandOptions showOrParseArgs)
  where
    set1 x flags = flags{configFlags = x}
    set2 x flags = flags{configExFlags = x}
    set3 x flags = flags{installFlags = x}
    set4 x flags = flags{haddockFlags = x}
    set5 x flags = flags{testFlags = x}
    set6 x flags = flags{benchmarkFlags = x}
    set7 x flags = flags{projectFlags = x}
    set8 x flags = flags{extraFlags = x}

defaultNixStyleFlags :: a -> NixStyleFlags a
defaultNixStyleFlags x =
  NixStyleFlags
    { configFlags = mempty
    , configExFlags = mempty
    , installFlags = mempty
    , haddockFlags = mempty
    , testFlags = mempty
    , benchmarkFlags = mempty
    , projectFlags = defaultProjectFlags
    , extraFlags = x
    }

updNixStyleCommonSetupFlags
  :: (CommonSetupFlags -> CommonSetupFlags)
  -> NixStyleFlags a
  -> NixStyleFlags a
updNixStyleCommonSetupFlags setFlag nixFlags =
  nixFlags
    { configFlags =
        let flags = configFlags nixFlags
            common = configCommonFlags flags
         in flags{configCommonFlags = setFlag common}
    , haddockFlags =
        let flags = haddockFlags nixFlags
            common = haddockCommonFlags flags
         in flags{haddockCommonFlags = setFlag common}
    , testFlags =
        let flags = testFlags nixFlags
            common = testCommonFlags flags
         in flags{testCommonFlags = setFlag common}
    , benchmarkFlags =
        let flags = benchmarkFlags nixFlags
            common = benchmarkCommonFlags flags
         in flags{benchmarkCommonFlags = setFlag common}
    }

cfgVerbosity :: VerbosityFlags -> NixStyleFlags a -> Verbosity
cfgVerbosity v flags =
  mkVerbosity defaultVerbosityHandles $
    fromFlagOrDefault v (setupVerbosity . configCommonFlags $ configFlags flags)

keepUnsupportedOptions :: OptionField a -> Bool
keepUnsupportedOptions (optionName -> o) = "root-cmd" == o || "allow-boot-library-installs" == o

keepInstallOptions :: OptionField a -> Bool
keepInstallOptions (optionName -> o) =
  "dir" `isSuffixOf` o
    || "reinstall" `isInfixOf` o
    || "run-tests" == o
    || "root-cmd" == o
    || "allow-boot-library-installs" == o
    || "program-prefix" == o
    || "program-suffix" == o
    || "ipid" == o
    || "cid" == o
    || "user" == o
    || "global" == o
    || "prefix" == o

keepIrrelevantOptions :: OptionField a -> Bool
keepIrrelevantOptions (optionName -> o) = "per-component" `isSuffixOf` o

keepHaddockOptions :: OptionField a -> Bool
keepHaddockOptions (optionName -> o) =
  "haddock" `isPrefixOf` o
    || "documentation" `isSuffixOf` o
    || "doc-index-file" == o

keepTestOptions :: OptionField a -> Bool
keepTestOptions (optionName -> o) = "test" `isPrefixOf` o

keepBenchOptions :: OptionField a -> Bool
keepBenchOptions (optionName -> o) = "bench" `isPrefixOf` o

keepProfilingOptions :: OptionField a -> Bool
keepProfilingOptions (optionName -> o) = "profiling" `isInfixOf` o

keepSolvingOptions :: OptionField a -> Bool
keepSolvingOptions (optionName -> o) =
  "max-backjumps" == o
    || "conflicts" `isInfixOf` o
    || "goals" `isInfixOf` o
    || "index-state" == o
    || "upgrade-dependencies" == o
    || "reject-unconstrained-dependencies" == o
    || "prefer-oldest" == o
    || "allow-older" == o
    || "allow-newer" == o
    || "preference" == o
    || "shadow-installed-packages" == o
    || "ignore-build-tools" == o
    || "solver" == o
    || "only-dependencies" == o
    || "dependencies-only" == o
    || "minimize-conflict-set" == o
    || "allow-depending-on-private-libs" == o

keepExeOptions :: OptionField a -> Bool
keepExeOptions (optionName -> o) =
  "executable" `isInfixOf` o
    || "split" `isInfixOf` o
    || "stripping" `isInfixOf` o

keepLibOptions :: OptionField a -> Bool
keepLibOptions (optionName -> o) =
  "vanilla" `isSuffixOf` o
    || "shared" `isSuffixOf` o
    || "static" `isSuffixOf` o
    || "bytecode" `isSuffixOf` o
    || "ghci" `isSuffixOf` o

keepCoverageOptions :: OptionField a -> Bool
keepCoverageOptions (optionName -> o) =
  "coverage" `isSuffixOf` o
    || "coverage" `isPrefixOf` o

keepOutputOptions :: OptionField a -> Bool
keepOutputOptions (optionName -> o) =
  "build-info" `isSuffixOf` o
    || "debug-info" `isSuffixOf` o
    || "deterministic" `isSuffixOf` o
    || "relocatable" `isSuffixOf` o
    || "write-ghc-environment-files" == o

keepConfigureOptions :: OptionField a -> Bool
keepConfigureOptions (optionName -> o) =
  "append" `isSuffixOf` o
    || "backup" `isSuffixOf` o
    || "configure-option" == o

keepPhaseOptions :: OptionField a -> Bool
keepPhaseOptions (optionName -> o) =
  "only-configure" == o
    || "only-download" == o
    || "dry-run" == o

keepCompilerOptions :: OptionField a -> Bool
keepCompilerOptions (optionName -> o) =
  "ghc" == o
    || "ghcjs" == o
    || "uhc" == o
    || "with-compiler" == o
    || "cabal-lib-version" == o
    || "optimization" `isSuffixOf` o
    || "semaphore" == o
    || "jobs" == o
    || "keep-going" == o
    || "offline" == o

keepLoggingOptions :: OptionField a -> Bool
keepLoggingOptions (optionName -> o) =
  "verbose" == o
    || "keep-temp-files" == o
    || "build-summary" == o
    || "build-log" == o
    || "build-timings" == o
    || "remote-build-reporting" == o
    || "report-planning-failure" == o

keepIncludeOptions :: OptionField a -> Bool
keepIncludeOptions (optionName -> o) =
  "extra-include-dirs" == o
    || "extra-lib-dirs" == o
    || "extra-framework-dirs" == o
    || "extra-prog-path" == o
    || "disable-response-files" == o

keepProgOptions :: OptionField a -> Bool
keepProgOptions (optionName -> o) =
  "with-PROG" == o
    || "PROG-option" `isPrefixOf` o
