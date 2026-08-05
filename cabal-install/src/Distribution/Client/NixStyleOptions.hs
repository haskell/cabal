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
  , removeUnsupportedOptions
  , removeInstallOptions
  , removeIrrelevantOptions
  , removeHaddockOptions
  , removeTestOptions
  , removeBenchOptions
  , removeProfilingOptions
  , removeSolvingOptions
  , removeExeOptions
  , removeLibOptions
  , removeCoverageOptions
  , removeOutputOptions
  , removeConfigureOptions
  , removePhaseOptions
  , removeCompilerOptions
  , removeLoggingOptions
  , removeIncludeOptions
  , removeProgOptions
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

removeUnsupportedOptions :: OptionField a -> Bool
removeUnsupportedOptions (optionName -> o) = not ("root-cmd" == o || "allow-boot-library-installs" == o)

removeInstallOptions :: OptionField a -> Bool
removeInstallOptions (optionName -> o) =
  not
    ( "dir" `isSuffixOf` o
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
    )

removeIrrelevantOptions :: OptionField a -> Bool
removeIrrelevantOptions (optionName -> o) = not ("per-component" `isSuffixOf` o)

removeHaddockOptions :: OptionField a -> Bool
removeHaddockOptions (optionName -> o) =
  not
    ( "haddock" `isPrefixOf` o
        || "documentation" `isSuffixOf` o
        || "doc-index-file" == o
    )

removeTestOptions :: OptionField a -> Bool
removeTestOptions (optionName -> o) = not ("test" `isPrefixOf` o)

removeBenchOptions :: OptionField a -> Bool
removeBenchOptions (optionName -> o) = not ("bench" `isPrefixOf` o)

removeProfilingOptions :: OptionField a -> Bool
removeProfilingOptions (optionName -> o) = not ("profiling" `isInfixOf` o)

removeSolvingOptions :: OptionField a -> Bool
removeSolvingOptions (optionName -> o) =
  not
    ( "max-backjumps" == o
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
    )

removeExeOptions :: OptionField a -> Bool
removeExeOptions (optionName -> o) =
  not
    ( "executable" `isInfixOf` o
        || "split" `isInfixOf` o
        || "stripping" `isInfixOf` o
    )

removeLibOptions :: OptionField a -> Bool
removeLibOptions (optionName -> o) =
  not
    ( "vanilla" `isSuffixOf` o
        || "shared" `isSuffixOf` o
        || "static" `isSuffixOf` o
        || "bytecode" `isSuffixOf` o
        || "ghci" `isSuffixOf` o
    )

removeCoverageOptions :: OptionField a -> Bool
removeCoverageOptions (optionName -> o) =
  not
    ( "coverage" `isSuffixOf` o
        || "coverage" `isPrefixOf` o
    )

removeOutputOptions :: OptionField a -> Bool
removeOutputOptions (optionName -> o) =
  not
    ( "build-info" `isSuffixOf` o
        || "debug-info" `isSuffixOf` o
        || "deterministic" `isSuffixOf` o
        || "relocatable" `isSuffixOf` o
        || "write-ghc-environment-files" == o
    )

removeConfigureOptions :: OptionField a -> Bool
removeConfigureOptions (optionName -> o) =
  not
    ( "append" `isSuffixOf` o
        || "backup" `isSuffixOf` o
        || "configure-option" == o
    )

removePhaseOptions :: OptionField a -> Bool
removePhaseOptions (optionName -> o) =
  not
    ( "only-configure" == o
        || "only-download" == o
        || "dry-run" == o
    )

removeCompilerOptions :: OptionField a -> Bool
removeCompilerOptions (optionName -> o) =
  not
    ( "ghc" == o
        || "ghcjs" == o
        || "uhc" == o
        || "with-compiler" == o
        || "cabal-lib-version" == o
        || "optimization" `isSuffixOf` o
        || "semaphore" == o
        || "jobs" == o
        || "keep-going" == o
        || "offline" == o
    )

removeLoggingOptions :: OptionField a -> Bool
removeLoggingOptions (optionName -> o) =
  not
    ( "verbose" == o
        || "keep-temp-files" == o
        || "build-summary" == o
        || "build-log" == o
        || "build-timings" == o
        || "remote-build-reporting" == o
        || "report-planning-failure" == o
    )

removeIncludeOptions :: OptionField a -> Bool
removeIncludeOptions (optionName -> o) =
  not
    ( "extra-include-dirs" == o
        || "extra-lib-dirs" == o
        || "extra-framework-dirs" == o
        || "extra-prog-path" == o
        || "disable-response-files" == o
    )

removeProgOptions :: OptionField a -> Bool
removeProgOptions (optionName -> o) =
  not
    ( "with-PROG" == o
        || "PROG-option" `isPrefixOf` o
    )
