-- | Command line options for nix-style / v2 commands.
--
-- The commands take a lot of the same options, which affect how install plan
-- is constructed.
module Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , nixStyleOptions
  , configureOptionNames
  , excludedConfigureOptionNames
  , installOptionNames
  , excludedInstallOptionNames
  , isProgramOptionName
  , defaultNixStyleFlags
  , updNixStyleCommonSetupFlags
  , cfgVerbosity
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
  , installDirsOptions
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
    -- We reuse the configure options from v1 commands (which on their turn
    -- reuse the ones from Cabal) but only take the ones named in
    -- 'configureOptionNames'. See 'excludedConfigureOptionNames' for the rest.
    ( selectOptions configureOptionNames isProgramOptionName $
        configureOptions showOrParseArgs
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
      ( selectOptions installOptionNames (const False) $
          installOptions showOrParseArgs
      )
    ++ liftOptions haddockFlags set4 (haddockOptions showOrParseArgs)
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

-- | Keep the options whose name is listed or satisfies the predicate.
selectOptions :: [String] -> (String -> Bool) -> [OptionField a] -> [OptionField a]
selectOptions names p = filter (\o -> optionName o `elem` names || p (optionName o))

-- | The program options of 'configureOptions' are generated for each known
-- program, @--with-PROG@, @--PROG-option@ and @--PROG-options@, so they are
-- matched by shape rather than listed by name. Only "configure-option",
-- "with-compiler" and "with-hc-pkg" share that shape, and those are wanted too.
isProgramOptionName :: String -> Bool
isProgramOptionName name =
  "with-" `isPrefixOf` name
    || "-option" `isSuffixOf` name
    || "-options" `isSuffixOf` name

-- | The 'configureOptions' that nix-style commands take, grouped by what they
-- affect. Together with 'excludedConfigureOptionNames' and
-- 'isProgramOptionName' this covers every option in 'configureOptions', which
-- a unit test checks, so a new v1 option has to be placed in one list or the
-- other before v2 commands accept it.
configureOptionNames :: [String]
configureOptionNames =
  [ -- plan and solver settings
    "verbose"
  , "builddir"
  , "compiler"
  , "with-compiler"
  , "with-hc-pkg"
  , "package-db"
  , "extra-prog-path"
  , "tests"
  , -- benchmark settings
    "benchmarks"
  , -- build-phase settings
    "keep-temp-files"
  , -- per-package build settings
    "program-prefix"
  , "program-suffix"
  , "library-vanilla"
  , "library-profiling"
  , "shared"
  , "static"
  , "library-bytecode"
  , "executable-dynamic"
  , "executable-static"
  , "profiling"
  , "profiling-shared"
  , "executable-profiling"
  , "profiling-detail"
  , "library-profiling-detail"
  , "optimization"
  , "debug-info"
  , "build-info"
  , "library-for-ghci"
  , "split-sections"
  , "split-objs"
  , "executable-stripping"
  , "library-stripping"
  , "configure-option"
  , "flags"
  , "extra-include-dirs"
  , "extra-lib-dirs"
  , "extra-lib-dirs-static"
  , "extra-framework-dirs"
  , "coverage"
  , "library-coverage"
  , "relocatable"
  ]
    ++ map optionName installDirsOptions

-- | The 'configureOptions' that nix-style commands do not take. The first
-- group is set by the planner itself from the install plan; the second is
-- handled by nix-style commands in another way; the third has no meaning for
-- a nix-style build. 'Distribution.Client.ProjectConfig.Legacy' never reads
-- these fields of 'ConfigFlags' into the project configuration. They remain
-- v1 command options and fields of the global config file.
excludedConfigureOptionNames :: [String]
excludedConfigureOptionNames =
  [ -- computed per package by the planner, see elaborateInstallPlan
    "ipid"
  , "cid"
  , "instantiate-with"
  , "deterministic"
  , "response-files"
  , "allow-depending-on-private-libs"
  , "coverage-for"
  , "ignore-build-tools"
  , -- the solver's business: --constraint is taken from 'configureExOptions'
    -- instead, with a constraint source
    "constraint"
  , "dependency"
  , "promised-dependency"
  , "exact-configuration"
  , -- per-package or per-user installs do not exist in nix-style builds
    "user-install"
  , "cabal-file"
  ]

-- | The 'installOptions' that nix-style commands take, grouped by what they
-- affect. See 'excludedInstallOptionNames' for the rest.
installOptionNames :: [String]
installOptionNames =
  [ -- plan and solver settings
    "documentation"
  , "per-component"
  , "max-backjumps"
  , "reorder-goals"
  , "count-conflicts"
  , "fine-grained-conflicts"
  , "minimize-conflict-set"
  , "independent-goals"
  , "prefer-oldest"
  , "prefer-version"
  , "strong-flags"
  , "allow-boot-library-installs"
  , "reject-unconstrained-dependencies"
  , "index-state"
  , -- haddock settings
    "doc-index-file"
  , -- per-package build settings
    "run-tests"
  , -- build-phase settings
    "dry-run"
  , "only-download"
  , "only-dependencies"
  , "dependencies-only"
  , "build-summary"
  , "build-log"
  , "build-timings"
  , "remote-build-reporting"
  , "report-planning-failure"
  , "semaphore"
  , "jobs"
  , "keep-going"
  , "offline"
  , -- read by @install --lib@ only, to overwrite packages already in the
    -- environment file
    "force-reinstalls"
  ]

-- | The 'installOptions' that nix-style commands do not take. They are knobs
-- of the v1 install plan that 'Distribution.Client.ProjectConfig.Legacy' never
-- reads into the project configuration. They remain v1 command options and
-- fields of the global config file.
excludedInstallOptionNames :: [String]
excludedInstallOptionNames =
  [ "reinstall"
  , "avoid-reinstalls"
  , "upgrade-dependencies"
  , "shadow-installed-packages"
  , "root-cmd"
  , "only"
  , -- obsoleted by --installdir in 'ClientInstallFlags'
    "symlink-bindir"
  , "target-package-db"
  ]

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
