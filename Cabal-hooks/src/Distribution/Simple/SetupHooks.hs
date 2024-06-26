{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Distribution.Simple.SetupHooks
Description: Interface for the @Hooks@ @build-type@.

This module defines the interface for the @Hooks@ @build-type@.

To write a package that implements @build-type: Hooks@, you should define
a module @SetupHooks.hs@ which exports a value @setupHooks :: 'SetupHooks'@.
This is a record that declares actions that should be hooked into the
cabal build process.

See 'SetupHooks' for more details.
-}
module Distribution.Simple.SetupHooks
  ( -- * Hooks

    -- $setupHooks
    SetupHooks(..)
  , noSetupHooks

     -- * Configure hooks

     -- $configureHooks
  , ConfigureHooks(..)
  , noConfigureHooks
    -- ** Per-package configure hooks
  , PreConfPackageInputs(..)
  , PreConfPackageOutputs(..) -- See Note [Not hiding SetupHooks constructors]
  , noPreConfPackageOutputs
  , PreConfPackageHook
  , PostConfPackageInputs(..)
  , PostConfPackageHook
    -- ** Per-component configure hooks
  , PreConfComponentInputs(..)
  , PreConfComponentOutputs(..) -- See Note [Not hiding SetupHooks constructors]
  , noPreConfComponentOutputs
  , PreConfComponentHook
  , ComponentDiff(..), emptyComponentDiff, buildInfoComponentDiff
  , LibraryDiff, ForeignLibDiff, ExecutableDiff
  , TestSuiteDiff, BenchmarkDiff
  , BuildInfoDiff

    -- * Build hooks

  , BuildHooks(..), noBuildHooks
  , BuildingWhat(..), buildingWhatVerbosity, buildingWhatDistPref

    -- ** Pre-build rules

    -- $preBuildRules
  , PreBuildComponentInputs(..)
  , PreBuildComponentRules

    -- ** Post-build hooks
  , PostBuildComponentInputs(..)
  , PostBuildComponentHook

    -- ** Rules
  , Rules
  , rules
  , noRules
  , Rule
  , Dependency (..)
  , RuleOutput (..)
  , RuleId
  , staticRule, dynamicRule
    -- *** Rule inputs/outputs

    -- $rulesDemand
  , Location(..)
  , location
  , autogenComponentModulesDir
  , componentBuildDir

    -- *** Actions
  , RuleCommands(..)
  , Command
  , mkCommand
  , Dict(..)

    -- *** Rules API

    -- $rulesAPI
  , RulesM
  , registerRule
  , registerRule_

  -- **** File/directory monitoring
  , addRuleMonitors
  , module Distribution.Simple.FileMonitor.Types

    -- * Install hooks
  , InstallHooks(..), noInstallHooks
  , InstallComponentInputs(..), InstallComponentHook

    -- * Re-exports

    -- ** Hooks
    -- *** Configure hooks
  , ConfigFlags(..)
    -- *** Build hooks
  , BuildFlags(..), ReplFlags(..), HaddockFlags(..), HscolourFlags(..)
    -- *** Install hooks
  , CopyFlags(..)

    -- ** @Hooks@ API
    --
    -- | These are functions provided as part of the @Hooks@ API.
    -- It is recommended to import them from this module as opposed to
    -- manually importing them from inside the Cabal module hierarchy.

    -- *** Copy/install functions
  , installFileGlob

    -- *** Interacting with the program database
  , Program(..), ConfiguredProgram(..), ProgArg
  , ProgramLocation(..)
  , ProgramDb
  , addKnownPrograms
  , configureUnconfiguredProgram
  , simpleProgram

    -- ** General @Cabal@ datatypes
  , Verbosity, Compiler(..), Platform(..), Suffix(..)

    -- *** Package information
  , LocalBuildConfig, LocalBuildInfo, PackageBuildDescr
      -- NB: we can't simply re-export all the fields of LocalBuildConfig etc,
      -- due to the presence of duplicate record fields.
      -- Ideally, we'd like to e.g. re-export LocalBuildConfig qualified,
      -- but qualified re-exports aren't a thing currently.

  , PackageDescription(..)

    -- *** Component information
  , Component(..), ComponentName(..), componentName
  , BuildInfo(..), emptyBuildInfo
  , TargetInfo(..), ComponentLocalBuildInfo(..)

    -- **** Components
  , Library(..), ForeignLib(..), Executable(..)
  , TestSuite(..), Benchmark(..)
  , LibraryName(..)
  , emptyLibrary, emptyForeignLib, emptyExecutable
  , emptyTestSuite, emptyBenchmark

  )
where
import Distribution.PackageDescription
  ( PackageDescription(..)
  , Library(..), ForeignLib(..)
  , Executable(..), TestSuite(..), Benchmark(..)
  , emptyLibrary, emptyForeignLib
  , emptyExecutable, emptyBenchmark, emptyTestSuite
  , BuildInfo(..), emptyBuildInfo
  , ComponentName(..), LibraryName(..)
  )
import Distribution.Simple.BuildPaths
  ( autogenComponentModulesDir )
import Distribution.Simple.Compiler
  ( Compiler(..) )
import Distribution.Simple.Errors
  ( CabalException(SetupHooksException) )
import Distribution.Simple.FileMonitor.Types
import Distribution.Simple.Install
  ( installFileGlob )
import Distribution.Simple.LocalBuildInfo
  ( componentBuildDir )
import Distribution.Simple.PreProcess.Types
  ( Suffix(..) )
import Distribution.Simple.Program.Db
  ( ProgramDb, addKnownPrograms
  , configureUnconfiguredProgram
  )
import Distribution.Simple.Program.Find
  ( simpleProgram )
import Distribution.Simple.Program.Types
  ( Program(..), ConfiguredProgram(..)
  , ProgArg
  , ProgramLocation(..)
  )
import Distribution.Simple.Setup
  ( BuildFlags(..)
  , ConfigFlags(..)
  , CopyFlags(..)
  , HaddockFlags(..)
  , HscolourFlags(..)
  , ReplFlags(..)
  )
import Distribution.Simple.SetupHooks.Errors
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule as Rule
import Distribution.Simple.Utils
  ( dieWithException )
import Distribution.System
  ( Platform(..) )
import Distribution.Types.Component
  ( Component(..), componentName )
import Distribution.Types.ComponentLocalBuildInfo
  ( ComponentLocalBuildInfo(..) )
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo(..) )
import Distribution.Types.LocalBuildConfig
  ( LocalBuildConfig, PackageBuildDescr )
import Distribution.Types.TargetInfo
  ( TargetInfo(..) )
import Distribution.Utils.ShortText
  ( ShortText )
import Distribution.Verbosity
  ( Verbosity )

import Control.Monad
  ( void )
import Control.Monad.IO.Class
  ( MonadIO(liftIO) )
import Control.Monad.Trans.Class
  ( lift )
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as Writer
#else
import qualified Control.Monad.Trans.Writer.Strict as Writer
#endif
import Data.Foldable
  ( for_ )
import Data.Map.Strict as Map
  ( insertLookupWithKey )

--------------------------------------------------------------------------------
-- Haddocks for the SetupHooks API

{- $setupHooks
A Cabal package with @Hooks@ @build-type@ must define the Haskell module
@SetupHooks@ which defines a value @setupHooks :: 'SetupHooks'@.

These *setup hooks* allow package authors to customise the configuration and
building of a package by providing certain hooks that get folded into the
general package configuration and building logic within @Cabal@.

This mechanism replaces the @Custom@ @build-type@, providing better
integration with the rest of the Haskell ecosystem.

Usage example:

> -- In your .cabal file
> build-type: Hooks
>
> custom-setup
>   setup-depends:
>     base        >= 4.18 && < 5,
>     Cabal-hooks >= 0.1  && < 0.2
>
> The declared Cabal version should also be at least 3.14.

> -- In SetupHooks.hs, next to your .cabal file
> module SetupHooks where
> import Distribution.Simple.SetupHooks ( SetupHooks, noSetupHooks )
>
> setupHooks :: SetupHooks
> setupHooks =
>  noSetupHooks
>    { configureHooks = myConfigureHooks
>    , buildHooks = myBuildHooks }

Note that 'SetupHooks' can be monoidally combined, e.g.:

> module SetupHooks where
> import Distribution.Simple.SetupHooks
> import qualified SomeOtherLibrary ( setupHooks )
>
> setupHooks :: SetupHooks
> setupHooks = SomeOtherLibrary.setupHooks <> mySetupHooks
>
> mySetupHooks :: SetupHooks
> mySetupHooks = ...
-}

{- $configureHooks
Configure hooks can be used to augment the Cabal configure logic with
package-specific logic. The main principle is that the configure hooks can
feed into updating the 'PackageDescription' of a @cabal@ package. From then on,
this package configuration is set in stone, and later hooks (e.g. hooks into
the build phase) can no longer modify this configuration; instead they will
receive this configuration in their inputs, and must honour it.

Configuration happens at two levels:

  * global configuration covers the entire package,
  * local configuration covers a single component.

Once the global package configuration is done, all hooks work on a
per-component level. The configuration hooks thus follow a simple philosophy:

  * All modifications to global package options must use `preConfPackageHook`.
  * All modifications to component configuration options must use `preConfComponentHook`.

For example, to generate modules inside a given component, you should:

  * In the per-component configure hook, declare the modules you are going to
    generate by adding them to the `autogenModules` field for that component
    (unless you know them ahead of time, in which case they can be listed
    textually in the @.cabal@ file of the project).
  * In the build hooks, describe the actions that will generate these modules.
-}

{- $preBuildRules
Pre-build hooks are specified as a collection of pre-build 'Rules'.
Each t'Rule' consists of:

  - a specification of its static dependencies and outputs,
  - the commands that execute the rule.

Rules are constructed using either one of the 'staticRule' or 'dynamicRule'
smart constructors. Directly constructing a t'Rule' using the constructors of
that data type is not advised, as this relies on internal implementation details
which are subject to change in between versions of the `Cabal-hooks` library.

Note that:

  - To declare the dependency on the output of a rule, one must refer to the
    rule directly, and not to the path to the output executing that rule will
    eventually produce.
    To do so, registering a t'Rule' with the API returns a unique identifier
    for that rule, in the form of a t'RuleId'.
  - File dependencies and outputs are not specified directly by
    'FilePath', but rather use the 'Location' type (which is more convenient
    when working with preprocessors).
  - Rules refer to the actions that execute them using static pointers, in order
    to enable serialisation/deserialisation of rules.
  - Rules can additionally monitor files or directories, which determines
    when to re-compute the entire set of rules.
-}

{- $rulesDemand
Rules can declare various kinds of dependencies:

  - 'staticDependencies': files or other rules that a rule statically depends on,
  - extra dynamic dependencies, using the 'DynamicRuleCommands' constructor,
  - 'MonitorFilePath': additional files and directories to monitor.

Rules are considered __out-of-date__ precisely when any of the following
conditions apply:

  [O1] there has been a (relevant) change in the files and directories
       monitored by the rules,
  [O2] the environment passed to the computation of rules has changed.

If the rules are out-of-date, the build system is expected to re-run the
computation that computes all rules.

After this re-computation of the set of all rules, we match up new rules
with old rules, by 'RuleId'. A rule is then considered __stale__ if any of
following conditions apply:

  [N] the rule is new, or
  [S] the rule matches with an old rule, and either:

    [S1] a file dependency of the rule has been modified/created/deleted, or
         a (transitive) rule dependency of the rule is itself stale, or
    [S2] the rule is different from the old rule, e.g. the argument stored in
         the rule command has changed, or the pointer to the action to run the
         rule has changed. (This is determined using the @Eq Rule@ instance.)

A stale rule becomes no longer stale once we run its associated action. The
build system is responsible for re-running the actions associated with
each stale rule, in dependency order. This means the build system is expected
to behave as follows:

  1. Any time the rules are out-of-date, query the rules to obtain
     up-to-date rules.
  2. Re-run stale rules.
-}

{- $rulesAPI
Defining pre-build rules can be done in the following style:

> {-# LANGUAGE BlockArguments, StaticPointers #-}
> myPreBuildRules :: PreBuildComponentRules
> myPreBuildRules = rules (static ()) $ \ preBuildEnvironment -> do
>   let cmd1 = mkCommand (static Dict) $ static \ arg -> do { .. }
>       cmd2 = mkCommand (static Dict) $ static \ arg -> do { .. }
>   myData <- liftIO someIOAction
>   addRuleMonitors [ monitorDirectory "someSearchDir" ]
>   registerRule_ "rule_1_1" $ staticRule (cmd1 arg1) deps1 outs1
>   registerRule_ "rule_1_2" $ staticRule (cmd1 arg2) deps2 outs2
>   registerRule_ "rule_1_3" $ staticRule (cmd1 arg3) deps3 outs3
>   registerRule_ "rule_2_4" $ staticRule (cmd2 arg4) deps4 outs4

Here we use the 'rules', 'staticRule' and 'mkCommand' smart constructors,
rather than directly using the v'Rules', v'Rule' and v'Command' constructors,
which insulates us from internal changes to the t'Rules', t'Rule' and t'Command'
datatypes, respectively.

We use 'addRuleMonitors' to declare a monitored directory that the collection
of rules as a whole depends on. In this case, we declare that they depend on the
contents of the "searchDir" directory. This means that the rules will be
computed anew whenever the contents of this directory change.
-}

{- Note [Not hiding SetupHooks constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We would like to hide as many datatype constructors from the API as possible
and provide smart constructors instead, so that hook authors don't end up
depending on internal implementation details that are subject to change.

However, doing so significantly degrades the Haddock documentation. So we
instead opt for exposing the constructor, but suggesting users use the
corresponding smart constructor instead.
-}

--------------------------------------------------------------------------------
-- API functions

-- | Register a rule. Returns an identifier for that rule.
registerRule
  :: ShortText -- ^ user-given rule name;
               -- these should be unique on a per-package level
  -> Rule      -- ^ the rule to register
  -> RulesM RuleId
registerRule nm !newRule = RulesT $ do
  RulesEnv { rulesEnvNameSpace = ns
           , rulesEnvVerbosity = verbosity } <- Reader.ask
  oldRules <- lift $ State.get
  let rId = RuleId { ruleNameSpace = ns, ruleName = nm }
      (mbDup, newRules) = Map.insertLookupWithKey (\ _ new _old -> new) rId newRule oldRules
  for_ mbDup $ \ oldRule ->
    liftIO $ dieWithException verbosity
           $ SetupHooksException
           $ RulesException
           $ DuplicateRuleId rId oldRule newRule
  lift $ State.put newRules
  return rId

-- | Register a rule, discarding the produced 'RuleId'.
--
-- Using this function means that you don't expect any other rules to ever
-- depend on any outputs of this rule. Use 'registerRule' to retain the
-- 'RuleId' instead.
registerRule_
  :: ShortText -- ^ user-given rule name;
               -- these should be unique on a per-package level
  -> Rule      -- ^ the rule to register
  -> RulesT IO ()
registerRule_ i r = void $ registerRule i r

-- | Declare additional monitored objects for the collection of all rules.
--
-- When these monitored objects change, the rules are re-computed.
addRuleMonitors :: Monad m => [MonitorFilePath] -> RulesT m ()
addRuleMonitors = RulesT . lift . lift . Writer.tell
{-# INLINEABLE addRuleMonitors #-}

-- TODO: add API functions that search and declare the appropriate monitoring
-- at the same time.
