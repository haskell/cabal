{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StaticPointers #-}

{-|
Module: Distribution.Simple.SetupHooks
Description: Interface for the @Hooks@ @build-type@.

This module defines the interface for the @Hooks@ @build-type@.

To write a package that implements @build-type: Hooks@, you should define
a module @SetupHooks.hs@ which exports a value @setupHooks :: 'SetupHooks'@.
This is a record that declares actions that should be hooked into the
cabal build process.

See 'SetupHooks' for more details, as well as the
[introductory blog post](https://well-typed.com/blog/2025/01/cabal-hooks) for
the feature.
-}
module Distribution.Simple.SetupHooks
  ( -- * Hooks

    -- $setupHooks
    SetupHooks(..)
  , noSetupHooks

     -- * Usage overview
     -- $usage

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

    -- *** Rules API

    -- $rulesAPI
  , RulesM
  , registerRule
  , registerRule_

    -- *** Rule construction
  , Rule
  , staticRule, dynamicRule

    -- **** Example usage of 'dynamicRule'
    -- $dynamicRules

    -- *** Rule inputs/outputs

    -- **** Rule dependencies
    -- $rulesDemand
  , Dependency (..)
  , RuleOutput (..)
  , RuleId

    -- **** Filesystem locations
  , Location(..)
  , location
  , autogenComponentModulesDir
  , componentBuildDir

    -- *** Actions
  , RuleCommands -- gnarly constructors not exposed; API is via 'staticRule' and 'dynamicRule'
  , Command
  , mkCommand
  , Dict(..)


  -- *** File/directory monitoring
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
import qualified Control.Monad.Trans.Writer.CPS as Writer
import Data.Foldable
  ( for_ )
import Data.Map.Strict as Map
  ( insertLookupWithKey )

--------------------------------------------------------------------------------
-- Haddocks for the SetupHooks API

{- $setupHooks
A Cabal package with @Hooks@ @build-type@ must define the Haskell module
@SetupHooks@ which defines a value @setupHooks :: 'SetupHooks'@.

These __setup hooks__ allow package authors to customise the configuration and
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
>     Cabal-hooks >= 3.14 && < 3.15
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

{- $usage
'SetupHooks' allow hooks into the following phases:

  * The configure phase, via 'ConfigureHooks'.

      There are three hooks into the configure phase:

        * Package-wide pre-configure hook.

            This hook enables custom logic in the style of traditional @./configure@
            scripts, e.g. finding out information about the system and configuring
            dependencies.

        * Package-wide post-configure hook.

            This hook is mostly used to write write custom package-wide information
            to disk so that the next step can read it without repeating work.

        * Per-component pre-configure hooks.

            These hooks are used to modify individual components, e.g. declaring
            new modules (such as autogenerated modules whose module names are not
            statically known) or specifying per-component flags to be used when
            building each component.

            You can think of this step as dynamically updating the stanza for a
            single component in the .cabal file of a package.

  * The build phase, via 'BuildHooks'.

      There are two hooks into the build phase:

        * Per-component pre-build rules.

            A rule can be thought of an invocation of a code generator; each rule
            specifies how to build a particular output.

            You can have rules that don't depend on any inputs (e.g. directly
            generate a collection of modules programmatically, perhaps from some
            kind of parsed schema), as well as preprocessor-like rules that take in
            input files, e.g. the Happy parser generator that takes in a .y
            file and generates a corresponding .hs file.

            This rather elaborate setup (compared to a one-shot program that
            generates the required modules) allows proper recompilation checking.
            See also <https://well-typed.com/blog/2025/01/cabal-hooks/#pre-build-rules>
            for some worked examples of pre-build rules.

        * Per-component post-build hooks.

            These can be thought of as "pre-linking hooks" and allow injecting
            additional data into the final executable.

  * The install phase, via 'InstallHooks'.

      There is a single, per-component install hook. This allows copying over
      additional files when installing a component (library/executable).
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

You can think of a t'Rule' as describing a particular invocation of a code
generator or preprocessor, with the t'Command' specifying the particular
command that will be executed.

Rules are constructed using either one of the 'staticRule' or 'dynamicRule'
smart constructors. Directly constructing a t'Rule' using the constructors of
that data type is not advised, as this relies on internal implementation details
which are subject to change in between versions of the @Cabal-hooks@ library.

Note that:

  - To declare the dependency on the output of a rule, one must refer to the
    rule directly, and not to the path to the output executing that rule will
    eventually produce.

      This is achieved by using the t'RuleId' returned by 'registerRule', which
      is the unique identifier for that rule.

  - File dependencies and outputs are not specified directly by
    'FilePath', but rather use the 'Location' type (which is more convenient
    when working with preprocessors).
  - Rules refer to the actions that execute them using static pointers, in order
    to enable serialisation/deserialisation of rules.
  - Rules can additionally monitor files or directories, which determines
    when to re-compute the entire set of rules.
-}

{- $dynamicRules
The 'dynamicRule' smart constructor allows specifying rules that have dynamic
dependencies. This is the most complex part of the API, so let's work through
a representative example.

Suppose for example that we have preprocessor for files that may depend on
each other via explicit import statements at the start of the file. To properly
preprocess these, we need two commands:

  1. A "find dependencies" command that parses the header to find the dependencies.
  2. A "run preprocessor" command. This command might need to be passed the
     dependencies as arguments, so the command in (1) should make that information
     available to (2).

This is exactly what 'dynamicRule' allows us to do. The first command is (1),
which returns dynamic dependencies and additional data passed to the second
command, (2).

The rules for such a preprocessor would thus look something like (sketch):

@
ppDynPreBuildRules :: PreBuildComponentInputs -> RulesM ()
ppDynPreBuildRules pbci = mdo -- NB: using RecursiveDo

  -- Scan filesystem for files to preprocess, e.g. with a monitored file glob.
  inputModNms <- ...

  let

    -- (1): the "find dependencies" action
    computeDepsAction
      :: (Map ModuleName RuleId, FilePath)
      -> IO ([Dependency], [ModuleName])
    computeDepsAction (modNmToRuleId, inFile) = do
      src <- readFile inFile
      let dynDeps = parseHeaderImports src
      return
        ( [ RuleDependency $ RuleOutput rId 1
          | dep <- dynDeps
          , let rId = modNmToRuleId Map.! dep ]
        , dynDeps )

    -- (2): the "run preprocessor" action
    runPPAction :: (FilePath, FilePath) -> [ModuleName] -> IO ()
    runPPAction (inFile, outFile) dynDeps = do
      src <- readFile inFile
      let ppResult = preprocessWithDependencies dynDeps src
      writeFile outFile ppResult

    -- Construct a rule with dynamic dependencies using 'dynamicRule'
    mkRule modNm =
      dynamicRule (static Dict)
        (mkCommand (static Dict) (static computeDepsAction) (allRuleIDs, inFile))
        (mkCommand (static Dict) (static runPPAction) (inFile, outFile))
        [ FileDependency $ Location sameDirectory (modPath <.> "myPp") ]
        ( Location autogenDir (modPath <.> "hs" )
        NE.:| [ Location buildDir (unsafeCoerceSymbolicPath modPath <.> "myPpIface") ] )
      where
        modPath = moduleNameSymbolicPath modNm
        inFile  = getSymbolicPath (sameDirectory </> modPath <.> "myPp")
        outFile = getSymbolicPath (autogenDir    </> modPath <.> "hs")

    autogenDir = ... -- derived from pbci
    buildDir   = ... -- derived from pbci

    registerOne :: ModuleName -> RulesM (ModuleName, RuleId)
    registerOne modNm = do
      rId <- registerRule ("MyPP: " <> show modNm) (mkRule modNm)
      return (modNm, rId)

  -- Return a map from ModuleName to RuleId (used above via RecursiveDo).
  allRuleIDs <- Map.fromList <$> traverse registerOne inputModNms
  return ()
@

Note the usage of @RecursiveDo@ to allow indexing into the set of all registered
rules in order to declare the dynamic dependencies of the rules.

In this example we use tuples for the command arguments and @[ModuleName]@ for
the result of the dynamic dependency command; these have the required instances
needed for serialisation. If you use custom datatypes for these, you will need
to derive @Binary@, @Show@, @Eq@ to satisfy the API requirements (enforced by
the various calls to @static Dict@).

-}

{- $rulesDemand
Rules can declare various kinds of dependencies:

  - static dependencies: files or other rules that a rule statically depends on,
  - extra dynamic dependencies, using 'dynamicRule' smart constructor,
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

    [S1] a file dependency of the rule has been modified\/created\/deleted,
         or a (transitive) rule dependency of the rule is itself stale, or
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
computed anew whenever the contents of this directory change. (This does not
mean all the rules will be re-run; only the out-of-date rules will be re-run.)
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
  oldRules <- lift State.get
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
