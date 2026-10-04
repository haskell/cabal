-- | cabal-install CLI command: buck2
--
-- Sets up (or refreshes) a buck2 build for the current project, using the
-- prelude and support scripts checked out at @buck2\/@ (a checkout of
-- <https://github.com/simonmar/haskell-buck2>, see @buck2\/README.md@):
--
--   1. Build every dependency (never the local packages themselves), the
--      same as @cabal build all --only-dependencies@ would - see
--      "Distribution.Client.Buck2.BuildDependencies".
--   2. Create @.buckconfig@\/@PACKAGE@ if they don't exist yet (copied
--      verbatim from @buck2\/example@).
--   3. Turn the resolved dependency closure into @third-party\/haskell@ -
--      see "Distribution.Client.Buck2.Prebuilt".
--   4. Configure every component of the local packages, to get the
--      'LocalBuildInfo' they are generated from - see
--      "Distribution.Client.Buck2.Configure".
--   5. Generate a @BUCK.cabal.bzl@ (and, where missing, a @BUCK@) for
--      every local package - see "Distribution.Client.Buck2.Write".
module Distribution.Client.CmdBuck2
  ( buck2Command
  , buck2Action
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.DistDirLayout (DistDirLayout (distProjectRootDirectory))
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( GlobalFlags
  , InstallFlags (installOnlyDeps)
  )

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Command (CommandUI (..), usageAlternatives)
import Distribution.Simple.Flag (toFlag)
import Distribution.Simple.Utils (dieWithException, notice)
import Distribution.Verbosity (normal)

import Distribution.Client.Buck2.BuildDependencies (buildDependencies)
import Distribution.Client.Buck2.Configure (configureComponents)
import Distribution.Client.Buck2.Write (writeAllPackages)
import Distribution.Client.Buck2.LocalPackages
  ( builtLocalPackages
  , projectTestOptions
  , wantedBuildTools
  )
import Distribution.Client.Buck2.Prebuilt (generatePrebuilt)
import Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  )
import Distribution.Client.Errors (CabalInstallException (Buck2ActionExtraArgs))

buck2Command :: CommandUI (NixStyleFlags ())
buck2Command =
  CommandUI
    { commandName = "buck2"
    , commandSynopsis = "Set up (or refresh) a buck2 build for this project."
    , commandUsage = usageAlternatives "buck2" ["[FLAGS]"]
    , commandDescription = Just $ \_ ->
        "Builds every dependency of the project (as `cabal build all "
          ++ "--only-dependencies` would), then generates the buck2 build "
          ++ "files (.buckconfig, PACKAGE, third-party/haskell, and a "
          ++ "BUCK.cabal.bzl for each local package) needed to build the "
          ++ "project with buck2 instead of cabal. Requires a checkout of "
          ++ "https://github.com/simonmar/haskell-buck2 at ./buck2. See "
          ++ "buck2/README.md for details.\n\n"
          ++ "Flags that would normally be passed to `cabal build`/`cabal "
          ++ "configure` (-f, --enable-profiling, --enable-tests, etc.) are "
          ++ "honoured here too, and apply to the dependency build."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

buck2Action :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
buck2Action flags extraArgs globalFlags = do
  unless (null extraArgs) $
    dieWithException verbosity (Buck2ActionExtraArgs extraArgs)

  withContextAndSelectors verbosity RejectNoTargets Nothing depsFlags ["all"] globalFlags BuildCommand $
    \targetCtx ctx targetSelectors -> do
      baseCtx <- case targetCtx of
        ProjectContext -> return ctx
        GlobalContext -> return ctx
        ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

      let projectRoot = distProjectRootDirectory (distDirLayout baseCtx)
      checkBuck2Prelude verbosity projectRoot

      buildCtx <- buildDependencies verbosity baseCtx targetSelectors

      ensureBuckconfigAndPackage verbosity projectRoot

      localPkgs <- builtLocalPackages verbosity (distDirLayout baseCtx) (elaboratedPlanOriginal buildCtx)

      (externalBuildTools, resolvedDeps) <-
        generatePrebuilt
          verbosity
          projectRoot
          (cabalDirLayout baseCtx)
          (elaboratedShared buildCtx)
          (elaboratedPlanToExecute buildCtx)
          (wantedBuildTools localPkgs)

      -- 'generatePrebuilt' already found and parsed every real @.conf@
      -- file of the resolved dependency closure.
      componentLBIs <- configureComponents verbosity baseCtx buildCtx (PackageIndex.fromList resolvedDeps)

      writeAllPackages
        verbosity
        projectRoot
        componentLBIs
        externalBuildTools
        (projectTestOptions (elaboratedPlanOriginal buildCtx))
        localPkgs

      notice verbosity $
        unlines
          [ "cabal buck2: done. You can now:"
          , "    buck2 build //...          # build everything"
          , "    buck2 test //...           # test everything"
          , "    buck2 build //... -m opt   # build everything in opt mode"
          ]
  where
    verbosity = cfgVerbosity normal flags
    depsFlags = flags{installFlags = (installFlags flags){installOnlyDeps = toFlag True}}
