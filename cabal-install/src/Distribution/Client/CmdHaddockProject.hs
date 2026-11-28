{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.Client.CmdHaddockProject
  ( haddockProjectCommand
  , haddockProjectAction
  ) where

import Control.Monad (mapM)
import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import qualified Distribution.Client.CmdBuild as CmdBuild
import qualified Distribution.Client.CmdHaddock as CmdHaddock

import qualified Data.Map as Map
import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , StoreDirLayout (..)
  )
import Distribution.Client.InstallPlan (foldPlanPackage)
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.NixStyleOptions as NixStyleOptions
import Distribution.Client.ProjectConfig.Types
  ( MapLast (..)
  , MapMappend (..)
  , PackageConfig (..)
  , ProjectConfig (..)
  )
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , ProjectBuildContext (..)
  , commandLineFlagsToProjectConfig
  , printPlan
  , pruneInstallPlanToTargets
  , resolveTargetsFromSolver
  , runProjectBuildPhase
  , runProjectPostBuildPhase
  , runProjectPreBuildPhase
  , selectComponentTargetBasic
  )
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (..)
  , ElaboratedInstallPlan
  , ElaboratedSharedConfig (..)
  , TargetAction (..)
  )
import Distribution.Client.ProjectPlanning.Types (elabComponentName)
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( CommonSetupFlags (setupVerbosity)
  , GlobalFlags (..)
  )
import qualified Distribution.Compat.Graph as Graph

import Distribution.Simple.BuildPaths
  ( haddockBenchmarkDirPath
  , haddockDirName
  , haddockLibraryDirPath
  , haddockLibraryPath
  , haddockPath
  , haddockTestDirPath
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  )
import Distribution.Simple.Flag
  ( fromFlag
  , fromFlagOrDefault
  , pattern Flag
  , pattern NoFlag
  )
import Distribution.Simple.Haddock (createHaddockIndex, recordHaddockOutputDirUnitId)
import Distribution.Simple.InstallDirs
  ( toPathTemplate
  )
import Distribution.Simple.Program.Builtin
  ( haddockProgram
  )
import Distribution.Simple.Program.Db
  ( addKnownProgram
  , reconfigurePrograms
  , requireProgramVersion
  )
import Distribution.Simple.Setup
  ( HaddockFlags (..)
  , HaddockProjectFlags (..)
  , HaddockTarget (..)
  , Visibility (..)
  , defaultHaddockFlags
  , haddockProjectCommand
  )
import Distribution.Simple.Utils
  ( copyDirectoryRecursive
  , createDirectoryIfMissingVerbose
  , dieWithException
  , info
  , ordNub
  , warn
  )
import Distribution.Types.ComponentName (ComponentName (CLibName))
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.Types.PackageDescription (PackageDescription (benchmarks, subLibraries, testSuites))
import Distribution.Types.PackageId (pkgName)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnitId (unUnitId)
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange (orLaterVersion)
import Distribution.Verbosity as Verbosity
  ( defaultVerbosityHandles
  , mkVerbosity
  , normal
  )

import Distribution.Client.Errors
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))

-- | 'haddockProjectAction' builds the project with the same settings as
-- @cabal build@, so that either can reuse the builds of the other, copies the
-- documentation of the dependencies into the output directory, documents the
-- local packages directly into it, and generates the contents and index pages
-- there. The extra arguments are ignored.
haddockProjectAction :: HaddockProjectFlags -> [String] -> GlobalFlags -> IO ()
haddockProjectAction flags extraArgs globalFlags = do
  haddockProjectHscolourCss <- traverse makeAbsolute (haddockProjectHscolourCss flags)
  haddockProjectResourcesDir <- traverse makeAbsolute (haddockProjectResourcesDir flags)
  haddockProjectDir <- traverse makeAbsolute (haddockProjectDir flags)
  haddockProjectAction'
    flags
      { haddockProjectHscolourCss
      , haddockProjectResourcesDir
      , haddockProjectDir
      }
    extraArgs
    globalFlags

-- | The implementation of 'haddockProjectAction', which expects the output,
-- resources and HsColour CSS paths in the flags to be absolute.
haddockProjectAction' :: HaddockProjectFlags -> [String] -> GlobalFlags -> IO ()
haddockProjectAction' flags _extraArgs globalFlags = do
  -- create destination directory if it does not exist
  let outputDir = fromFlag (haddockProjectDir flags)
  createDirectoryIfMissingVerbose verbosity True outputDir

  warn verbosity "haddock-project command is experimental, it might break in the future"

  -- Resolve CLI paths before Setup changes the working directory.
  absoluteCss <- traverse makeAbsolute (haddockCss haddockFlags)
  let projectHaddockFlags =
        haddockFlags
          { haddockCss = absoluteCss
          , -- Document local packages directly into the destination folder,
            -- with the same layout, so that they link to the haddocks there,
            -- and the documentation of the build in `dist-newstyle` is left
            -- as it is.
            haddockOutputDir = Flag outputDir
          }
      localOverrides =
        projectConfigLocalPackages $
          commandLineFlagsToProjectConfig
            globalFlags
            (buildFlags{NixStyleOptions.haddockFlags = projectHaddockFlags})
            mempty

  --
  -- Construct the build plan and infer the list of packages which haddocks
  -- we need.
  --

  withContextAndSelectors
    verbosity
    RejectNoTargets
    Nothing
    buildFlags
    ["all"]
    globalFlags
    HaddockCommand
    $ \targetCtx ctx targetSelectors -> do
      initialCtx <- case targetCtx of
        ProjectContext -> return ctx
        GlobalContext -> return ctx
        ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta
      let config = projectConfig initialCtx
          -- Inject documentation: True only when not already set (in local or
          -- global package config), so that projects with documentation: True
          -- in cabal.project don't see a solver-plan change when switching
          -- between build and haddock-project. When documentation: True is
          -- set globally, cabal build also passes -haddock to GHC, so
          -- haddock-project requires no recompilations either.
          docOverride
            | fromFlagOrDefault False (packageConfigDocumentation (projectConfigLocalPackages config)) = mempty
            | fromFlagOrDefault False (packageConfigDocumentation (projectConfigAllPackages config)) = mempty
            | otherwise = mempty{packageConfigDocumentation = Flag True}
          -- Documentation enabled only by docOverride gives GHC the flags that
          -- let Haddock reuse the compilation artifacts, but the build does not
          -- run Haddock for it: haddock-project documents the local packages
          -- into its output directory, so documentation in their build
          -- directories would be generated for nothing.
          withoutBuildDocumentation plan
            | packageConfigDocumentation docOverride /= Flag True = plan
            | otherwise =
                InstallPlan.new (InstallPlan.planIndepGoals plan)
                  . Graph.fromDistinctList
                  . map withoutHaddock
                  . InstallPlan.toList
                  $ plan
            where
              withoutHaddock (InstallPlan.Configured pkg)
                | elabLocalToProject pkg && not (ownDocumentation pkg) =
                    InstallPlan.Configured pkg{elabBuildHaddocks = False}
              withoutHaddock pkg = pkg
              ownDocumentation pkg =
                fromFlagOrDefault False . packageConfigDocumentation $
                  Map.findWithDefault mempty (pkgName (elabPkgSourceId pkg)) (getMapMappend (projectConfigSpecificPackage config))
          -- Build context for Plan 1 (the compilation phase): do NOT apply
          -- haddock output settings (haddockHtml, haddockLinkedSource, etc.)
          -- from localOverrides.  Those settings flow into haddockDocSettings
          -- and are written to pkgFileMonitorDocs; if they differ from a
          -- regular 'cabal build', the next build triggers a spurious
          -- BuildReasonEphemeralTargets rebuild.  Only cliProgramConfig (tool
          -- paths/args that affect the build) and docOverride (documentation:
          -- True so -haddock is passed to GHC) belong here.
          buildBaseCtx =
            initialCtx
              { projectConfig =
                  config
                    { projectConfigLocalPackages =
                        projectConfigLocalPackages config <> cliProgramConfig <> docOverride
                    }
              }
          -- Haddock context for Plan 2: includes the full haddock output
          -- settings so haddockActionWithContext generates the right output.
          baseCtx =
            initialCtx
              { projectConfig =
                  config
                    { projectConfigLocalPackages =
                        projectConfigLocalPackages config <> localOverrides <> cliProgramConfig <> docOverride
                    }
              }
          cabalLayout = cabalDirLayout buildBaseCtx
      buildCtx <-
        runProjectPreBuildPhase verbosity buildBaseCtx $ \elaboratedPlan -> do
          -- Interpret the targets on the command line as build targets
          -- (as opposed to say repl or haddock targets).
          targets <-
            either reportTargetProblems return $
              resolveTargetsFromSolver
                CmdBuild.selectPackageTargets
                selectComponentTargetBasic
                elaboratedPlan
                Nothing
                targetSelectors

          let elaboratedPlan' =
                withoutBuildDocumentation $
                  pruneInstallPlanToTargets
                    TargetActionBuild
                    targets
                    elaboratedPlan
          return (elaboratedPlan', targets)

      let elaboratedPlan :: ElaboratedInstallPlan
          elaboratedPlan = elaboratedPlanOriginal buildCtx

          sharedConfig :: ElaboratedSharedConfig
          sharedConfig = elaboratedShared buildCtx

          pkgs :: [Either InstalledPackageInfo ElaboratedConfiguredPackage]
          pkgs = matchingPackages elaboratedPlan

      progs <-
        reconfigurePrograms
          verbosity
          (haddockProjectProgramPaths flags)
          (haddockProjectProgramArgs flags)
          -- we need to insert 'haddockProgram' before we reconfigure it,
          -- otherwise 'set
          . addKnownProgram haddockProgram
          . pkgConfigCompilerProgs
          $ sharedConfig
      let sharedConfig' = sharedConfig{pkgConfigCompilerProgs = progs}

      _ <-
        requireProgramVersion
          verbosity
          haddockProgram
          (orLaterVersion (mkVersion [2, 26, 1]))
          progs

      --
      -- Build project; we need to build dependencies.
      -- Issue #8958.
      --

      when localStyle $ do
        printPlan verbosity buildBaseCtx buildCtx
        buildOutcomes <- runProjectBuildPhase verbosity buildBaseCtx buildCtx
        runProjectPostBuildPhase verbosity buildBaseCtx buildCtx buildOutcomes

      --
      -- Copy haddocks of dependencies to the destination folder, where the
      -- haddocks of local packages will link to them
      --

      dependencyInfos <-
        for pkgs $
          \case
            Left package | localStyle -> do
              let packageName = unPackageName (pkgName $ sourcePackageId package)
                  destDir = outputDir </> packageName
              fmap catMaybes $ for (haddockInterfaces package) $ \interfacePath -> do
                let docDir = takeDirectory interfacePath
                a <- doesFileExist interfacePath
                case a of
                  True -> do
                    copyDirectoryRecursive verbosity docDir destDir
                    recordHaddockOutputDirUnitId
                      verbosity
                      outputDir
                      ForDevelopment
                      (sourcePackageId package)
                      (sourceLibName package)
                      (installedUnitId package)
                    return $ Just $ Right (packageName, interfacePath, Hidden)
                  False -> return Nothing
            Right package
              | localStyle && not (elabLocalToProject package) -> do
                  let pkg_descr = elabPkgDescription package
                      unitId = unUnitId (elabUnitId package)
                      packageDir =
                        storePackageDirectory
                          (cabalStoreDirLayout cabalLayout)
                          (pkgConfigCompiler sharedConfig')
                          (elabUnitId package)
                      -- TODO: use `InstallDirTemplates`
                      docDir = packageDir </> "share" </> "doc" </> "html"
                      destDir = outputDir </> haddockDirName ForDevelopment pkg_descr
                      interfacePath = destDir </> haddockPath pkg_descr
                  a <- doesDirectoryExist docDir
                  case a of
                    True -> do
                      copyDirectoryRecursive verbosity docDir destDir
                      for_ [lib | Just (CLibName lib) <- [elabComponentName package]] $ \lib ->
                        recordHaddockOutputDirUnitId
                          verbosity
                          outputDir
                          ForDevelopment
                          (elabPkgSourceId package)
                          lib
                          (elabUnitId package)
                      -- non local packages will be hidden in haddock's
                      -- generated contents page
                      return [Right (unitId, interfacePath, Hidden)]
                    False -> do
                      return [Left unitId]
            _ -> return []

      --
      -- Build haddocks of each components
      --

      -- All program arguments are supplied via cliProgramConfig already;
      -- non-idempotent options (e.g. -fplugin-opt) would otherwise be passed
      -- to GHC twice.
      let haddockFlags' = projectHaddockFlags{haddockProgramArgs = []}
      CmdHaddock.haddockActionWithContext
        verbosity
        haddockFlags'
        baseCtx
        targetSelectors

      --
      -- Collect the haddocks of local packages, built in the destination
      -- folder
      --

      localInfos <-
        for pkgs $
          \case
            Right package
              | elabLocalToProject package -> do
                  let pkg_descr = elabPkgDescription package

                      packageName = pkgName $ elabPkgSourceId package
                      unitId = elabUnitId package
                      packageDir = haddockDirName ForDevelopment pkg_descr
                      destDir = outputDir </> packageDir
                      interfacePath = destDir </> haddockPath pkg_descr

                  a <- doesDirectoryExist destDir
                  if a
                    then do
                      let infos :: [(String, FilePath, Visibility)]
                          infos =
                            (unPackageName packageName, interfacePath, Visible)
                              : [ (sublibDirPath, sublibInterfacePath, Visible)
                                | lib <- subLibraries pkg_descr
                                , let sublibDirPath = haddockLibraryDirPath ForDevelopment pkg_descr lib
                                      sublibInterfacePath =
                                        outputDir
                                          </> sublibDirPath
                                          </> haddockLibraryPath pkg_descr lib
                                ]
                              ++ [ (testPath, testInterfacePath, Visible)
                                 | test <- testSuites pkg_descr
                                 , let testPath = haddockTestDirPath ForDevelopment pkg_descr test
                                       testInterfacePath =
                                        outputDir
                                          </> testPath
                                          </> haddockPath pkg_descr
                                 ]
                              ++ [ (benchPath, benchInterfacePath, Visible)
                                 | bench <- benchmarks pkg_descr
                                 , let benchPath = haddockBenchmarkDirPath ForDevelopment pkg_descr bench
                                       benchInterfacePath =
                                        outputDir
                                          </> benchPath
                                          </> haddockPath pkg_descr
                                 ]
                      infos' <-
                        mapM
                          ( \x@(_, path, _) -> do
                              e <- doesFileExist path
                              return $
                                if e
                                  then Right x
                                  else Left path
                          )
                          infos
                      return infos'
                    else do
                      warn
                        verbosity
                        ( "haddocks of "
                            ++ unUnitId unitId
                            ++ " not found in the store"
                        )
                      return []
            _ -> return []

      -- In the order of the plan.
      let packageInfos = ordNub (concat (zipWith (++) dependencyInfos localInfos))

      --
      -- generate index, content, etc.
      --

      let (missingHaddocks, packageInfos') = partitionEithers packageInfos
      unless (null missingHaddocks) $ do
        warn verbosity "missing haddocks for some packages from the store"
        -- Show the package list if `-v1` is passed; it's usually a long list.
        -- One needs to add `package` stantza in `cabal.project` file for
        -- `cabal` to include a version which has haddocks (or set
        -- `documentation: True` in the global config).
        info verbosity (intercalate "\n" missingHaddocks)

      let flags' =
            flags
              { haddockProjectDir = Flag outputDir
              , haddockProjectInterfaces =
                  Flag
                    [ ( interfacePath
                      , Just url
                      , Just url
                      , visibility
                      )
                    | (url, interfacePath, visibility) <- packageInfos'
                    ]
              , haddockProjectUseUnicode = NoFlag
              }
      createHaddockIndex
        verbosity
        (pkgConfigCompilerProgs sharedConfig')
        (pkgConfigCompiler sharedConfig')
        (pkgConfigPlatform sharedConfig')
        Nothing
        flags'
  where
    -- build all packages with appropriate haddock flags
    commonFlags = haddockProjectCommonFlags flags

    -- CLI program paths/args injected into the project config so they affect
    -- both the build and haddock phases and invalidate the configuration cache.
    cliProgramConfig :: PackageConfig
    cliProgramConfig =
      mempty
        { packageConfigProgramPaths =
            MapLast (Map.fromList (haddockProjectProgramPaths flags))
        , packageConfigProgramArgs =
            MapMappend (Map.fromListWith (flip (++)) (haddockProjectProgramArgs flags))
        }

    verbosity =
      mkVerbosity defaultVerbosityHandles $
        fromFlagOrDefault normal (setupVerbosity commonFlags)

    haddockFlags =
      defaultHaddockFlags
        { haddockCommonFlags = commonFlags
        , haddockHtml = Flag True
        , -- one can either use `--haddock-base-url` or
          -- `--haddock-html-location`.
          haddockBaseUrl =
            if localStyle
              then Flag ".."
              else NoFlag
        , haddockProgramPaths = haddockProjectProgramPaths flags
        , haddockProgramArgs = haddockProjectProgramArgs flags
        , haddockHtmlLocation =
            if fromFlagOrDefault False (haddockProjectHackage flags)
              then Flag "https://hackage.haskell.org/package/$pkg-$version/docs"
              else haddockProjectHtmlLocation flags
        , haddockHoogle = haddockProjectHoogle flags
        , haddockExecutables = haddockProjectExecutables flags
        , haddockTestSuites = haddockProjectTestSuites flags
        , haddockBenchmarks = haddockProjectBenchmarks flags
        , haddockForeignLibs = haddockProjectForeignLibs flags
        , haddockInternal = haddockProjectInternal flags
        , haddockCss = haddockProjectCss flags
        , haddockLinkedSource = Flag True
        , haddockQuickJump = Flag True
        , haddockHscolourCss = haddockProjectHscolourCss flags
        , haddockContents =
            if localStyle
              then Flag (toPathTemplate "../index.html")
              else NoFlag
        , haddockIndex =
            if localStyle
              then Flag (toPathTemplate "../doc-index.html")
              else NoFlag
        , haddockResourcesDir = haddockProjectResourcesDir flags
        , haddockUseUnicode = haddockProjectUseUnicode flags
        }

    -- Discover the project without adding documentation options to dependencies.
    buildFlags =
      NixStyleOptions.updNixStyleCommonSetupFlags
        (const commonFlags)
        (commandDefaultFlags CmdBuild.buildCommand)

    -- Build a self contained directory which contains haddocks of all
    -- transitive dependencies; or depend on `--haddocks-html-location` to
    -- provide location of the documentation of dependencies.
    localStyle =
      let hackage = fromFlagOrDefault False (haddockProjectHackage flags)
          location = fromFlagOrDefault False (True <$ haddockProjectHtmlLocation flags)
       in not hackage && not location

    reportTargetProblems :: Show x => [x] -> IO a
    reportTargetProblems =
      dieWithException verbosity . CmdHaddockReportTargetProblems . map show

    matchingPackages
      :: ElaboratedInstallPlan
      -> [Either InstalledPackageInfo ElaboratedConfiguredPackage]
    matchingPackages =
      fmap (foldPlanPackage Left Right)
        . InstallPlan.toList
