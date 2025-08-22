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

import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , StoreDirLayout (..)
  , distBuildDirectory
  )
import Distribution.Client.InstallPlan (foldPlanPackage)
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.NixStyleOptions as NixStyleOptions
import Distribution.Client.ProjectOrchestration
  ( AvailableTarget (..)
  , AvailableTargetStatus (..)
  , CurrentCommand (..)
  , ProjectBaseContext (..)
  , ProjectBuildContext (..)
  , TargetSelector (..)
  , pruneInstallPlanToTargets
  , resolveTargetsFromSolver
  , runProjectPreBuildPhase
  , selectComponentTargetBasic
  )
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (..)
  , ElaboratedInstallPlan
  , ElaboratedSharedConfig (..)
  , TargetAction (..)
  )
import Distribution.Client.ProjectPlanning.Types
  ( elabDistDirParams
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( CommonSetupFlags (setupVerbosity)
  , ConfigFlags (..)
  , GlobalFlags (..)
  )
import Distribution.Client.TargetProblem (TargetProblem (..))

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
import Distribution.Simple.Haddock (createHaddockIndex)
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
  , warn
  )
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
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (normalise, takeDirectory, (</>))

haddockProjectAction :: HaddockProjectFlags -> [String] -> GlobalFlags -> IO ()
haddockProjectAction flags _extraArgs globalFlags = do
  -- create destination directory if it does not exist
  let outputDir = normalise $ fromFlag (haddockProjectDir flags)
  createDirectoryIfMissingVerbose verbosity True outputDir

  warn verbosity "haddock-project command is experimental, it might break in the future"

  --
  -- Construct the build plan and infer the list of packages which haddocks
  -- we need.
  --

  withContextAndSelectors
    verbosity
    RejectNoTargets
    Nothing
    (commandDefaultFlags CmdBuild.buildCommand)
    ["all"]
    globalFlags
    HaddockCommand
    $ \targetCtx ctx targetSelectors -> do
      baseCtx <- case targetCtx of
        ProjectContext -> return ctx
        GlobalContext -> return ctx
        ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta
      let distLayout = distDirLayout baseCtx
          cabalLayout = cabalDirLayout baseCtx
      buildCtx <-
        runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
          -- Interpret the targets on the command line as build targets
          -- (as opposed to say repl or haddock targets).
          targets <-
            either reportTargetProblems return $
              resolveTargetsFromSolver
                selectPackageTargets
                selectComponentTargetBasic
                elaboratedPlan
                Nothing
                targetSelectors

          let elaboratedPlan' =
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

      when localStyle $
        CmdBuild.buildAction
          (commandDefaultFlags CmdBuild.buildCommand)
          ["all"]
          globalFlags

      --
      -- Build haddocks of each components
      --

      CmdHaddock.haddockAction
        nixFlags
        ["all"]
        globalFlags

      --
      -- Copy haddocks to the destination folder
      --

      packageInfos <- fmap (nub . concat) $ for pkgs $ \pkg ->
        case pkg of
          Left package | localStyle -> do
            let packageName = unPackageName (pkgName $ sourcePackageId package)
                destDir = outputDir </> packageName
            fmap catMaybes $ for (haddockInterfaces package) $ \interfacePath -> do
              let docDir = takeDirectory interfacePath
              a <- doesFileExist interfacePath
              case a of
                True -> do
                  copyDirectoryRecursive verbosity docDir destDir
                  return $ Just $ Right (packageName, interfacePath, Hidden)
                False -> return Nothing
          Left _ -> return []
          Right package ->
            case elabLocalToProject package of
              True -> do
                let distDirParams = elabDistDirParams sharedConfig' package
                    pkg_descr = elabPkgDescription package

                    packageName = pkgName $ elabPkgSourceId package
                    unitId = elabUnitId package
                    packageDir = haddockDirName ForDevelopment pkg_descr
                    destDir = outputDir </> packageDir
                    interfacePath = destDir </> haddockPath pkg_descr

                    buildDir = distBuildDirectory distLayout distDirParams
                    docDir =
                      buildDir
                        </> "doc"
                        </> "html"
                        </> packageDir

                a <- doesDirectoryExist docDir
                if a
                  then do
                    copyDirectoryRecursive verbosity docDir destDir
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
              False
                | not localStyle ->
                    return []
              False -> do
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
                    -- non local packages will be hidden in haddock's
                    -- generated contents page
                    return [Right (unitId, interfacePath, Hidden)]
                  False -> do
                    return [Left unitId]

      --
      -- generate index, content, etc.
      --

      let (missingHaddocks, packageInfos') = partitionEithers packageInfos
      when (not (null missingHaddocks)) $ do
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
        -- NOTE: we don't pass `haddockOutputDir`. If we do, we'll need to
        -- make sure `InstalledPackageInfo` contains the right path to
        -- haddock interfaces.  Instead we build documentation inside
        -- `dist-newstyle` directory and copy it to the output directory.
        }

    nixFlags =
      (commandDefaultFlags CmdHaddock.haddockCommand)
        { NixStyleOptions.haddockFlags = haddockFlags
        , NixStyleOptions.configFlags =
            (NixStyleOptions.configFlags (commandDefaultFlags CmdBuild.buildCommand))
              { configCommonFlags = commonFlags
              }
        }

    -- Build a self contained directory which contains haddocks of all
    -- transitive dependencies; or depend on `--haddocks-html-location` to
    -- provide location of the documentation of dependencies.
    localStyle =
      let hackage = fromFlagOrDefault False (haddockProjectHackage flags)
          location = fromFlagOrDefault False (const True <$> haddockProjectHtmlLocation flags)
       in not hackage && not location

    reportTargetProblems :: Show x => [x] -> IO a
    reportTargetProblems =
      dieWithException verbosity . CmdHaddockReportTargetProblems . map show

    -- TODO: this is just a sketch
    selectPackageTargets
      :: TargetSelector
      -> [AvailableTarget k]
      -> Either (TargetProblem ()) [k]
    selectPackageTargets _ ts =
      Right $
        mapMaybe
          ( \t -> case availableTargetStatus t of
              TargetBuildable k _
                | availableTargetLocalToProject t ->
                    Just k
              _ -> Nothing
          )
          ts

    matchingPackages
      :: ElaboratedInstallPlan
      -> [Either InstalledPackageInfo ElaboratedConfiguredPackage]
    matchingPackages =
      fmap (foldPlanPackage Left Right)
        . InstallPlan.toList
