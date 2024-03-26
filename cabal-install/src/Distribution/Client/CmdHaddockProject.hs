module Distribution.Client.CmdHaddockProject
  ( haddockProjectCommand
  , haddockProjectAction
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import qualified Distribution.Client.CmdBuild as CmdBuild
import qualified Distribution.Client.CmdHaddock as CmdHaddock

import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , StoreDirLayout (..)
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
  , printPlan
  , pruneInstallPlanToTargets
  , resolveTargets
  , runProjectPreBuildPhase
  , selectComponentTargetBasic
  )
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (..)
  , ElaboratedInstallPlan
  , ElaboratedSharedConfig (..)
  , TargetAction (..)
  )
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags (..)
  )
import Distribution.Client.TargetProblem (TargetProblem (..))

import Distribution.Simple.BuildPaths
  ( haddockDirName
  , haddockLibraryDirName
  , haddockLibraryPath
  , haddockPackageLibraryDirName
  , haddockPath
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  )
import Distribution.Simple.Flag
  ( Flag (..)
  , fromFlag
  , fromFlagOrDefault
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
import Distribution.Types.PackageDescription (PackageDescription (subLibraries))
import Distribution.Types.PackageId (pkgName)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnitId (unUnitId)
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange (orLaterVersion)
import Distribution.Verbosity as Verbosity
  ( normal
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

  -- build all packages with appropriate haddock flags
  let haddockFlags =
        defaultHaddockFlags
          { haddockHtml = Flag True
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
          , haddockKeepTempFiles = haddockProjectKeepTempFiles flags
          , haddockVerbosity = haddockProjectVerbosity flags
          , haddockLib = haddockProjectLib flags
          , haddockOutputDir = haddockProjectDir flags
          }
      nixFlags =
        (commandDefaultFlags CmdHaddock.haddockCommand)
          { NixStyleOptions.haddockFlags = haddockFlags
          , NixStyleOptions.configFlags =
              (NixStyleOptions.configFlags (commandDefaultFlags CmdBuild.buildCommand))
                { configVerbosity = haddockProjectVerbosity flags
                }
          }

  --
  -- Construct the build plan and infer the list of packages which haddocks
  -- we need.
  --

  withContextAndSelectors
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
      let cabalLayout = cabalDirLayout baseCtx
      buildCtx <-
        runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
          -- Interpret the targets on the command line as build targets
          -- (as opposed to say repl or haddock targets).
          targets <-
            either reportTargetProblems return $
              resolveTargets
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

      printPlan verbosity baseCtx buildCtx

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
          Left _
            | not localStyle ->
                return []
          Left package -> do
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
          Right package ->
            case elabLocalToProject package of
              True -> do
                let pkg_descr = elabPkgDescription package

                    packageName = pkgName $ elabPkgSourceId package
                    packageDir = haddockDirName ForDevelopment pkg_descr
                    interfacePath = outputDir </> packageDir </> haddockPath pkg_descr

                a <- doesFileExist interfacePath
                if a
                  then
                    return $
                      Right (unPackageName packageName, interfacePath, Visible)
                        : [ Right (sublibDirName, sublibInterfacePath, Visible)
                          | lib <- subLibraries pkg_descr
                          , let sublibDirName = haddockPackageLibraryDirName pkg_descr lib
                                sublibDirPath = haddockLibraryDirName ForDevelopment pkg_descr lib
                                sublibInterfacePath =
                                  outputDir
                                    </> sublibDirPath
                                    </> haddockLibraryPath pkg_descr lib
                          ]
                  else do
                    -- if the directory does not exist, it means the package has
                    -- no haddocks, e.g. doesn't have a library component.
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
              }
      createHaddockIndex
        verbosity
        (pkgConfigCompilerProgs sharedConfig')
        (pkgConfigCompiler sharedConfig')
        (pkgConfigPlatform sharedConfig')
        flags'
  where
    verbosity = fromFlagOrDefault normal (haddockProjectVerbosity flags)

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
