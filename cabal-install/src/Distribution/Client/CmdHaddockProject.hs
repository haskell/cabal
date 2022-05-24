module Distribution.Client.CmdHaddockProject
  ( haddockProjectCommand
  , haddockProjectAction
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (get)

import qualified Distribution.Client.CmdBuild   as CmdBuild
import qualified Distribution.Client.CmdHaddock as CmdHaddock

import Distribution.Client.DistDirLayout      (DistDirLayout(..)
                                              ,CabalDirLayout(..)
                                              ,StoreDirLayout(..))
import Distribution.Client.InstallPlan        (foldPlanPackage)
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.NixStyleOptions as NixStyleOptions
import Distribution.Client.ProjectOrchestration
                                              (AvailableTarget(..)
                                              ,AvailableTargetStatus(..)
                                              ,ProjectBaseContext(..)
                                              ,ProjectBuildContext(..)
                                              ,TargetSelector(..)
                                              ,printPlan
                                              ,pruneInstallPlanToTargets
                                              ,resolveTargets
                                              ,runProjectPreBuildPhase
                                              ,selectComponentTargetBasic)
import Distribution.Client.ProjectPlanning    (ElaboratedConfiguredPackage(..)
                                              ,ElaboratedInstallPlan
                                              ,ElaboratedSharedConfig(..)
                                              ,TargetAction(..))
import Distribution.Client.ProjectPlanning.Types
                                              (elabDistDirParams)
import Distribution.Client.Setup              (GlobalFlags(..)
                                              ,ConfigFlags(..))
import Distribution.Client.ScriptUtils        (AcceptNoTargets(..)
                                              ,TargetContext(..)
                                              ,updateContextAndWriteProjectFile
                                              ,withContextAndSelectors)
import Distribution.Client.TargetProblem      (TargetProblem(..))

import Distribution.Types.PackageId (pkgName)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Simple.Command
         ( CommandUI(..) )
import Distribution.Simple.Compiler
         ( Compiler (..) )
import Distribution.Simple.InstallDirs
         ( toPathTemplate )
import Distribution.Simple.Haddock (createHaddockIndex)
import Distribution.Simple.Utils
         ( die', createDirectoryIfMissingVerbose
         , copyDirectoryRecursive, )
import Distribution.Simple.Program.Builtin
         ( haddockProgram )
import Distribution.Simple.Program.Db
         ( addKnownProgram, reconfigurePrograms )
import Distribution.Simple.Setup
         ( HaddockFlags(..), defaultHaddockFlags
         , HaddockProjectFlags(..)
         , Flag(..), fromFlag, fromFlagOrDefault
         , haddockProjectCommand
         )
import Distribution.Verbosity as Verbosity
         ( normal )

import System.FilePath          ( normalise, (</>), (<.>) )
import System.Directory         ( doesDirectoryExist )

haddockProjectAction :: HaddockProjectFlags -> [String] -> GlobalFlags -> IO ()
haddockProjectAction flags _extraArgs globalFlags = do
    -- create destination directory if it does not exist
    let outputDir = normalise $ fromFlag (haddockProjectDir flags)
    createDirectoryIfMissingVerbose verbosity True outputDir

    -- build all packages with appropriate haddock flags
    let haddockFlags = defaultHaddockFlags
          { haddockHtml         = Flag True
          , haddockBaseUrl      = Flag ".."
          , haddockProgramPaths = haddockProjectProgramPaths  flags
          , haddockProgramArgs  = haddockProjectProgramArgs   flags
          , haddockHoogle       = haddockProjectHoogle        flags
          , haddockExecutables  = haddockProjectExecutables   flags
          , haddockTestSuites   = haddockProjectTestSuites    flags
          , haddockBenchmarks   = haddockProjectBenchmarks    flags
          , haddockForeignLibs  = haddockProjectForeignLibs   flags
          , haddockInternal     = haddockProjectInternal      flags
          , haddockCss          = haddockProjectCss           flags
          , haddockLinkedSource = haddockProjectLinkedSource  flags
          , haddockQuickJump    = haddockProjectQuickJump     flags
          , haddockHscolourCss  = haddockProjectHscolourCss   flags
          , haddockContents     = Flag (toPathTemplate "../index.html")
          , haddockIndex        = Flag (toPathTemplate "../doc-index.html")
          , haddockKeepTempFiles= haddockProjectKeepTempFiles flags
          , haddockVerbosity    = haddockProjectVerbosity     flags
          , haddockLib          = haddockProjectLib           flags
          }
        nixFlags = (commandDefaultFlags CmdHaddock.haddockCommand)
                   { NixStyleOptions.haddockFlags = haddockFlags
                   , NixStyleOptions.configFlags  =
                       (NixStyleOptions.configFlags (commandDefaultFlags CmdBuild.buildCommand))
                       { configVerbosity = haddockProjectVerbosity flags }
                   }
    CmdHaddock.haddockAction
      nixFlags
      ["all"]
      globalFlags

    -- copy local packages to the destination directory
    withContextAndSelectors RejectNoTargets Nothing nixFlags ["all"] globalFlags $ \targetCtx ctx targetSelectors -> do
      baseCtx <- case targetCtx of
        ProjectContext             -> return ctx
        GlobalContext              -> return ctx
        ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta
      let distLayout  = distDirLayout baseCtx
          cabalLayout = cabalDirLayout baseCtx
      buildCtx <-
        runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
              -- Interpret the targets on the command line as build targets
              -- (as opposed to say repl or haddock targets).
              targets <- either reportTargetProblems return
                       $ resolveTargets
                           selectPackageTargets
                           selectComponentTargetBasic
                           elaboratedPlan
                           Nothing
                           targetSelectors

              let elaboratedPlan' = pruneInstallPlanToTargets
                                      TargetActionBuild
                                      targets
                                      elaboratedPlan
              return (elaboratedPlan', targets)

      printPlan verbosity baseCtx buildCtx

      let elaboratedPlan :: ElaboratedInstallPlan
          elaboratedPlan = elaboratedPlanOriginal buildCtx

          sharedConfig :: ElaboratedSharedConfig
          sharedConfig = elaboratedShared buildCtx

          pkgs :: [ElaboratedConfiguredPackage]
          pkgs = matchingPackages elaboratedPlan

      progs <- reconfigurePrograms verbosity
                 (haddockProjectProgramPaths flags)
                 (haddockProjectProgramArgs flags)
               -- we need to insert 'haddockProgram' before we reconfigure it,
               -- otherwise 'set
             . addKnownProgram haddockProgram
             . pkgConfigCompilerProgs
             $ sharedConfig
      let sharedConfig' = sharedConfig { pkgConfigCompilerProgs = progs }

      packageNames <- fmap (nub . catMaybes) $ for pkgs $ \package ->
        if elabLocalToProject package
        then do
          let distDirParams = elabDistDirParams sharedConfig' package
              buildDir = distBuildDirectory distLayout distDirParams
              packageName = unPackageName (pkgName $ elabPkgSourceId package)
          let docDir = buildDir
                   </> "doc" </> "html"
                   </> packageName
              destDir = outputDir </> packageName
          a <- doesDirectoryExist docDir
          case a of
            True  -> copyDirectoryRecursive verbosity docDir destDir
                  >> return (Just (packageName, destDir))
            False -> return Nothing
        else do
          let packageName = unPackageName (pkgName $ elabPkgSourceId package)
              packageDir = storePackageDirectory (cabalStoreDirLayout cabalLayout)
                             (compilerId (pkgConfigCompiler sharedConfig'))
                             (elabUnitId package)
              docDir = packageDir </> "share" </> "doc" </> "html"
              destDir = outputDir </> packageName
          a <- doesDirectoryExist docDir
          case a of
            True  -> copyDirectoryRecursive verbosity docDir destDir
                  >> return (Just (packageName, destDir))
            False -> return Nothing

      -- run haddock to generate index, content, etc.
      let flags' = flags
            { haddockProjectDir        = Flag outputDir
            , haddockProjectInterfaces = Flag
                [ ( destDir </> packageName <.> "haddock"
                  , Just packageName
                  , Just packageName
                  )
                | (packageName, destDir) <- packageNames
                ]
            }
      createHaddockIndex verbosity
                         (pkgConfigCompilerProgs sharedConfig')
                         (pkgConfigCompiler sharedConfig')
                         (pkgConfigPlatform sharedConfig')
                         flags'
  where
    verbosity = fromFlagOrDefault normal (haddockProjectVerbosity flags)

    reportTargetProblems :: Show x => [x] -> IO a
    reportTargetProblems =
        die' verbosity . unlines . map show

    -- TODO: this is just a sketch
    selectPackageTargets :: TargetSelector
                         -> [AvailableTarget k]
                         -> Either (TargetProblem ()) [k]
    selectPackageTargets _ ts = Right $
      mapMaybe
        (\t -> case availableTargetStatus t of
            TargetBuildable k _ | availableTargetLocalToProject t
                                -> Just k
            _                   -> Nothing)
        ts

    matchingPackages :: ElaboratedInstallPlan
                     -> [ElaboratedConfiguredPackage]
    matchingPackages =
        catMaybes
      . fmap (foldPlanPackage (const Nothing) Just)
      . InstallPlan.toList
