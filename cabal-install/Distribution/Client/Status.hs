-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Status
-- Copyright   :  (c) Lennart Spitzner 2015
-- License     :  BSD-like
--
-- Maintainer  :  lsp@informatik.uni-kiel.de
-- Stability   :  provisional
-- Portability :  portable
--
-- print a summary of info about
--   - versions used
--   - the package (if present)
--   - package databases
--   - the sandbox (if present)
--   - the install plan
--
-----------------------------------------------------------------------------
module Distribution.Client.Status (
    status
  ) where

import Distribution.Simple.PackageIndex       ( allPackages )
import Distribution.Simple.Program.Db         ( ProgramDb, configuredPrograms )
import Distribution.Simple.Program.Types      ( ConfiguredProgram(..) )
import Distribution.Simple.Configure          ( tryGetPersistBuildConfig )
import Distribution.Simple.Compiler           ( Compiler(..), compilerFlavor
                                              , PackageDB(..), PackageDBStack
                                              , compilerId )
import Distribution.Simple.BuildPaths         ( exeExtension )
import qualified Distribution.Simple.GHC as GHC
                                              ( checkPackageDBs )
import Distribution.Simple.Utils              ( cabalVersion
                                              , die, debug
                                              , rawSystemStdout
                                              , listPackageDescs
                                              , currentDir
                                              , wrapText
                                              )
import Distribution.Client.Setup       hiding ( cabalVersion, packageName )
import Distribution.Client.Config
import Distribution.Client.Sandbox
import Distribution.Client.Sandbox.PackageEnvironment
import Distribution.Client.SetupWrapper
import Distribution.Client.Freeze
import Distribution.Client.IndexUtils
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Solver.Types.PkgConfigDb

import Distribution.Compiler                  ( CompilerFlavor(..) )
import Distribution.Text                      ( simpleParse
                                              , display
                                              , disp
                                              , render
                                              , brokenString
                                              )
import Distribution.Version                   ( Version(..)
                                              )
import Distribution.PackageDescription.Parse  ( readPackageDescription )
import Distribution.PackageDescription        ( GenericPackageDescription(..)
                                              , package
                                              )
import Distribution.InstalledPackageInfo      ( InstalledPackageInfo(..) )
import Distribution.Package
import Distribution.Verbosity

import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Simple.LocalBuildInfo as LBI

import qualified Paths_cabal_install          ( version )

import Distribution.Client.Compat.ExecutablePath
import System.Directory                       ( doesFileExist )
import System.FilePath                        ( splitFileName
                                              , combine
                                              , splitPath
                                              , joinPath
                                              , takeDirectory
                                              , (<.>)
                                              )
import Control.Monad                          ( when
                                              , forM
                                              , guard
                                              , forM_
                                              )
import Data.Version                           ( showVersion
                                              )
import Data.Maybe                             ( isJust
                                              )
import Data.List                              ( groupBy
                                              , sortBy
                                              )
import Data.Ord                               ( comparing
                                              )
import Text.PrettyPrint                       ( empty
                                              , ($$)
                                              , vcat
                                              , nest
                                              , text
                                              , punctuate
                                              , ($+$)
                                              , (<+>)
                                              )
import Data.Monoid                            ( (<>)
                                              )
import qualified Data.Monoid as Monoid
import Data.List                              ( inits )
import Data.Foldable                          ( asum )


-- | Check the consistency of the given package databases.
checkPackageDBs :: Verbosity -> Compiler
                -> PackageDBStack -- ^ The stack of package databases.
                -> ProgramDb
                -> IO [(PackageDB, [String])]
checkPackageDBs verbosity comp packageDBs progdb = do
  when (null packageDBs) $
    die $ "No package databases have been specified. If you use "
       ++ "--package-db=clear, you must follow it with --package-db= "
       ++ "with 'global', 'user' or a specific file."

  debug verbosity "checking package-db..."
  case compilerFlavor comp of
    GHC   -> GHC.checkPackageDBs verbosity comp packageDBs progdb
    -- GHCJS -> GHCJS.checkPackageDBs verbosity packageDBs progdb
    -- JHC   -> JHC.checkPackageDBs verbosity packageDBs progdb
    -- LHC   -> LHC.checkPackageDBs verbosity packageDBs progdb
    -- UHC   -> UHC.checkPackageDBs verbosity comp packageDBs progdb
    -- HaskellSuite {} ->
    --   HaskellSuite.checkPackageDBs verbosity packageDBs progdb
    flv -> die $ "don't know how to check the packages database for "
              ++ display flv


status :: Verbosity -> GlobalFlags -> StatusFlags -> IO ()
status verbosity globalFlags statusFlags = do
  (useSandbox, config) <- loadConfigOrSandboxConfig
    verbosity
    (globalFlags { globalRequireSandbox = Cabal.Flag False })
  (defaultComp, platform, defaultConf) <- configCompilerAux'
                                        $ savedConfigureFlags config
  let distPref = useDistPref defaultSetupScriptOptions
  -- TODO: this currently only works if the cabal version used for configuring
  --       is the same as the one doing the status. would be cool if it would
  --       either work or at least indicate this otherwise.
  buildConfig <- tryGetPersistBuildConfig distPref
  let (comp, conf) = case buildConfig of
        Left _ -> (defaultComp, defaultConf)
        Right c -> (LBI.compiler c, LBI.withPrograms c)
  let globalFlags' = savedGlobalFlags config `Monoid.mappend` globalFlags
  sandboxConfigPath <- getSandboxConfigFilePath globalFlags
  sandboxConfigExists <- doesFileExist sandboxConfigPath
  (sandboxPath, sandboxDbPath) <- if sandboxConfigExists
    then do
      (sandboxPath, _pkgEnv) <- tryLoadSandboxConfig verbosity globalFlags
      return $ ( Just sandboxPath
               , Just $ sandboxPackageDBPath sandboxPath comp platform)
    else return (Nothing, Nothing)
  let dbs :: [PackageDB] -- TODO: it might be better to inspect the currently
                         -- configured dbs instead of a static global,local,..
      dbs = GlobalPackageDB
          : UserPackageDB
          : case sandboxDbPath of
              Nothing -> []
              Just p -> [SpecificPackageDB p]
  let noFlags = not $ any Cabal.fromFlag [ statusVersion      statusFlags
                                         , statusProgVersions statusFlags
                                         , statusCompiler     statusFlags
                                         , statusPackage      statusFlags
                                         , statusPlan         statusFlags
                                         , statusSandbox      statusFlags
                                         , statusPkgDbs       statusFlags
                                         , statusCheckDb      statusFlags
                                         , statusAll          statusFlags
                                         ]
      doAll          = Cabal.fromFlag $ statusAll statusFlags
      doVersion      = Cabal.fromFlag (statusVersion statusFlags)
                       || doAll || noFlags
      doProgVersions = Cabal.fromFlag (statusProgVersions statusFlags)
                       || doAll
      doCompiler     = Cabal.fromFlag (statusCompiler statusFlags)
                       || doAll || noFlags
      doPackage      = Cabal.fromFlag (statusPackage statusFlags)
                       || Cabal.fromFlag (statusPlan statusFlags) -- implication
                       || doAll || noFlags
      doPlan         = Cabal.fromFlag (statusPlan statusFlags)
                       || doAll
      doSandbox      = Cabal.fromFlag (statusSandbox statusFlags)
                       || doAll || noFlags
      doPkgDbs       = Cabal.fromFlag (statusPkgDbs statusFlags)
                       || doAll
      doCheckDb      = Cabal.fromFlag (statusCheckDb statusFlags)
                       || doAll
  case () of { () -> do
    when doVersion      printCurrentVersion
    -- printOtherCabalCheck is called by printCurrentVersion
    when doProgVersions printProgramVersions
    when doCompiler     printCompiler
    when doPackage      printPackageInformation
    when doSandbox      printSandboxInformation
    when doPkgDbs       printPackageDbs
    when doCheckDb      printPackageDBChecks
    -- printInstallPlan is called by printPackageInformation
   where
  -- print self version information:
  printCurrentVersion :: IO ()
  -- check if there is a different newer cabal-install installed
  -- (with the likely reason of a missing PATH addition):
  printOtherCabalCheck :: IO ()
  -- print external program versions (ghc, ...):
  printProgramVersions :: IO ()
  -- print the configured compiler
  printCompiler :: IO ()
  -- information about the package in the current directory:
  printPackageInformation :: IO ()
  -- print information about the sandbox in the current directory
  -- (including a check if a parent directory has a sandbox):
  printSandboxInformation :: IO ()
  -- print which package dbs are currently configured:
  printPackageDbs :: IO ()
  -- ghc-pkg check on the current package dbs (including sandbox, if present)
  printPackageDBChecks :: IO ()
  -- print the install plan (i.e. all configured dependencies); similar
  -- to "cabal freeze --dry-run":
  printInstallPlan :: IO ()
  printCurrentVersion = do
    putStrLn "Self version:"
    putStrLn $ render
             $ nest 2
             $ brokenString
             $ "cabal-install version "
            ++ display Paths_cabal_install.version
            ++ " compiled using version "
            ++ display cabalVersion
            ++ " of the Cabal library."
    printOtherCabalCheck
  printOtherCabalCheck = do
    defInstDirs <- LBI.defaultInstallDirs defaultCompiler True False
    let binDir :: FilePath
        binDir = LBI.fromPathTemplate
               $ LBI.bindir
               $ LBI.substituteInstallDirTemplates [] defInstDirs
    currentBinDir <- getExecutablePath
    when (takeDirectory binDir /= takeDirectory currentBinDir) $ do
      let defaultCabalPath = binDir `combine` ("cabal" <.> exeExtension)
      exists <- doesFileExist defaultCabalPath
      when exists $ do
        -- TODO: replace rawSystemStdout by rawSystemStdInOuts
        v <- rawSystemStdout verbosity defaultCabalPath ["--version"]
        let vs :: [Maybe Version]
            vs = map (simpleParse . filter (\x -> x `elem` "0123456789."))
               $ lines v
        case vs of
          [Just cabalInstallVersion, Just _cabalVersion] ->
            if cabalInstallVersion == Paths_cabal_install.version
              then return ()
              else putStrLn $ render
                            $ nest 2
                            $ brokenString
                            $ "  There is a different 'cabal' executable"
                           ++ " (version "
                           ++ showVersion cabalInstallVersion
                           ++ ") located in " ++ binDir
                           ++ "."
          _ ->     putStrLn $ render
                            $ nest 2
                            $ brokenString
                            $ wrapText
                            $ "  Warning: could not parse version of"
                           ++ " the `cabal` executable located in "
                           ++ binDir
                           ++ "."
    return ()
  printProgramVersions = do
    putStrLn "Program versions:"
    flip mapM_ (configuredPrograms conf) $ \cp ->
      case programVersion cp of
        Nothing -> return ()
        Just v  -> putStrLn
                 $ render
                 $ nest 2
                 $    text (programId cp)
                   <> text "-"
                   <> disp v
  printCompiler = do
    putStrLn "Configured compiler:"
    case buildConfig of
      Left _ -> putStrLn $ "  Could not be determined."
      Right lbi -> putStrLn $ "  " ++ (display $ compilerId $ LBI.compiler lbi)
  printPackageInformation = do
    putStrLn "Local package(s):"
    packageDescs <- listPackageDescs currentDir
    case packageDescs of
      []  -> putStrLn "  No packages in the current directory."
      [p] -> do
        ppd <- readPackageDescription verbosity p
        let packageDesc = packageDescription $ ppd
        let packName :: String
            packName = unPackageName $ pkgName $ package $ packageDesc
        let components :: [String]
            components =
              (  [ "Library \"" ++ packName ++ "\""
                 | isJust (condLibrary ppd) ]
              ++ [ "Executable \"" ++ fst e ++ "\""
                 | e <- condExecutables ppd]
              ++ [ "Testsuite \"" ++ fst t ++ "\""
                 | t <- condTestSuites ppd]
              ++ [ "Benchmark \"" ++ fst b ++ "\""
                 | b <- condBenchmarks ppd]
              )
        putStrLn $ render
                 $ nest 2
                 $ text ("Package \"" ++ packName ++ "\" containing "
                        ++ show (length components) ++ " component(s):")
                   $$ ( nest 2
                      $ vcat
                      $ fmap text components
                      )
        when doPlan printInstallPlan
      _ ->
        putStrLn "  Multiple packages in the current directory."
  printInstallPlan = do
    putStrLn "Install plan:"
    installedPkgIndex <- getInstalledPackages
                           verbosity
                           comp
                           dbs
                           conf
    sourcePkgDb       <- withRepoContext verbosity
                                         globalFlags'
                                         (getSourcePackages verbosity)
    pkgConfigDb       <- readPkgConfigDb verbosity conf
    pkgSpecifiers     <- withRepoContext verbosity
                                         globalFlags'
                         $ \repoContext -> resolveUserTargets
                             verbosity
                             repoContext
                             (Cabal.fromFlag $ globalWorldFile globalFlags')
                             (packageIndex sourcePkgDb)
                             [UserTargetLocalDir "."]
    maybeWithSandboxPackageInfo
      (lessVerbose verbosity)
      (savedConfigureFlags config)
      globalFlags'
      comp
      platform
      conf
      useSandbox $ \mSandboxPkgInfo ->
        maybeWithSandboxDirOnSearchPath useSandbox $ do
          planPkgs <- planPackages
            (lessVerbose verbosity)
            comp
            platform
            mSandboxPkgInfo
            defaultFreezeFlags
            installedPkgIndex
            sourcePkgDb
            pkgConfigDb
            pkgSpecifiers
          planPkgs `forM_` \pkg ->
            let pid = (packageId pkg)
            in putStrLn $ "  " ++ display (packageName pid)
                               ++ " == "
                               ++ showVersion (packageVersion pid)
  printSandboxInformation = case (sandboxPath, sandboxDbPath) of
    (Just path, Just dbpath) -> do
      putStrLn $ "Sandbox: Present in current directory:"
      putStrLn $ "  config  in " ++ sandboxConfigPath
      putStrLn $ "  sandbox in " ++ path
      putStrLn $ "  sandbox package db in " ++ dbpath
      -- TODO: (optionally) print add-source directories (?)
    _ -> do
      let (dir,fname) = splitFileName sandboxConfigPath
          dirs = splitPath dir
      r <- if length dirs <= 1
        then return Nothing
        -- test if there is a sandbox is any of the parents
        else fmap asum
          $ ( fmap joinPath
            $ reverse
            $ inits
            $ init dirs
            ) `forM` \path -> -- doesnt exist -> Nothing; otherwise Just path.
                              fmap (fmap (const path) . guard)
                            $ doesFileExist
                            $ combine path fname
      putStr "Sandbox: Not present in current directory."
      case r of
        Nothing -> putStrLn $ "."
        Just op -> putStrLn $ ";\n  But there is a sandbox in parent:\n  "
                            ++ op
  printPackageDbs = do
    installedPackageIndex <- getInstalledPackages verbosity comp dbs conf
    let pkgs = allPackages installedPackageIndex
        pkgTuples = [ (root, disp $ sourcePackageId pkg)
                    | pkg <- pkgs
                    , root <- [x | Just x <- [pkgRoot pkg]]
                    ]
        grouped = groupBy (\a b -> fst a == fst b)
                $ sortBy (comparing fst)
                $ pkgTuples
        groups = [(fst (head x), map snd x) | x <- grouped]
    let packageDoc ps = if    Cabal.fromFlag (statusPkgDbs statusFlags)
                           || Cabal.fromFlag (statusAll statusFlags)
          then nest 2 (   text "("
                       <> vcat (punctuate (text ",") ps)
                       <> text ")")
          else empty
    putStrLn $ render
             $ (  text "Package databases:"
               $$ ( nest 2
                  $ vcat
                  $ flip fmap groups
                  $ \(root, ps) ->
                          (   text (show (length ps))
                          <+> text "packages in database"
                          <+> text root)
                      $+$
                          packageDoc ps))
  printPackageDBChecks = do
    putStrLn $ "Package database checks:"
    checks <- checkPackageDBs verbosity comp dbs conf
    flip mapM_ checks $ \(db, output) -> if null output
      then putStrLn $ "  clean for database " ++ show db ++ "."
      else do
        putStrLn $ "  " ++ show db ++ ":"
        mapM_ putStrLn output
  }
