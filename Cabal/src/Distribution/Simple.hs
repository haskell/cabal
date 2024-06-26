{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-----------------------------------------------------------------------------
{-
Work around this warning:
libraries/Cabal/Distribution/Simple.hs:78:0:
    Warning: In the use of `runTests'
             (imported from Distribution.Simple.UserHooks):
             Deprecated: "Please use the new testing interface instead!"
-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Module      :  Distribution.Simple
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the command line front end to the Simple build system. When given
-- the parsed command-line args and package information, is able to perform
-- basic commands like configure, build, install, register, etc.
--
-- This module exports the main functions that Setup.hs scripts use. It
-- re-exports the 'UserHooks' type, the standard entry points like
-- 'defaultMain' and 'defaultMainWithHooks' and the predefined sets of
-- 'UserHooks' that custom @Setup.hs@ scripts can extend to add their own
-- behaviour.
--
-- This module isn't called \"Simple\" because it's simple.  Far from
-- it.  It's called \"Simple\" because it does complicated things to
-- simple software.
--
-- The original idea was that there could be different build systems that all
-- presented the same compatible command line interfaces. There is still a
-- "Distribution.Make" system but in practice no packages use it.
module Distribution.Simple
  ( module Distribution.Package
  , module Distribution.Version
  , module Distribution.License
  , module Distribution.Simple.Compiler
  , module Language.Haskell.Extension

    -- * Simple interface
  , defaultMain
  , defaultMainNoRead
  , defaultMainArgs

    -- * Customization
  , UserHooks (..)
  , Args
  , defaultMainWithHooks
  , defaultMainWithSetupHooks
  , defaultMainWithSetupHooksArgs
  , defaultMainWithHooksArgs
  , defaultMainWithHooksNoRead
  , defaultMainWithHooksNoReadArgs

    -- ** Standard sets of hooks
  , simpleUserHooks
  , autoconfUserHooks
  , autoconfSetupHooks
  , emptyUserHooks
  ) where

import Control.Exception (try)

import Distribution.Compat.Prelude
import Distribution.Compat.ResponseFile (expandResponse)
import Prelude ()

-- local

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Command
import Distribution.Simple.Compiler
import Distribution.Simple.PackageDescription
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup
import qualified Distribution.Simple.SetupHooks.Internal as SetupHooks
import Distribution.Simple.UserHooks

import Distribution.Simple.Build
import Distribution.Simple.Register
import Distribution.Simple.SrcDist

import Distribution.Simple.Configure

import Distribution.License
import Distribution.Pretty
import Distribution.Simple.Bench
import Distribution.Simple.BuildPaths
import Distribution.Simple.ConfigureScript (runConfigureScript)
import Distribution.Simple.Errors
import Distribution.Simple.Haddock
import Distribution.Simple.Install
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.SetupHooks.Internal
  ( SetupHooks
  )
import Distribution.Simple.Test
import Distribution.Simple.Utils
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension

-- Base
import Data.List (unionBy, (\\))
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (getArgs, getProgName)

-- | A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = getArgs >>= defaultMainHelper simpleUserHooks

-- | A version of 'defaultMain' that is passed the command line
-- arguments, rather than getting them from the environment.
defaultMainArgs :: [String] -> IO ()
defaultMainArgs = defaultMainHelper simpleUserHooks

defaultMainWithSetupHooks :: SetupHooks -> IO ()
defaultMainWithSetupHooks setup_hooks =
  getArgs >>= defaultMainWithSetupHooksArgs setup_hooks

defaultMainWithSetupHooksArgs :: SetupHooks -> [String] -> IO ()
defaultMainWithSetupHooksArgs setupHooks =
  defaultMainHelper $
    simpleUserHooks
      { confHook = setup_confHook
      , buildHook = setup_buildHook
      , copyHook = setup_copyHook
      , instHook = setup_installHook
      , replHook = setup_replHook
      , haddockHook = setup_haddockHook
      , hscolourHook = setup_hscolourHook
      }
  where
    setup_confHook
      :: (GenericPackageDescription, HookedBuildInfo)
      -> ConfigFlags
      -> IO LocalBuildInfo
    setup_confHook =
      configure_setupHooks
        (SetupHooks.configureHooks setupHooks)

    setup_buildHook
      :: PackageDescription
      -> LocalBuildInfo
      -> UserHooks
      -> BuildFlags
      -> IO ()
    setup_buildHook pkg_descr lbi hooks flags =
      build_setupHooks
        (SetupHooks.buildHooks setupHooks)
        pkg_descr
        lbi
        flags
        (allSuffixHandlers hooks)

    setup_copyHook
      :: PackageDescription
      -> LocalBuildInfo
      -> UserHooks
      -> CopyFlags
      -> IO ()
    setup_copyHook pkg_descr lbi _hooks flags =
      install_setupHooks
        (SetupHooks.installHooks setupHooks)
        pkg_descr
        lbi
        flags

    setup_installHook
      :: PackageDescription
      -> LocalBuildInfo
      -> UserHooks
      -> InstallFlags
      -> IO ()
    setup_installHook =
      defaultInstallHook_setupHooks
        (SetupHooks.installHooks setupHooks)

    setup_replHook
      :: PackageDescription
      -> LocalBuildInfo
      -> UserHooks
      -> ReplFlags
      -> [String]
      -> IO ()
    setup_replHook pkg_descr lbi hooks flags args =
      repl_setupHooks
        (SetupHooks.buildHooks setupHooks)
        pkg_descr
        lbi
        flags
        (allSuffixHandlers hooks)
        args

    setup_haddockHook
      :: PackageDescription
      -> LocalBuildInfo
      -> UserHooks
      -> HaddockFlags
      -> IO ()
    setup_haddockHook pkg_descr lbi hooks flags =
      haddock_setupHooks
        (SetupHooks.buildHooks setupHooks)
        pkg_descr
        lbi
        (allSuffixHandlers hooks)
        flags

    setup_hscolourHook
      :: PackageDescription
      -> LocalBuildInfo
      -> UserHooks
      -> HscolourFlags
      -> IO ()
    setup_hscolourHook pkg_descr lbi hooks flags =
      hscolour_setupHooks
        (SetupHooks.buildHooks setupHooks)
        pkg_descr
        lbi
        (allSuffixHandlers hooks)
        flags

-- | A customizable version of 'defaultMain'.
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks hooks = getArgs >>= defaultMainHelper hooks

-- | A customizable version of 'defaultMain' that also takes the command
-- line arguments.
defaultMainWithHooksArgs :: UserHooks -> [String] -> IO ()
defaultMainWithHooksArgs = defaultMainHelper

-- | Like 'defaultMain', but accepts the package description as input
-- rather than using IO to read it.
defaultMainNoRead :: GenericPackageDescription -> IO ()
defaultMainNoRead = defaultMainWithHooksNoRead simpleUserHooks

-- | A customizable version of 'defaultMainNoRead'.
defaultMainWithHooksNoRead :: UserHooks -> GenericPackageDescription -> IO ()
defaultMainWithHooksNoRead hooks pkg_descr =
  getArgs
    >>= defaultMainHelper hooks{readDesc = return (Just pkg_descr)}

-- | A customizable version of 'defaultMainNoRead' that also takes the
-- command line arguments.
--
-- @since 2.2.0.0
defaultMainWithHooksNoReadArgs :: UserHooks -> GenericPackageDescription -> [String] -> IO ()
defaultMainWithHooksNoReadArgs hooks pkg_descr =
  defaultMainHelper hooks{readDesc = return (Just pkg_descr)}

-- | The central command chooser of the Simple build system,
-- with other defaultMain functions acting as exposed callers,
-- and with 'topHandler' operating as an exceptions handler.
--
-- This uses 'expandResponse' to read response files, preprocessing
-- response files given by "@" prefixes.
--
-- Given hooks and args, this runs 'commandsRun' onto the args,
-- getting 'CommandParse' data back, which is then pattern-matched into
-- IO actions for execution, with arguments applied by the parser.
defaultMainHelper :: UserHooks -> Args -> IO ()
defaultMainHelper hooks args = topHandler $ do
  args' <- expandResponse args
  command <- commandsRun (globalCommand commands) commands args'
  case command of
    CommandHelp help -> printHelp help
    CommandList opts -> printOptionsList opts
    CommandErrors errs -> printErrors errs
    CommandReadyToGo (globalFlags, commandParse) ->
      case commandParse of
        _
          | fromFlag (globalVersion globalFlags) -> printVersion
          | fromFlag (globalNumericVersion globalFlags) -> printNumericVersion
        CommandHelp help -> printHelp help
        CommandList opts -> printOptionsList opts
        CommandErrors errs -> printErrors errs
        CommandReadyToGo action -> action globalFlags
  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (intercalate "\n" errs)
      exitWith (ExitFailure 1)
    printNumericVersion = putStrLn $ prettyShow cabalVersion
    printVersion =
      putStrLn $
        "Cabal library version "
          ++ prettyShow cabalVersion

    progs = addKnownPrograms (hookedPrograms hooks) defaultProgramDb
    addAction :: CommandUI flags -> (GlobalFlags -> UserHooks -> flags -> [String] -> IO res) -> Command (GlobalFlags -> IO ())
    addAction cmd action =
      cmd `commandAddAction` \flags as globalFlags -> void $ action globalFlags hooks flags as
    commands :: [Command (GlobalFlags -> IO ())]
    commands =
      [ configureCommand progs `addAction` configureAction
      , buildCommand progs `addAction` buildAction
      , replCommand progs `addAction` replAction
      , installCommand `addAction` installAction
      , copyCommand `addAction` copyAction
      , haddockCommand `addAction` haddockAction
      , cleanCommand `addAction` cleanAction
      , sdistCommand `addAction` sdistAction
      , hscolourCommand `addAction` hscolourAction
      , registerCommand `addAction` registerAction
      , unregisterCommand `addAction` unregisterAction
      , testCommand `addAction` testAction
      , benchmarkCommand `addAction` benchAction
      ]

-- | Combine the preprocessors in the given hooks with the
-- preprocessors built into cabal.
allSuffixHandlers
  :: UserHooks
  -> [PPSuffixHandler]
allSuffixHandlers hooks =
  overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
  where
    overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
    overridesPP = unionBy (\x y -> fst x == fst y)

configureAction :: GlobalFlags -> UserHooks -> ConfigFlags -> Args -> IO LocalBuildInfo
configureAction globalFlags hooks flags args = do
  distPref <- findDistPrefOrDefault (setupDistPref $ configCommonFlags flags)
  let commonFlags = configCommonFlags flags
      commonFlags' =
        commonFlags
          { setupDistPref = toFlag distPref
          , setupWorkingDir = globalWorkingDir globalFlags <> setupWorkingDir commonFlags
          , setupTargets = args
          }
      flags' =
        flags
          { configCommonFlags = commonFlags'
          }
      mbWorkDir = flagToMaybe $ setupWorkingDir commonFlags'
      verbosity = fromFlag $ setupVerbosity commonFlags'

  -- See docs for 'HookedBuildInfo'
  pbi <- preConf hooks args flags'

  (mb_pd_file, pkg_descr0) <-
    confPkgDescr
      hooks
      verbosity
      mbWorkDir
      (flagToMaybe (setupCabalFilePath commonFlags'))

  let epkg_descr = (pkg_descr0, pbi)

  lbi1 <- confHook hooks epkg_descr flags'

  -- remember the .cabal filename if we know it
  -- and all the extra command line args
  let localbuildinfo =
        lbi1
          { pkgDescrFile = mb_pd_file
          , extraConfigArgs = args
          }
  writePersistBuildConfig mbWorkDir distPref localbuildinfo

  let pkg_descr = localPkgDescr localbuildinfo
  postConf hooks args flags' pkg_descr localbuildinfo
  return localbuildinfo

confPkgDescr
  :: UserHooks
  -> Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> Maybe (SymbolicPath Pkg File)
  -> IO (Maybe (SymbolicPath Pkg File), GenericPackageDescription)
confPkgDescr hooks verbosity cwd mb_path = do
  mdescr <- readDesc hooks
  case mdescr of
    Just descr -> return (Nothing, descr)
    Nothing -> do
      pdfile <- case mb_path of
        Nothing -> relativeSymbolicPath <$> tryFindPackageDesc verbosity cwd
        Just path -> return path
      info verbosity "Using Parsec parser"
      descr <- readGenericPackageDescription verbosity cwd pdfile
      return (Just pdfile, descr)

getCommonFlags
  :: GlobalFlags
  -> UserHooks
  -> CommonSetupFlags
  -> Args
  -> IO (LocalBuildInfo, CommonSetupFlags)
getCommonFlags globalFlags hooks commonFlags args = do
  distPref <- findDistPrefOrDefault (setupDistPref commonFlags)
  let verbosity = fromFlag $ setupVerbosity commonFlags
  lbi <- getBuildConfig globalFlags hooks verbosity distPref
  let common' = configCommonFlags $ configFlags lbi
  return $
    ( lbi
    , commonFlags
        { setupDistPref = toFlag distPref
        , setupCabalFilePath = setupCabalFilePath common' <> setupCabalFilePath commonFlags
        , setupWorkingDir =
            globalWorkingDir globalFlags
              <> setupWorkingDir common'
              <> setupWorkingDir commonFlags
        , setupTargets = args
        }
    )

buildAction :: GlobalFlags -> UserHooks -> BuildFlags -> Args -> IO ()
buildAction globalFlags hooks flags args = do
  let common = buildCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{buildCommonFlags = common'}

  progs <-
    reconfigurePrograms
      verbosity
      (buildProgramPaths flags')
      (buildProgramArgs flags')
      (withPrograms lbi)

  hookedAction
    verbosity
    preBuild
    buildHook
    postBuild
    (return lbi{withPrograms = progs})
    hooks
    flags'
    args

replAction :: GlobalFlags -> UserHooks -> ReplFlags -> Args -> IO ()
replAction globalFlags hooks flags args = do
  let common = replCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{replCommonFlags = common'}
  progs <-
    reconfigurePrograms
      verbosity
      (replProgramPaths flags')
      (replProgramArgs flags')
      (withPrograms lbi)

  -- As far as I can tell, the only reason this doesn't use
  -- 'hookedActionWithArgs' is because the arguments of 'replHook'
  -- takes the args explicitly.  UGH.   -- ezyang
  pbi <- preRepl hooks args flags'
  let pkg_descr0 = localPkgDescr lbi
  sanityCheckHookedBuildInfo verbosity pkg_descr0 pbi
  let pkg_descr = updatePackageDescription pbi pkg_descr0
      lbi' =
        lbi
          { withPrograms = progs
          , localPkgDescr = pkg_descr
          }
  replHook hooks pkg_descr lbi' hooks flags' args
  postRepl hooks args flags' pkg_descr lbi'

hscolourAction :: GlobalFlags -> UserHooks -> HscolourFlags -> Args -> IO ()
hscolourAction globalFlags hooks flags args = do
  let common = hscolourCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{hscolourCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'

  hookedAction
    verbosity
    preHscolour
    hscolourHook
    postHscolour
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

haddockAction :: GlobalFlags -> UserHooks -> HaddockFlags -> Args -> IO ()
haddockAction globalFlags hooks flags args = do
  let common = haddockCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{haddockCommonFlags = common'}

  progs <-
    reconfigurePrograms
      verbosity
      (haddockProgramPaths flags')
      (haddockProgramArgs flags')
      (withPrograms lbi)

  hookedAction
    verbosity
    preHaddock
    haddockHook
    postHaddock
    (return lbi{withPrograms = progs})
    hooks
    flags'
    args

cleanAction :: GlobalFlags -> UserHooks -> CleanFlags -> Args -> IO ()
cleanAction globalFlags hooks flags args = do
  let common = cleanCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  distPref <- findDistPrefOrDefault (setupDistPref common)
  elbi <- tryGetBuildConfig globalFlags hooks verbosity distPref
  let common' =
        common
          { setupDistPref = toFlag distPref
          , setupWorkingDir = case elbi of
              Left _ ->
                globalWorkingDir globalFlags
                  <> setupWorkingDir common
              Right lbi ->
                globalWorkingDir globalFlags
                  <> setupWorkingDir (configCommonFlags $ configFlags lbi)
                  <> setupWorkingDir common
          , setupCabalFilePath = case elbi of
              Left _ -> setupCabalFilePath common
              Right lbi ->
                setupCabalFilePath common
                  <> setupCabalFilePath (configCommonFlags $ configFlags lbi)
          , setupTargets = args
          }
      flags' =
        flags{cleanCommonFlags = common'}

      mbWorkDirFlag = cleanWorkingDir flags
      mbWorkDir = flagToMaybe mbWorkDirFlag

  pbi <- preClean hooks args flags'

  (_, ppd) <- confPkgDescr hooks verbosity mbWorkDir Nothing
  -- It might seem like we are doing something clever here
  -- but we're really not: if you look at the implementation
  -- of 'clean' in the end all the package description is
  -- used for is to clear out @extra-tmp-files@.  IMO,
  -- the configure script goo should go into @dist@ too!
  --          -- ezyang
  let pkg_descr0 = flattenPackageDescription ppd
  -- We don't sanity check for clean as an error
  -- here would prevent cleaning:
  -- sanityCheckHookedBuildInfo verbosity pkg_descr0 pbi
  let pkg_descr = updatePackageDescription pbi pkg_descr0

  cleanHook hooks pkg_descr () hooks flags'
  postClean hooks args flags' pkg_descr ()

copyAction :: GlobalFlags -> UserHooks -> CopyFlags -> Args -> IO ()
copyAction globalFlags hooks flags args = do
  let common = copyCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{copyCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'
  hookedAction
    verbosity
    preCopy
    copyHook
    postCopy
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

installAction :: GlobalFlags -> UserHooks -> InstallFlags -> Args -> IO ()
installAction globalFlags hooks flags args = do
  let common = installCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{installCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'
  hookedAction
    verbosity
    preInst
    instHook
    postInst
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

-- Since Cabal-3.4 UserHooks are completely ignored
sdistAction :: GlobalFlags -> UserHooks -> SDistFlags -> Args -> IO ()
sdistAction _globalFlags _hooks flags _args = do
  let mbWorkDir = flagToMaybe $ sDistWorkingDir flags
  (_, ppd) <- confPkgDescr emptyUserHooks verbosity mbWorkDir Nothing
  let pkg_descr = flattenPackageDescription ppd
  sdist pkg_descr flags srcPref knownSuffixHandlers
  where
    verbosity = fromFlag (setupVerbosity $ sDistCommonFlags flags)

testAction :: GlobalFlags -> UserHooks -> TestFlags -> Args -> IO ()
testAction globalFlags hooks flags args = do
  let common = testCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{testCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'
  hookedActionWithArgs
    verbosity
    preTest
    testHook
    postTest
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

benchAction :: GlobalFlags -> UserHooks -> BenchmarkFlags -> Args -> IO ()
benchAction globalFlags hooks flags args = do
  let common = benchmarkCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{benchmarkCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'
  hookedActionWithArgs
    verbosity
    preBench
    benchHook
    postBench
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

registerAction :: GlobalFlags -> UserHooks -> RegisterFlags -> Args -> IO ()
registerAction globalFlags hooks flags args = do
  let common = registerCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{registerCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'
  hookedAction
    verbosity
    preReg
    regHook
    postReg
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

unregisterAction :: GlobalFlags -> UserHooks -> RegisterFlags -> Args -> IO ()
unregisterAction globalFlags hooks flags args = do
  let common = registerCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
  (_lbi, common') <- getCommonFlags globalFlags hooks common args
  let flags' = flags{registerCommonFlags = common'}
      distPref = fromFlag $ setupDistPref common'
  hookedAction
    verbosity
    preUnreg
    unregHook
    postUnreg
    (getBuildConfig globalFlags hooks verbosity distPref)
    hooks
    flags'
    args

hookedAction
  :: Verbosity
  -> (UserHooks -> Args -> flags -> IO HookedBuildInfo)
  -> ( UserHooks
       -> PackageDescription
       -> LocalBuildInfo
       -> UserHooks
       -> flags
       -> IO ()
     )
  -> ( UserHooks
       -> Args
       -> flags
       -> PackageDescription
       -> LocalBuildInfo
       -> IO ()
     )
  -> IO LocalBuildInfo
  -> UserHooks
  -> flags
  -> Args
  -> IO ()
hookedAction verbosity pre_hook cmd_hook =
  hookedActionWithArgs
    verbosity
    pre_hook
    ( \h _ pd lbi uh flags ->
        cmd_hook h pd lbi uh flags
    )

hookedActionWithArgs
  :: Verbosity
  -> (UserHooks -> Args -> flags -> IO HookedBuildInfo)
  -> ( UserHooks
       -> Args
       -> PackageDescription
       -> LocalBuildInfo
       -> UserHooks
       -> flags
       -> IO ()
     )
  -> ( UserHooks
       -> Args
       -> flags
       -> PackageDescription
       -> LocalBuildInfo
       -> IO ()
     )
  -> IO LocalBuildInfo
  -> UserHooks
  -> flags
  -> Args
  -> IO ()
hookedActionWithArgs
  verbosity
  pre_hook
  cmd_hook
  post_hook
  get_build_config
  hooks
  flags
  args = do
    pbi <- pre_hook hooks args flags
    lbi0 <- get_build_config
    let pkg_descr0 = localPkgDescr lbi0
    sanityCheckHookedBuildInfo verbosity pkg_descr0 pbi
    let pkg_descr = updatePackageDescription pbi pkg_descr0
        lbi = lbi0{localPkgDescr = pkg_descr}
    cmd_hook hooks args pkg_descr lbi hooks flags
    post_hook hooks args flags pkg_descr lbi

sanityCheckHookedBuildInfo
  :: Verbosity -> PackageDescription -> HookedBuildInfo -> IO ()
sanityCheckHookedBuildInfo
  verbosity
  (PackageDescription{library = Nothing})
  (Just _, _) =
    dieWithException verbosity $ NoLibraryForPackage
sanityCheckHookedBuildInfo verbosity pkg_descr (_, hookExes)
  | exe1 : _ <- nonExistant =
      dieWithException verbosity $ SanityCheckHookedBuildInfo exe1
  where
    pkgExeNames = nub (map exeName (executables pkg_descr))
    hookExeNames = nub (map fst hookExes)
    nonExistant = hookExeNames \\ pkgExeNames
sanityCheckHookedBuildInfo _ _ _ = return ()

-- | Try to read the 'localBuildInfoFile'
tryGetBuildConfig
  :: GlobalFlags
  -> UserHooks
  -> Verbosity
  -> SymbolicPath Pkg (Dir Dist)
  -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetBuildConfig g u v = try . getBuildConfig g u v

-- | Read the 'localBuildInfoFile' or throw an exception.
getBuildConfig
  :: GlobalFlags
  -> UserHooks
  -> Verbosity
  -> SymbolicPath Pkg (Dir Dist)
  -> IO LocalBuildInfo
getBuildConfig globalFlags hooks verbosity distPref = do
  lbi_wo_programs <- getPersistBuildConfig mbWorkDir distPref
  -- Restore info about unconfigured programs, since it is not serialized
  let lbi =
        lbi_wo_programs
          { withPrograms =
              restoreProgramDb
                (builtinPrograms ++ hookedPrograms hooks)
                (withPrograms lbi_wo_programs)
          }

  case pkgDescrFile lbi of
    Nothing -> return lbi
    Just pkg_descr_file -> do
      outdated <- checkPersistBuildConfigOutdated mbWorkDir distPref pkg_descr_file
      if outdated
        then reconfigure pkg_descr_file lbi
        else return lbi
  where
    mbWorkDir = flagToMaybe $ globalWorkingDir globalFlags
    reconfigure :: SymbolicPath Pkg File -> LocalBuildInfo -> IO LocalBuildInfo
    reconfigure pkg_descr_file lbi = do
      notice verbosity $
        getSymbolicPath pkg_descr_file
          ++ " has been changed. "
          ++ "Re-configuring with most recently used options. "
          ++ "If this fails, please run configure manually.\n"
      let cFlags = configFlags lbi
      let cFlags' =
            cFlags
              { -- Since the list of unconfigured programs is not serialized,
                -- restore it to the same value as normally used at the beginning
                -- of a configure run:
                configPrograms_ =
                  fmap
                    ( restoreProgramDb
                        (builtinPrograms ++ hookedPrograms hooks)
                    )
                    `fmap` configPrograms_ cFlags
              , configCommonFlags =
                  (configCommonFlags cFlags)
                    { -- Use the current, not saved verbosity level:
                      setupVerbosity = Flag verbosity
                    }
              }
      configureAction globalFlags hooks cFlags' (extraConfigArgs lbi)

-- --------------------------------------------------------------------------
-- Cleaning

clean :: PackageDescription -> CleanFlags -> IO ()
clean pkg_descr flags = do
  let common = cleanCommonFlags flags
      verbosity = fromFlag (setupVerbosity common)
      distPref = fromFlagOrDefault defaultDistPref $ setupDistPref common
      mbWorkDir = flagToMaybe $ setupWorkingDir common
      i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path
      distPath = i distPref
  notice verbosity "cleaning..."

  maybeConfig <-
    if fromFlag (cleanSaveConf flags)
      then maybeGetPersistBuildConfig mbWorkDir distPref
      else return Nothing

  -- remove the whole dist/ directory rather than tracking exactly what files
  -- we created in there.
  chattyTry "removing dist/" $ do
    exists <- doesDirectoryExist distPath
    when exists (removeDirectoryRecursive distPath)

  -- Any extra files the user wants to remove
  traverse_ (removeFileOrDirectory . i) (extraTmpFiles pkg_descr)

  -- If the user wanted to save the config, write it back
  traverse_ (writePersistBuildConfig mbWorkDir distPref) maybeConfig
  where
    removeFileOrDirectory :: FilePath -> IO ()
    removeFileOrDirectory fname = do
      isDir <- doesDirectoryExist fname
      isFile <- doesFileExist fname
      if isDir
        then removeDirectoryRecursive fname
        else when isFile $ removeFile fname

-- --------------------------------------------------------------------------
-- Default hooks

-- | Hooks that correspond to a plain instantiation of the
-- \"simple\" build system
simpleUserHooks :: UserHooks
simpleUserHooks =
  emptyUserHooks
    { confHook = configure
    , postConf = finalChecks
    , buildHook = defaultBuildHook
    , replHook = defaultReplHook
    , copyHook = \desc lbi _ f -> install desc lbi f
    , -- 'install' has correct 'copy' behavior with params
      instHook = defaultInstallHook
    , testHook = defaultTestHook
    , benchHook = defaultBenchHook
    , cleanHook = \p _ _ f -> clean p f
    , hscolourHook = \p l h f -> hscolour p l (allSuffixHandlers h) f
    , haddockHook = \p l h f -> haddock p l (allSuffixHandlers h) f
    , regHook = defaultRegHook
    , unregHook = \p l _ f -> unregister p l f
    }
  where
    finalChecks _args flags pkg_descr lbi =
      checkForeignDeps pkg_descr lbi (lessVerbose verbosity)
      where
        verbosity = fromFlag (setupVerbosity $ configCommonFlags flags)

-- | Basic autoconf 'UserHooks':
--
-- * 'postConf' runs @.\/configure@, if present.
--
-- * the pre-hooks, except for pre-conf, read additional build information from
--   /package/@.buildinfo@, if present.
--
-- Thus @configure@ can use local system information to generate
-- /package/@.buildinfo@ and possibly other files.
autoconfUserHooks :: UserHooks
autoconfUserHooks =
  simpleUserHooks
    { postConf = defaultPostConf
    , preBuild = readHookWithArgs buildCommonFlags
    , preRepl = readHookWithArgs replCommonFlags
    , preCopy = readHookWithArgs copyCommonFlags
    , preClean = readHook cleanCommonFlags
    , preInst = readHook installCommonFlags
    , preHscolour = readHook hscolourCommonFlags
    , preHaddock = readHookWithArgs haddockCommonFlags
    , preReg = readHook registerCommonFlags
    , preUnreg = readHook registerCommonFlags
    , preTest = readHookWithArgs testCommonFlags
    , preBench = readHookWithArgs benchmarkCommonFlags
    }
  where
    defaultPostConf
      :: Args
      -> ConfigFlags
      -> PackageDescription
      -> LocalBuildInfo
      -> IO ()
    defaultPostConf args flags pkg_descr lbi =
      do
        let common = configCommonFlags flags
            verbosity = fromFlag $ setupVerbosity common
            mbWorkDir = flagToMaybe $ setupWorkingDir common
        runConfigureScript
          flags
          (flagAssignment lbi)
          (withPrograms lbi)
          (hostPlatform lbi)
        pbi <- getHookedBuildInfo verbosity mbWorkDir (buildDir lbi)
        sanityCheckHookedBuildInfo verbosity pkg_descr pbi
        let pkg_descr' = updatePackageDescription pbi pkg_descr
            lbi' = lbi{localPkgDescr = pkg_descr'}
        postConf simpleUserHooks args flags pkg_descr' lbi'

    readHookWithArgs
      :: (flags -> CommonSetupFlags)
      -> Args
      -> flags
      -> IO HookedBuildInfo
    readHookWithArgs get_common_flags _args flags = do
      let common = get_common_flags flags
          verbosity = fromFlag (setupVerbosity common)
          mbWorkDir = flagToMaybe $ setupWorkingDir common
          distPref = setupDistPref common
      dist_dir <- findDistPrefOrDefault distPref
      getHookedBuildInfo verbosity mbWorkDir (dist_dir </> makeRelativePathEx "build")

    readHook
      :: (flags -> CommonSetupFlags)
      -> Args
      -> flags
      -> IO HookedBuildInfo
    readHook get_common_flags args flags = do
      let common = get_common_flags flags
          verbosity = fromFlag (setupVerbosity common)
          mbWorkDir = flagToMaybe $ setupWorkingDir common
          distPref = setupDistPref common
      noExtraFlags args
      dist_dir <- findDistPrefOrDefault distPref
      getHookedBuildInfo verbosity mbWorkDir (dist_dir </> makeRelativePathEx "build")

getHookedBuildInfo
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Build)
  -> IO HookedBuildInfo
getHookedBuildInfo verbosity mbWorkDir build_dir = do
  maybe_infoFile <- findHookedPackageDesc verbosity mbWorkDir build_dir
  case maybe_infoFile of
    Nothing -> return emptyHookedBuildInfo
    Just infoFile -> do
      info verbosity $ "Reading parameters from " ++ getSymbolicPath infoFile
      readHookedBuildInfo verbosity mbWorkDir infoFile

autoconfSetupHooks :: SetupHooks
autoconfSetupHooks =
  SetupHooks.noSetupHooks
    { SetupHooks.configureHooks =
        SetupHooks.noConfigureHooks
          { SetupHooks.postConfPackageHook = Just post_conf_pkg
          , SetupHooks.preConfComponentHook = Just pre_conf_comp
          }
    }
  where
    post_conf_pkg
      :: SetupHooks.PostConfPackageInputs
      -> IO ()
    post_conf_pkg
      ( SetupHooks.PostConfPackageInputs
          { SetupHooks.localBuildConfig =
            LBC.LocalBuildConfig{LBC.withPrograms = progs}
          , SetupHooks.packageBuildDescr =
            LBC.PackageBuildDescr
              { LBC.configFlags = cfg
              , LBC.flagAssignment = flags
              , LBC.hostPlatform = plat
              }
          }
        ) = runConfigureScript cfg flags progs plat

    pre_conf_comp
      :: SetupHooks.PreConfComponentInputs
      -> IO SetupHooks.PreConfComponentOutputs
    pre_conf_comp
      ( SetupHooks.PreConfComponentInputs
          { SetupHooks.packageBuildDescr =
            LBC.PackageBuildDescr
              { LBC.configFlags = cfg
              , localPkgDescr = pkg_descr
              }
          , SetupHooks.component = component
          }
        ) = do
        let verbosity = fromFlag $ configVerbosity cfg
            mbWorkDir = flagToMaybe $ configWorkingDir cfg
            distPref = configDistPref cfg
        dist_dir <- findDistPrefOrDefault distPref
        -- Read the ".buildinfo" file and use that to update
        -- the components (main library + executables only).
        hbi <- getHookedBuildInfo verbosity mbWorkDir (dist_dir </> makeRelativePathEx "build")
        sanityCheckHookedBuildInfo verbosity pkg_descr hbi
        -- SetupHooks TODO: we are reading getHookedBuildInfo once
        -- for each component. I think this is inherent to the SetupHooks
        -- approach.
        let comp_name = componentName component
        diff <- case SetupHooks.hookedBuildInfoComponentDiff_maybe hbi comp_name of
          Nothing -> return $ SetupHooks.emptyComponentDiff comp_name
          Just do_diff -> do_diff
        return $
          SetupHooks.PreConfComponentOutputs
            { SetupHooks.componentDiff = diff
            }

defaultTestHook
  :: Args
  -> PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> TestFlags
  -> IO ()
defaultTestHook args pkg_descr localbuildinfo _ flags =
  test args pkg_descr localbuildinfo flags

defaultBenchHook
  :: Args
  -> PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BenchmarkFlags
  -> IO ()
defaultBenchHook args pkg_descr localbuildinfo _ flags =
  bench args pkg_descr localbuildinfo flags

defaultInstallHook
  :: PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> InstallFlags
  -> IO ()
defaultInstallHook =
  defaultInstallHook_setupHooks SetupHooks.noInstallHooks

defaultInstallHook_setupHooks
  :: SetupHooks.InstallHooks
  -> PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> InstallFlags
  -> IO ()
defaultInstallHook_setupHooks inst_hooks pkg_descr localbuildinfo _ flags = do
  let copyFlags =
        defaultCopyFlags
          { copyDest = installDest flags
          , copyCommonFlags = installCommonFlags flags
          }
  install_setupHooks inst_hooks pkg_descr localbuildinfo copyFlags
  let registerFlags =
        defaultRegisterFlags
          { regInPlace = installInPlace flags
          , regPackageDB = installPackageDB flags
          }
  when (hasLibs pkg_descr) $
    register pkg_descr localbuildinfo registerFlags

defaultBuildHook
  :: PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags =
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)

defaultReplHook
  :: PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> ReplFlags
  -> [String]
  -> IO ()
defaultReplHook pkg_descr localbuildinfo hooks flags args =
  repl pkg_descr localbuildinfo flags (allSuffixHandlers hooks) args

defaultRegHook
  :: PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> RegisterFlags
  -> IO ()
defaultRegHook pkg_descr localbuildinfo _ flags =
  if hasLibs pkg_descr
    then register pkg_descr localbuildinfo flags
    else
      setupMessage
        (fromFlag (setupVerbosity $ registerCommonFlags flags))
        "Package contains no library to register:"
        (packageId pkg_descr)
