{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , defaultMainWithHooksArgs
  , defaultMainWithHooksNoRead
  , defaultMainWithHooksNoReadArgs

    -- ** Standard sets of hooks
  , simpleUserHooks
  , autoconfUserHooks
  , emptyUserHooks
  ) where

import Control.Exception (try)

import Distribution.Compat.Prelude
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
import Distribution.Simple.UserHooks

import Distribution.Simple.Build
import Distribution.Simple.Register
import Distribution.Simple.SrcDist

import Distribution.Simple.Configure

import Distribution.License
import Distribution.Pretty
import Distribution.Simple.Bench
import Distribution.Simple.BuildPaths
import Distribution.Simple.ConfigureScript
import Distribution.Simple.Errors
import Distribution.Simple.Haddock
import Distribution.Simple.Install
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Test
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension

-- Base

import Distribution.Compat.ResponseFile (expandResponse)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (getArgs, getProgName)
import System.FilePath (takeDirectory, (</>))

import Data.List (unionBy, (\\))

-- | A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = getArgs >>= defaultMainHelper simpleUserHooks

-- | A version of 'defaultMain' that is passed the command line
-- arguments, rather than getting them from the environment.
defaultMainArgs :: [String] -> IO ()
defaultMainArgs = defaultMainHelper simpleUserHooks

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

defaultMainHelper :: UserHooks -> Args -> IO ()
defaultMainHelper hooks args = topHandler $ do
  args' <- expandResponse args
  case commandsRun (globalCommand commands) commands args' of
    CommandHelp help -> printHelp help
    CommandList opts -> printOptionsList opts
    CommandErrors errs -> printErrors errs
    CommandReadyToGo (flags, commandParse) ->
      case commandParse of
        _
          | fromFlag (globalVersion flags) -> printVersion
          | fromFlag (globalNumericVersion flags) -> printNumericVersion
        CommandHelp help -> printHelp help
        CommandList opts -> printOptionsList opts
        CommandErrors errs -> printErrors errs
        CommandReadyToGo action -> action
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
    commands =
      [ configureCommand progs
          `commandAddAction` \fs as -> configureAction hooks fs as >> return ()
      , buildCommand progs `commandAddAction` buildAction hooks
      , replCommand progs `commandAddAction` replAction hooks
      , installCommand `commandAddAction` installAction hooks
      , copyCommand `commandAddAction` copyAction hooks
      , haddockCommand `commandAddAction` haddockAction hooks
      , cleanCommand `commandAddAction` cleanAction hooks
      , sdistCommand `commandAddAction` sdistAction hooks
      , hscolourCommand `commandAddAction` hscolourAction hooks
      , registerCommand `commandAddAction` registerAction hooks
      , unregisterCommand `commandAddAction` unregisterAction hooks
      , testCommand `commandAddAction` testAction hooks
      , benchmarkCommand `commandAddAction` benchAction hooks
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

configureAction :: UserHooks -> ConfigFlags -> Args -> IO LocalBuildInfo
configureAction hooks flags args = do
  distPref <- findDistPrefOrDefault (configDistPref flags)
  let flags' =
        flags
          { configDistPref = toFlag distPref
          , configArgs = args
          }

  -- See docs for 'HookedBuildInfo'
  pbi <- preConf hooks args flags'

  (mb_pd_file, pkg_descr0) <-
    confPkgDescr
      hooks
      verbosity
      (flagToMaybe (configCabalFilePath flags))

  let epkg_descr = (pkg_descr0, pbi)

  localbuildinfo0 <- confHook hooks epkg_descr flags'

  -- remember the .cabal filename if we know it
  -- and all the extra command line args
  let localbuildinfo =
        localbuildinfo0
          { pkgDescrFile = mb_pd_file
          , extraConfigArgs = args
          }
  writePersistBuildConfig distPref localbuildinfo

  let pkg_descr = localPkgDescr localbuildinfo
  postConf hooks args flags' pkg_descr localbuildinfo
  return localbuildinfo
  where
    verbosity = fromFlag (configVerbosity flags)

confPkgDescr
  :: UserHooks
  -> Verbosity
  -> Maybe FilePath
  -> IO (Maybe FilePath, GenericPackageDescription)
confPkgDescr hooks verbosity mb_path = do
  mdescr <- readDesc hooks
  case mdescr of
    Just descr -> return (Nothing, descr)
    Nothing -> do
      pdfile <- case mb_path of
        Nothing -> defaultPackageDesc verbosity
        Just path -> return path
      info verbosity "Using Parsec parser"
      descr <- readGenericPackageDescription verbosity pdfile
      return (Just pdfile, descr)

buildAction :: UserHooks -> BuildFlags -> Args -> IO ()
buildAction hooks flags args = do
  distPref <- findDistPrefOrDefault (buildDistPref flags)
  let verbosity = fromFlag $ buildVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { buildDistPref = toFlag distPref
          , buildCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }

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
    flags'{buildArgs = args}
    args

replAction :: UserHooks -> ReplFlags -> Args -> IO ()
replAction hooks flags args = do
  distPref <- findDistPrefOrDefault (replDistPref flags)
  let verbosity = fromFlag $ replVerbosity flags
      flags' = flags{replDistPref = toFlag distPref}

  lbi <- getBuildConfig hooks verbosity distPref
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

hscolourAction :: UserHooks -> HscolourFlags -> Args -> IO ()
hscolourAction hooks flags args = do
  distPref <- findDistPrefOrDefault (hscolourDistPref flags)
  let verbosity = fromFlag $ hscolourVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { hscolourDistPref = toFlag distPref
          , hscolourCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }

  hookedAction
    verbosity
    preHscolour
    hscolourHook
    postHscolour
    (getBuildConfig hooks verbosity distPref)
    hooks
    flags'
    args

haddockAction :: UserHooks -> HaddockFlags -> Args -> IO ()
haddockAction hooks flags args = do
  distPref <- findDistPrefOrDefault (haddockDistPref flags)
  let verbosity = fromFlag $ haddockVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { haddockDistPref = toFlag distPref
          , haddockCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }

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
    flags'{haddockArgs = args}
    args

cleanAction :: UserHooks -> CleanFlags -> Args -> IO ()
cleanAction hooks flags args = do
  distPref <- findDistPrefOrDefault (cleanDistPref flags)

  elbi <- tryGetBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { cleanDistPref = toFlag distPref
          , cleanCabalFilePath = case elbi of
              Left _ -> mempty
              Right lbi -> maybeToFlag (cabalFilePath lbi)
          }

  pbi <- preClean hooks args flags'

  (_, ppd) <- confPkgDescr hooks verbosity Nothing
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
  where
    verbosity = fromFlag (cleanVerbosity flags)

copyAction :: UserHooks -> CopyFlags -> Args -> IO ()
copyAction hooks flags args = do
  distPref <- findDistPrefOrDefault (copyDistPref flags)
  let verbosity = fromFlag $ copyVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { copyDistPref = toFlag distPref
          , copyCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }
  hookedAction
    verbosity
    preCopy
    copyHook
    postCopy
    (getBuildConfig hooks verbosity distPref)
    hooks
    flags'{copyArgs = args}
    args

installAction :: UserHooks -> InstallFlags -> Args -> IO ()
installAction hooks flags args = do
  distPref <- findDistPrefOrDefault (installDistPref flags)
  let verbosity = fromFlag $ installVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { installDistPref = toFlag distPref
          , installCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }
  hookedAction
    verbosity
    preInst
    instHook
    postInst
    (getBuildConfig hooks verbosity distPref)
    hooks
    flags'
    args

-- Since Cabal-3.4 UserHooks are completely ignored
sdistAction :: UserHooks -> SDistFlags -> Args -> IO ()
sdistAction _hooks flags _args = do
  (_, ppd) <- confPkgDescr emptyUserHooks verbosity Nothing
  let pkg_descr = flattenPackageDescription ppd
  sdist pkg_descr flags srcPref knownSuffixHandlers
  where
    verbosity = fromFlag (sDistVerbosity flags)

testAction :: UserHooks -> TestFlags -> Args -> IO ()
testAction hooks flags args = do
  distPref <- findDistPrefOrDefault (testDistPref flags)
  let verbosity = fromFlag $ testVerbosity flags
      flags' = flags{testDistPref = toFlag distPref}

  hookedActionWithArgs
    verbosity
    preTest
    testHook
    postTest
    (getBuildConfig hooks verbosity distPref)
    hooks
    flags'
    args

benchAction :: UserHooks -> BenchmarkFlags -> Args -> IO ()
benchAction hooks flags args = do
  distPref <- findDistPrefOrDefault (benchmarkDistPref flags)
  let verbosity = fromFlag $ benchmarkVerbosity flags
      flags' = flags{benchmarkDistPref = toFlag distPref}
  hookedActionWithArgs
    verbosity
    preBench
    benchHook
    postBench
    (getBuildConfig hooks verbosity distPref)
    hooks
    flags'
    args

registerAction :: UserHooks -> RegisterFlags -> Args -> IO ()
registerAction hooks flags args = do
  distPref <- findDistPrefOrDefault (regDistPref flags)
  let verbosity = fromFlag $ regVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { regDistPref = toFlag distPref
          , regCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }
  hookedAction
    verbosity
    preReg
    regHook
    postReg
    (getBuildConfig hooks verbosity distPref)
    hooks
    flags'{regArgs = args}
    args

unregisterAction :: UserHooks -> RegisterFlags -> Args -> IO ()
unregisterAction hooks flags args = do
  distPref <- findDistPrefOrDefault (regDistPref flags)
  let verbosity = fromFlag $ regVerbosity flags
  lbi <- getBuildConfig hooks verbosity distPref
  let flags' =
        flags
          { regDistPref = toFlag distPref
          , regCabalFilePath = maybeToFlag (cabalFilePath lbi)
          }
  hookedAction
    verbosity
    preUnreg
    unregHook
    postUnreg
    (getBuildConfig hooks verbosity distPref)
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
  :: UserHooks
  -> Verbosity
  -> FilePath
  -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetBuildConfig u v = try . getBuildConfig u v

-- | Read the 'localBuildInfoFile' or throw an exception.
getBuildConfig :: UserHooks -> Verbosity -> FilePath -> IO LocalBuildInfo
getBuildConfig hooks verbosity distPref = do
  lbi_wo_programs <- getPersistBuildConfig distPref
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
      outdated <- checkPersistBuildConfigOutdated distPref pkg_descr_file
      if outdated
        then reconfigure pkg_descr_file lbi
        else return lbi
  where
    reconfigure :: FilePath -> LocalBuildInfo -> IO LocalBuildInfo
    reconfigure pkg_descr_file lbi = do
      notice verbosity $
        pkg_descr_file
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
              , -- Use the current, not saved verbosity level:
                configVerbosity = Flag verbosity
              }
      configureAction hooks cFlags' (extraConfigArgs lbi)

-- --------------------------------------------------------------------------
-- Cleaning

clean :: PackageDescription -> CleanFlags -> IO ()
clean pkg_descr flags = do
  let distPref = fromFlagOrDefault defaultDistPref $ cleanDistPref flags
  notice verbosity "cleaning..."

  maybeConfig <-
    if fromFlag (cleanSaveConf flags)
      then maybeGetPersistBuildConfig distPref
      else return Nothing

  -- remove the whole dist/ directory rather than tracking exactly what files
  -- we created in there.
  chattyTry "removing dist/" $ do
    exists <- doesDirectoryExist distPref
    when exists (removeDirectoryRecursive distPref)

  -- Any extra files the user wants to remove
  traverse_ removeFileOrDirectory (extraTmpFiles pkg_descr)

  -- If the user wanted to save the config, write it back
  traverse_ (writePersistBuildConfig distPref) maybeConfig
  where
    removeFileOrDirectory :: FilePath -> IO ()
    removeFileOrDirectory fname = do
      isDir <- doesDirectoryExist fname
      isFile <- doesFileExist fname
      if isDir
        then removeDirectoryRecursive fname
        else when isFile $ removeFile fname
    verbosity = fromFlag (cleanVerbosity flags)

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
      testHook = defaultTestHook
    , benchHook = defaultBenchHook
    , instHook = defaultInstallHook
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
        verbosity = fromFlag (configVerbosity flags)

-- | Basic autoconf 'UserHooks':
--
-- * 'postConf' runs @.\/configure@, if present.
--
-- * the pre-hooks 'preBuild', 'preClean', 'preCopy', 'preInst',
--   'preReg' and 'preUnreg' read additional build information from
--   /package/@.buildinfo@, if present.
--
-- Thus @configure@ can use local system information to generate
-- /package/@.buildinfo@ and possibly other files.
autoconfUserHooks :: UserHooks
autoconfUserHooks =
  simpleUserHooks
    { postConf = defaultPostConf
    , preBuild = readHookWithArgs buildVerbosity buildDistPref -- buildCabalFilePath,
    , preCopy = readHookWithArgs copyVerbosity copyDistPref
    , preClean = readHook cleanVerbosity cleanDistPref
    , preInst = readHook installVerbosity installDistPref
    , preHscolour = readHook hscolourVerbosity hscolourDistPref
    , preHaddock = readHookWithArgs haddockVerbosity haddockDistPref
    , preReg = readHook regVerbosity regDistPref
    , preUnreg = readHook regVerbosity regDistPref
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
        let verbosity = fromFlag (configVerbosity flags)
            baseDir lbi' =
              fromMaybe
                ""
                (takeDirectory <$> cabalFilePath lbi')
        confExists <- doesFileExist $ (baseDir lbi) </> "configure"
        if confExists
          then
            runConfigureScript
              verbosity
              flags
              lbi
          else dieWithException verbosity ConfigureScriptNotFound

        pbi <- getHookedBuildInfo verbosity (buildDir lbi)
        sanityCheckHookedBuildInfo verbosity pkg_descr pbi
        let pkg_descr' = updatePackageDescription pbi pkg_descr
            lbi' = lbi{localPkgDescr = pkg_descr'}
        postConf simpleUserHooks args flags pkg_descr' lbi'

    readHookWithArgs
      :: (a -> Flag Verbosity)
      -> (a -> Flag FilePath)
      -> Args
      -> a
      -> IO HookedBuildInfo
    readHookWithArgs get_verbosity get_dist_pref _ flags = do
      dist_dir <- findDistPrefOrDefault (get_dist_pref flags)
      getHookedBuildInfo verbosity (dist_dir </> "build")
      where
        verbosity = fromFlag (get_verbosity flags)

    readHook
      :: (a -> Flag Verbosity)
      -> (a -> Flag FilePath)
      -> Args
      -> a
      -> IO HookedBuildInfo
    readHook get_verbosity get_dist_pref a flags = do
      noExtraFlags a
      dist_dir <- findDistPrefOrDefault (get_dist_pref flags)
      getHookedBuildInfo verbosity (dist_dir </> "build")
      where
        verbosity = fromFlag (get_verbosity flags)

getHookedBuildInfo :: Verbosity -> FilePath -> IO HookedBuildInfo
getHookedBuildInfo verbosity build_dir = do
  maybe_infoFile <- findHookedPackageDesc verbosity build_dir
  case maybe_infoFile of
    Nothing -> return emptyHookedBuildInfo
    Just infoFile -> do
      info verbosity $ "Reading parameters from " ++ infoFile
      readHookedBuildInfo verbosity infoFile

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
defaultInstallHook pkg_descr localbuildinfo _ flags = do
  let copyFlags =
        defaultCopyFlags
          { copyDistPref = installDistPref flags
          , copyDest = installDest flags
          , copyVerbosity = installVerbosity flags
          }
  install pkg_descr localbuildinfo copyFlags
  let registerFlags =
        defaultRegisterFlags
          { regDistPref = installDistPref flags
          , regInPlace = installInPlace flags
          , regPackageDB = installPackageDB flags
          , regVerbosity = installVerbosity flags
          }
  when (hasLibs pkg_descr) $ register pkg_descr localbuildinfo registerFlags

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
        (fromFlag (regVerbosity flags))
        "Package contains no library to register:"
        (packageId pkg_descr)
