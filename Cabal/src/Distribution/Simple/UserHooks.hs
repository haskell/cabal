{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.UserHooks
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines the API that @Setup.hs@ scripts can use to customise the way
-- the build works. This module just defines the 'UserHooks' type. The
-- predefined sets of hooks that implement the @Simple@, @Make@ and @Configure@
-- build systems are defined in "Distribution.Simple". The 'UserHooks' is a big
-- record of functions. There are 3 for each action, a pre, post and the action
-- itself. There are few other miscellaneous hooks, ones to extend the set of
-- programs and preprocessors and one to override the function used to read the
-- @.cabal@ file.
--
-- This hooks type is widely agreed to not be the right solution. Partly this
-- is because changes to it usually break custom @Setup.hs@ files and yet many
-- internal code changes do require changes to the hooks. For example we cannot
-- pass any extra parameters to most of the functions that implement the
-- various phases because it would involve changing the types of the
-- corresponding hook. At some point it will have to be replaced.
module Distribution.Simple.UserHooks
  ( UserHooks (..)
  , Args
  , emptyUserHooks
  ) where

import Distribution.Compat.Prelude hiding (getContents, putStr)
import Prelude ()

import Distribution.PackageDescription
import Distribution.Simple.Command
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup

type Args = [String]

-- | Hooks allow authors to add specific functionality before and after a
-- command is run, and also to specify additional preprocessors.
--
-- * WARNING: The hooks interface is under rather constant flux as we try to
-- understand users needs. Setup files that depend on this interface may
-- break in future releases.
data UserHooks = UserHooks
  { readDesc :: IO (Maybe GenericPackageDescription)
  -- ^ Read the description file
  , hookedPreProcessors :: [PPSuffixHandler]
  -- ^ Custom preprocessors in addition to and overriding 'knownSuffixHandlers'.
  , hookedPrograms :: [Program]
  -- ^ These programs are detected at configure time.  Arguments for them are
  -- added to the configure command.
  , preConf :: Args -> ConfigFlags -> IO HookedBuildInfo
  -- ^ Hook to run before configure command
  , confHook
      :: (GenericPackageDescription, HookedBuildInfo)
      -> ConfigFlags
      -> IO LocalBuildInfo
  -- ^ Over-ride this hook to get different behavior during configure.
  , postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after configure command
  , preBuild :: Args -> BuildFlags -> IO HookedBuildInfo
  -- ^ Hook to run before build command.  Second arg indicates verbosity level.
  , buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during build.
  , postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after build command.  Second arg indicates verbosity level.
  , preRepl :: Args -> ReplFlags -> IO HookedBuildInfo
  -- ^ Hook to run before repl command.  Second arg indicates verbosity level.
  , replHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO ()
  -- ^ Over-ride this hook to get different behavior during interpretation.
  , postRepl :: Args -> ReplFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after repl command.  Second arg indicates verbosity level.
  , preClean :: Args -> CleanFlags -> IO HookedBuildInfo
  -- ^ Hook to run before clean command.  Second arg indicates verbosity level.
  , cleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during clean.
  , postClean :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
  -- ^ Hook to run after clean command.  Second arg indicates verbosity level.
  , preCopy :: Args -> CopyFlags -> IO HookedBuildInfo
  -- ^ Hook to run before copy command
  , copyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during copy.
  , postCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after copy command
  , preInst :: Args -> InstallFlags -> IO HookedBuildInfo
  -- ^ Hook to run before install command
  , instHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during install.
  , postInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after install command.  postInst should be run
  --  on the target, not on the build machine.
  , preReg :: Args -> RegisterFlags -> IO HookedBuildInfo
  -- ^ Hook to run before register command
  , regHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during registration.
  , postReg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after register command
  , preUnreg :: Args -> RegisterFlags -> IO HookedBuildInfo
  -- ^ Hook to run before unregister command
  , unregHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during unregistration.
  , postUnreg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after unregister command
  , preHscolour :: Args -> HscolourFlags -> IO HookedBuildInfo
  -- ^ Hook to run before hscolour command.  Second arg indicates verbosity level.
  , hscolourHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HscolourFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during hscolour.
  , postHscolour :: Args -> HscolourFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after hscolour command.  Second arg indicates verbosity level.
  , preHaddock :: Args -> HaddockFlags -> IO HookedBuildInfo
  -- ^ Hook to run before haddock command.  Second arg indicates verbosity level.
  , haddockHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during haddock.
  , postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after haddock command.  Second arg indicates verbosity level.
  , preTest :: Args -> TestFlags -> IO HookedBuildInfo
  -- ^ Hook to run before test command.
  , testHook :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during test.
  , postTest :: Args -> TestFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after test command.
  , preBench :: Args -> BenchmarkFlags -> IO HookedBuildInfo
  -- ^ Hook to run before bench command.
  , benchHook :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> BenchmarkFlags -> IO ()
  -- ^ Over-ride this hook to get different behavior during bench.
  , postBench :: Args -> BenchmarkFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  -- ^ Hook to run after bench command.
  }

-- | Empty 'UserHooks' which do nothing.
emptyUserHooks :: UserHooks
emptyUserHooks =
  UserHooks
    { readDesc = return Nothing
    , hookedPreProcessors = []
    , hookedPrograms = []
    , preConf = rn'
    , confHook = (\_ _ -> return (error "No local build info generated during configure. Over-ride empty configure hook."))
    , postConf = ru
    , preBuild = rn'
    , buildHook = ru
    , postBuild = ru
    , preRepl = \_ _ -> return emptyHookedBuildInfo
    , replHook = \_ _ _ _ _ -> return ()
    , postRepl = ru
    , preClean = rn
    , cleanHook = ru
    , postClean = ru
    , preCopy = rn'
    , copyHook = ru
    , postCopy = ru
    , preInst = rn
    , instHook = ru
    , postInst = ru
    , preReg = rn'
    , regHook = ru
    , postReg = ru
    , preUnreg = rn
    , unregHook = ru
    , postUnreg = ru
    , preHscolour = rn
    , hscolourHook = ru
    , postHscolour = ru
    , preHaddock = rn'
    , haddockHook = ru
    , postHaddock = ru
    , preTest = rn'
    , testHook = \_ -> ru
    , postTest = ru
    , preBench = rn'
    , benchHook = \_ -> ru
    , postBench = ru
    }
  where
    rn args _ = noExtraFlags args >> return emptyHookedBuildInfo
    rn' _ _ = return emptyHookedBuildInfo
    ru _ _ _ _ = return ()
