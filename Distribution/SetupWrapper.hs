{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.SetupWrapper
-- Copyright   :  (c) The University of Glasgow 2006
-- 
-- Maintainer  :  http://hackage.haskell.org/trac/hackage
-- Stability   :  alpha
-- Portability :  portable
--
-- The user interface to building and installing Cabal packages.
-- If the @Built-Type@ field is specified as something other than
-- 'Custom', and the current version of Cabal is acceptable, this performs
-- setup actions directly.  Otherwise it builds the setup script and
-- runs it with the given arguments.

module Distribution.SetupWrapper (setupWrapper) where

import qualified Distribution.Make as Make
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Configure
				( configCompiler, getInstalledPackages,
		  	  	  configDependency )
import Distribution.Setup	( reqPathArg )
import Distribution.PackageDescription	 
				( readPackageDescription,
				  PackageDescription(..),
                                  BuildType(..), cabalVersion )
import System.Console.GetOpt
import System.Directory
import Control.Exception        ( finally )
import Control.Monad		( when, unless )
import System.Directory 	( doesFileExist, getCurrentDirectory, setCurrentDirectory )

  -- read the .cabal file
  -- 	- attempt to find the version of Cabal required

  -- if the Cabal file specifies the build type (not Custom),
  --    - behave like a boilerplate Setup.hs of that type
  -- otherwise,
  --    - if we find GHC,
  --	    - build the Setup script with the right version of Cabal
  --        - invoke it with args
  --    - if we find runhaskell (TODO)
  --        - use runhaskell to invoke it
  --
  -- Later:
  --    - add support for multiple packages, by figuring out
  --      dependencies here and building/installing the sub packages
  --      in the right order.
setupWrapper :: 
       [String] -- ^ Command-line arguments.
    -> Maybe FilePath -- ^ Directory to run in. If 'Nothing', the current directory is used.
    -> IO ()
setupWrapper args mdir = inDir mdir $ do  
  let (flag_fn, non_opts, unrec_opts, errs) = getOpt' Permute opts args
  when (not (null errs)) $ die (unlines errs)
  let flags = foldr (.) id flag_fn defaultFlags
  let setup_args = unrec_opts ++ non_opts

  pkg_descr_file <- defaultPackageDesc (verbose flags)
  pkg_descr <- readPackageDescription (verbose flags) pkg_descr_file 

  comp <- configCompiler (Just GHC) (withCompiler flags) (withHcPkg flags) 0
  cabal_flag <- configCabalFlag flags (descCabalVersion pkg_descr) comp

  let
    trySetupScript f on_fail = do
       b <- doesFileExist f
       if not b then on_fail else do
       hasSetup <- do b <- doesFileExist "setup"
                      if not b then return False else do
                      t1 <- getModificationTime f
                      t2 <- getModificationTime "setup"
                      return (t1 < t2)
       unless hasSetup $
         rawSystemExit (verbose flags)
           (compilerPath comp)
           (cabal_flag ++ 
            ["--make", f, "-o", "setup", "-v"++show (verbose flags)])
       rawSystemExit (verbose flags)
         ('.':pathSeparator:"setup")
         setup_args

  case lookup (buildType pkg_descr) buildTypes of
    Just (mainAction, mainText) ->
      if withinRange cabalVersion (descCabalVersion pkg_descr)
	then mainAction setup_args -- current version is OK, so no need
				   -- to compile a special Setup.hs.
	else do writeFile ".Setup.hs" mainText
		trySetupScript ".Setup.hs" $ error "panic! shouldn't happen"
    Nothing ->
      trySetupScript "Setup.hs"  $
      trySetupScript "Setup.lhs" $
      die "no special Build-Type, but lacks Setup.hs or Setup.lhs"

buildTypes :: [(BuildType, ([String] -> IO (), String))]
buildTypes = [
  (Simple, (defaultMainArgs, "import Distribution.Simple; main=defaultMain")),
  (Configure, (defaultMainWithHooksArgs defaultUserHooks,
    "import Distribution.Simple; main=defaultMainWithHooks defaultUserHooks")),
  (Make, (Make.defaultMainArgs, "import Distribution.Make; main=defaultMain"))]

inDir :: Maybe FilePath -> IO () -> IO ()
inDir Nothing m = m
inDir (Just d) m = do
  old <- getCurrentDirectory
  setCurrentDirectory d
  m `finally` setCurrentDirectory old

data Flags
  = Flags {
    withCompiler :: Maybe FilePath,
    withHcPkg    :: Maybe FilePath,
    verbose      :: Int
  }

defaultFlags = Flags {
  withCompiler = Nothing,
  withHcPkg    = Nothing,
  verbose      = 1
 }

setWithCompiler f flags = flags{ withCompiler=f }
setWithHcPkg    f flags = flags{ withHcPkg=f }
setVerbose      v flags = flags{ verbose=v }

opts :: [OptDescr (Flags -> Flags)]
opts = [
           Option "w" ["with-setup-compiler"] (reqPathArg (setWithCompiler.Just))
               "give the path to a particular compiler to use on setup",
           Option "" ["with-setup-hc-pkg"] (reqPathArg (setWithHcPkg.Just))
               "give the path to the package tool to use on setup",
	   Option "v" ["verbose"] (OptArg (setVerbose . maybe 3 read) "n") "Control verbosity (n is 0--5, normal verbosity level is 1, -v alone is equivalent to -v3)"
  ]

noSetupScript = error "noSetupScript"

configCabalFlag :: Flags -> VersionRange -> Compiler -> IO [String]
configCabalFlag flags AnyVersion _ = return []
configCabalFlag flags range comp = do
  ipkgs <-  getInstalledPackages comp True (verbose flags)
	-- user packages are *allowed* here, no portability problem
  cabal_pkgid <- configDependency ipkgs (Dependency "Cabal" range)
  return ["-package", showPackageId cabal_pkgid]

pathSeparator :: Char
#if mingw32_HOST_OS || mingw32_TARGET_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif
