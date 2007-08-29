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

module Distribution.Simple.SetupWrapper (setupWrapper) where

import qualified Distribution.Make as Make
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Configure
				( configCompiler, getInstalledPackagesGHC,
		  	  	  configDependency )
import Distribution.Simple.Setup	( reqPathArg )
import Distribution.PackageDescription	 
				( readPackageDescription,
                                  packageDescription,
				  PackageDescription(..),
                                  BuildType(..), cabalVersion )
import Distribution.Simple.Program ( ProgramConfiguration,
                                     emptyProgramConfiguration,
                                     rawSystemProgramConf, ghcProgram )
import Distribution.GetOpt
import System.Directory
import Distribution.Compat.Exception ( finally )
import Distribution.Verbosity
import System.FilePath (pathSeparator)
import Control.Monad		( when, unless )

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

  pkg_descr_file <- defaultPackageDesc (verbosity flags)
  ppkg_descr <- readPackageDescription (verbosity flags) pkg_descr_file 

  (_, conf) <- configCompiler (Just GHC) (withCompiler flags) (withHcPkg flags)
                 emptyProgramConfiguration normal

  cabal_flag <- let verRange = descCabalVersion (packageDescription ppkg_descr)
                 in configCabalFlag flags verRange conf

  let
    trySetupScript f on_fail = do
       b <- doesFileExist f
       if not b then on_fail else do
         hasSetup <- do b' <- doesFileExist "setup"
                        if not b' then return False else do
                          t1 <- getModificationTime f
                          t2 <- getModificationTime "setup"
                          return (t1 < t2)
         unless hasSetup $
           rawSystemProgramConf (verbosity flags) ghcProgram conf
             (cabal_flag ++ 
              ["--make", f, "-o", "setup", "-v"++showForGHC (verbosity flags)])
         rawSystemExit (verbosity flags)
           ('.':pathSeparator:"setup")
           setup_args

  case lookup (buildType (packageDescription ppkg_descr)) buildTypes of
    Just (mainAction, mainText) ->
      if withinRange cabalVersion (descCabalVersion (packageDescription ppkg_descr))
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
    verbosity      :: Verbosity
  }

defaultFlags :: Flags
defaultFlags = Flags {
  withCompiler = Nothing,
  withHcPkg    = Nothing,
  verbosity      = normal
 }

setWithCompiler :: Maybe FilePath -> Flags -> Flags
setWithCompiler f flags = flags{ withCompiler=f }

setWithHcPkg :: Maybe FilePath -> Flags -> Flags
setWithHcPkg f flags = flags{ withHcPkg=f }

setVerbosity :: Verbosity -> Flags -> Flags
setVerbosity v flags = flags{ verbosity=v }

opts :: [OptDescr (Flags -> Flags)]
opts = [
           Option "w" ["with-setup-compiler"] (reqPathArg (setWithCompiler.Just))
               "give the path to a particular compiler to use on setup",
           Option "" ["with-setup-hc-pkg"] (reqPathArg (setWithHcPkg.Just))
               "give the path to the package tool to use on setup",
	   Option "v" ["verbosity"] (OptArg (setVerbosity . flagToVerbosity) "n") "Control verbosity (n is 0--5, normal verbosity level is 1, -v alone is equivalent to -v3)"
  ]

configCabalFlag :: Flags -> VersionRange -> ProgramConfiguration -> IO [String]
configCabalFlag _flags AnyVersion _ = return []
configCabalFlag flags range conf = do
  ipkgs <-  getInstalledPackagesGHC (verbosity flags) conf True
	-- user packages are *allowed* here, no portability problem
  cabal_pkgid <- configDependency (verbosity flags) ipkgs (Dependency "Cabal" range)
  return ["-package", showPackageId cabal_pkgid]
