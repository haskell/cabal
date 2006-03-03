{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CabalSetup
-- Copyright   :  (c) The University of Glasgow 2006
-- 
-- Maintainer  :  http://hackage.haskell.org/trac/hackage
-- Stability   :  alpha
-- Portability :  portable
--
-- The user interface to building and installing Cabal packages.

module Main (main) where

import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Configure
				( configCompiler, getInstalledPackages,
		  	  	  configDependency )
import Distribution.Setup	( reqPathArg )
import Distribution.PackageDescription	 
				( readPackageDescription,
				  PackageDescription(..) )
import System.Console.GetOpt
import System.Environment
import Control.Monad		( when )
import System.Directory 	( doesFileExist )

main = do
  args <- getArgs
  
  -- read the .cabal file
  -- 	- attempt to find the version of Cabal required

  -- if there's a Setup script, 
  --    - if we find GHC,
  --	    - build it with the right version of Cabal
  --        - invoke it with args
  --    - if we find runhaskell (TODO)
  --        - use runhaskell to invoke it
  -- otherwise,
  --	- behave like a boilerplate Setup.hs
  --
  -- Later:
  --    - add support for multiple packages, by figuring out
  --      dependencies here and building/installing the sub packages
  --      in the right order.

  pkg_descr_file <- defaultPackageDesc
  pkg_descr <- readPackageDescription pkg_descr_file
  
  let (flag_fn, non_opts, unrec_opts, errs) = getOpt' Permute opts args
  when (not (null errs)) $ die (unlines errs)
  let flags = foldr (.) id flag_fn defaultFlags

  comp <- configCompiler (Just GHC) (withCompiler flags) (withHcPkg flags) 0
  cabal_flag <- configCabalFlag flags (descCabalVersion pkg_descr) comp

  let
    trySetupScript f on_fail = do
       b <- doesFileExist f
       if not b then on_fail else do
       rawSystemExit (verbose flags)
         (compilerPath comp)
         (cabal_flag ++ 
          ["--make", f, "-o", "setup", "-v"++show (verbose flags)])
       rawSystemExit (verbose flags)
         ('.':pathSeparator:"setup")
         args

  trySetupScript "Setup.hs"  $ do
  trySetupScript "Setup.lhs" $ do
  trySetupScript ".Setup.hs" $ do
  
  -- Setup.hs doesn't exist, we need to behave like defaultMain
  if descCabalVersion pkg_descr == AnyVersion
	then defaultMain
		-- doesn't matter which version we use, so no need to compile
		-- a special Setup.hs.
	else do writeFile ".Setup.hs" 
			  "import Distribution.Simple; main=defaultMain"
		trySetupScript ".Setup.hs" $ error "panic! shouldn't happen"

data Flags
  = Flags {
    withCompiler :: Maybe FilePath,
    withHcPkg    :: Maybe FilePath,
    verbose      :: Int
  }

defaultFlags = Flags {
  withCompiler = Nothing,
  withHcPkg    = Nothing,
  verbose      = 0
 }

setWithCompiler f flags = flags{ withCompiler=f }
setWithHcPkg    f flags = flags{ withHcPkg=f }
setVerbose      v flags = flags{ verbose=v }

opts :: [OptDescr (Flags -> Flags)]
opts = [
           Option "w" ["with-compiler"] (reqPathArg (setWithCompiler.Just))
               "give the path to a particular compiler",
           Option "" ["with-hc-pkg"] (reqPathArg (setWithHcPkg.Just))
               "give the path to the package tool",
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
