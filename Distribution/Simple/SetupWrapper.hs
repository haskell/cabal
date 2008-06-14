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
				( configCompiler, getInstalledPackages,
		  	  	  configDependency )
import Distribution.PackageDescription
         ( PackageDescription(..), GenericPackageDescription(..), BuildType(..) )
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Program ( ProgramConfiguration,
                                     emptyProgramConfiguration,
                                     rawSystemProgramConf, ghcProgram )
import Distribution.Simple.GHC (ghcVerbosityOptions)
import Distribution.Text
         ( display )
import Distribution.GetOpt
import Distribution.ReadE
import System.Directory
import Distribution.Verbosity
import System.FilePath ((</>), (<.>))
import Control.Monad		( when, unless )
import Data.Maybe		( fromMaybe )
import Data.Monoid		( Monoid(mempty) )

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
       FilePath -- ^ "dist" prefix
    -> [String] -- ^ Command-line arguments.
    -> Maybe FilePath -- ^ Directory to run in. If 'Nothing', the current directory is used.
    -> IO ()
setupWrapper distPref args mdir = inDir mdir $ do  
  let (flag_fn, _, _, errs) = getOpt' Permute opts args
  when (not (null errs)) $ die (unlines errs)
  let Flags { withCompiler = hc, withHcPkg = hcPkg, withVerbosity = verbosity
        } = foldr (.) id flag_fn defaultFlags

  pkg_descr_file <- defaultPackageDesc verbosity
  ppkg_descr <- readPackageDescription verbosity pkg_descr_file 

  let
    setupDir  = distPref </> "setup"
    setupHs   = setupDir </> "setup" <.> "hs"
    setupProg = setupDir </> "setup" <.> exeExtension
    trySetupScript f on_fail = do
       b <- doesFileExist f
       if not b then on_fail else do
         hasSetup <- do b' <- doesFileExist setupProg
                        if not b' then return False else do
                          t1 <- getModificationTime f
                          t2 <- getModificationTime setupProg
                          return (t1 < t2)
         unless hasSetup $ do
	   (comp, conf) <- configCompiler (Just GHC) hc hcPkg
	                     emptyProgramConfiguration normal
	   let verRange  = descCabalVersion (packageDescription ppkg_descr)
	   cabal_flag   <- configCabalFlag verbosity verRange comp conf
           createDirectoryIfMissingVerbose verbosity True setupDir
	   rawSystemProgramConf verbosity ghcProgram conf $
                cabal_flag
             ++ ["--make", f, "-o", setupProg
	        ,"-odir", setupDir, "-hidir", setupDir]
	     ++ ghcVerbosityOptions verbosity
         rawSystemExit verbosity setupProg args

  let buildType' = fromMaybe Custom (buildType (packageDescription ppkg_descr))
  case lookup buildType' buildTypes of
    Just (mainAction, mainText) ->
      if withinRange cabalVersion (descCabalVersion (packageDescription ppkg_descr))
	then mainAction args -- current version is OK, so no need
			     -- to compile a special Setup.hs.
	else do createDirectoryIfMissingVerbose verbosity True setupDir
	        writeUTF8File setupHs mainText
		trySetupScript setupHs $ error "panic! shouldn't happen"
    Nothing ->
      trySetupScript "Setup.hs"  $
      trySetupScript "Setup.lhs" $
      die "no special Build-Type, but lacks Setup.hs or Setup.lhs"

buildTypes :: [(BuildType, ([String] -> IO (), String))]
buildTypes = [
  (Simple, (defaultMainArgs, "import Distribution.Simple; main=defaultMain")),
  (Configure, (defaultMainWithHooksArgs autoconfUserHooks,
    "import Distribution.Simple; main=defaultMainWithHooks autoconfUserHooks")),
  (Make, (Make.defaultMainArgs, "import Distribution.Make; main=defaultMain"))]

data Flags
  = Flags {
    withCompiler :: Maybe FilePath,
    withHcPkg    :: Maybe FilePath,
    withVerbosity :: Verbosity
  }

defaultFlags :: Flags
defaultFlags = Flags {
  withCompiler = Nothing,
  withHcPkg    = Nothing,
  withVerbosity = normal
 }

setWithCompiler :: Maybe FilePath -> Flags -> Flags
setWithCompiler f flags = flags{ withCompiler=f }

setWithHcPkg :: Maybe FilePath -> Flags -> Flags
setWithHcPkg f flags = flags{ withHcPkg=f }

setVerbosity :: Verbosity -> Flags -> Flags
setVerbosity v flags = flags{ withVerbosity=v }

opts :: [OptDescr (Flags -> Flags)]
opts = [
           Option "w" ["with-setup-compiler"] (ReqArg (setWithCompiler.Just) "PATH")
               "give the path to a particular compiler to use on setup",
           Option "" ["with-setup-hc-pkg"] (ReqArg (setWithHcPkg.Just) "PATH")
               "give the path to the package tool to use on setup",
	   Option "v" ["verbose"] (OptArg (maybe (setVerbosity verbose)
                                                 (setVerbosity . readEOrFail flagToVerbosity)) "n")
	       "Control verbosity (n is 0--3, default verbosity level is 1)"
  ]

configCabalFlag :: Verbosity -> VersionRange -> Compiler -> ProgramConfiguration -> IO [String]
configCabalFlag _ AnyVersion _ _ = return []
configCabalFlag verbosity range comp conf = do
  packageIndex <- fromMaybe mempty
           `fmap` getInstalledPackages verbosity comp UserPackageDB conf
	-- user packages are *allowed* here, no portability problem
  cabal_pkgid <- configDependency verbosity packageIndex
                   (Dependency (PackageName "Cabal") range)
  return ["-package", display cabal_pkgid]
