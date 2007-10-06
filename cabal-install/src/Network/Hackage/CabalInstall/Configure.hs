-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Configure
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions used to generate ConfigFlags.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Configure
    ( defaultOutputGen
    , mkConfigFlags
    ) where

import Control.Monad (guard, mplus, when)

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen (..)
                                      , TempFlags (..), ResolvedPackage (..))
import Network.Hackage.CabalInstall.Config
         (getDefaultConfigDir, getLocalConfigDir, getLocalCacheDir,
          getKnownServers, selectValidConfigDir)
import Network.Hackage.CabalInstall.Fetch (pkgURL)

import qualified Distribution.Simple.Configure as Configure (configCompiler)
import Distribution.Simple.Program
import Distribution.ParseUtils (showDependency)
import Distribution.Package (showPackageId)
import Distribution.Version (VersionRange(..))
import Distribution.Verbosity
import System.FilePath ((</>))

import Text.Printf (printf)
import System.IO (openFile, IOMode (..))
import System.Directory (doesFileExist, getHomeDirectory, getAppUserDataDirectory)
import Data.Maybe (fromMaybe)

{-|
  Structure with default responses to various events.
-}
defaultOutputGen :: Verbosity -> IO OutputGen
defaultOutputGen verbosity
    = do (outch,errch) <- do guard (verbosity <= normal)
                             nullOut <- openFile ("/"</>"dev"</>"null") AppendMode
                             nullErr <- openFile ("/"</>"dev"</>"null") AppendMode
                             return (Just nullOut, Just nullErr)
                         `mplus` return (Nothing,Nothing)
         return OutputGen
                { prepareInstall = \_pkgs -> return ()
                , pkgIsPresent   = printf "'%s' is present.\n" . showPackageId
                , downloadingPkg = printf "Downloading '%s'...\n" . showPackageId
                , executingCmd   = \cmd args
                                 -> when (verbosity > silent) $ printf "Executing: '%s %s'\n" cmd (unwords args)
                , cmdFailed      = \cmd args errno
                                 -> error (printf "Command failed: '%s %s'. Errno: %d\n" cmd (unwords args) errno)
                , buildingPkg    = printf "Building '%s'\n" . showPackageId
                , stepConfigPkg  = const (printf "  Configuring...\n")
                , stepBuildPkg   = const (printf "  Building...\n")
                , stepInstallPkg = const (printf "  Installing...\n")
                , stepFinishedPkg= const (printf "  Done.\n")
                , noSetupScript  = const (error "Couldn't find a setup script in the tarball.")
                , noCabalFile    = const (error "Couldn't find a .cabal file in the tarball")
                , gettingPkgList = \serv ->
                                   when (verbosity > silent) (printf "Downloading package list from server '%s'\n" serv)
                , showPackageInfo = showPkgInfo
                , showOtherPackageInfo = showOtherPkg
                , cmdStdout      = outch
                , cmdStderr      = errch 
                , message        = \v s -> when (verbosity >= v) (putStrLn s)
                }
    where showOtherPkg mbPkg dep
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   case mbPkg of
                     Nothing  -> printf "    Not available!\n\n"
                     Just pkg -> do printf "    Using:     %s\n" (showPackageId pkg)
                                    printf "    Installed: Yes\n\n"
          showPkgInfo mbPath installed ops dep (pkg,repo,deps)
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   printf "    Using:     %s\n" (showPackageId pkg)
                   printf "    Installed: %s\n" (if installed then "Yes" else "No")
                   printf "    Depends:   %s\n" (showDeps deps)
                   printf "    Options:   %s\n" (unwords ops)
                   printf "    Location:  %s\n" (pkgURL pkg repo)
                   printf "    Local:     %s\n\n" (fromMaybe "*Not downloaded" mbPath)
          showDeps = show . map showDep
          showDep dep = show (showDependency (fulfilling dep))


-- |Compute the default prefix when doing a local install ('~/usr' on Linux).
localPrefix :: IO FilePath
localPrefix
    = do home <- getHomeDirectory
         return (home </> "usr")

{-|
  Give concrete answers to questions like:

  * where to find \'runhaskell\'.

  * where to find \'tar\'.

  * which compiler to use.

  * which config-directory to use.
-}
mkConfigFlags :: TempFlags -> IO ConfigFlags
mkConfigFlags cfg
    = do let verbosity = tempVerbose cfg
             conf = userMaybeSpecifyPath "runhaskell" (tempRunHc cfg) $
	            userMaybeSpecifyPath "tar" (tempTarPath cfg) $
	            defaultProgramConfiguration
         (runHc, conf') <- requireProgram verbosity runhaskellProgram AnyVersion conf
	 (tarProg, conf'') <- requireProgram verbosity tarProgram AnyVersion conf'
         (comp, conf''') <- Configure.configCompiler (tempHcFlavor cfg) (tempHcPath cfg) (tempHcPkg cfg) conf'' verbosity
         let userIns = tempUserIns cfg
         prefix <- if userIns
                      then fmap Just (maybe localPrefix return (tempPrefix cfg))
                      else return Nothing
         defaultConfigDir <- getDefaultConfigDir
         localConfigDir   <- getLocalConfigDir
         localCacheDir    <- getLocalCacheDir
         confDir <- selectValidConfigDir ( maybe id (:) (tempConfDir cfg)
                                           [localConfigDir, defaultConfigDir] )
         let cacheDir   = fromMaybe localCacheDir   (tempCacheDir cfg)
         when (verbosity > normal) $ do
             printf "Using config dir: %s\n" confDir
             printf "Using cache dir: %s\n" cacheDir
         outputGen <- defaultOutputGen (tempVerbose cfg)
         let config = ConfigFlags
                      { configCompiler    = comp
		      , configPrograms    = conf'''
                      , configConfDir     = confDir
                      , configCacheDir    = cacheDir
                      , configPrefix      = prefix
                      , configServers     = []
                      , configTarPath     = programPath tarProg
                      , configRunHc       = programPath runHc
                      , configOutputGen   = outputGen
                      , configVerbose     = tempVerbose cfg
--                      , configUpgradeDeps = tempUpgradeDeps cfg
                      , configUserIns     = userIns
                      }
         knownServers <- getKnownServers config
         return (config{ configServers = knownServers})

runhaskellProgram :: Program
runhaskellProgram = simpleProgram "runhaskell"
