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
import Network.Hackage.CabalInstall.Config (getKnownServers, selectValidConfigDir)

import qualified Distribution.Simple.Configure as Configure (findProgram, configCompiler)
import Distribution.ParseUtils (showDependency)
import Distribution.Package (showPackageId)
import Distribution.Compat.FilePath (joinFileName)

import Text.Printf (printf)
import System.IO (openFile, IOMode (..))
import System.Directory (getHomeDirectory, getAppUserDataDirectory)
import Data.Maybe (fromMaybe)

{-|
  Structure with default responses to various events.
-}
defaultOutputGen :: Int -> IO OutputGen
defaultOutputGen verbose
    = do (outch,errch) <- do guard (verbose <= 1)
                             nullOut <- openFile "/dev/null" AppendMode
                             nullErr <- openFile "/dev/null" AppendMode
                             return (Just nullOut, Just nullErr)
                         `mplus` return (Nothing,Nothing)
         return OutputGen
                { prepareInstall = \_pkgs -> return ()
                , pkgIsPresent   = printf "'%s' is present.\n" . showPackageId
                , downloadingPkg = printf "Downloading '%s'\n" . showPackageId
                , executingCmd   = \cmd args
                                 -> when (verbose > 0) $ printf "Executing: '%s %s'\n" cmd (unwords args)
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
                                   when (verbose > 0) (printf "Downloading package list from server '%s'\n" serv)
                , showPackageInfo = showPkgInfo
                , showOtherPackageInfo = showOtherPkg
                , cmdStdout      = outch
                , cmdStderr      = errch 
                }
    where showOtherPkg mbPkg dep
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   case mbPkg of
                     Nothing  -> printf "    Not available!\n\n"
                     Just pkg -> do printf "    Using:     %s\n" (showPackageId pkg)
                                    printf "    Installed: Yes\n\n"
          showPkgInfo mbPath installed ops dep (pkg,location,deps)
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   printf "    Using:     %s\n" (showPackageId pkg)
                   printf "    Installed: %s\n" (if installed then "Yes" else "No")
                   printf "    Depends:   %s\n" (showDeps deps)
                   printf "    Options:   %s\n" (unwords ops)
                   printf "    Location:  %s\n" location
                   printf "    Local:     %s\n\n" (fromMaybe "*Not downloaded" mbPath)
          showDeps = show . map showDep
          showDep dep = show (showDependency (fulfilling dep))




findProgramOrDie :: String -> Maybe FilePath -> IO FilePath
findProgramOrDie name p = fmap (fromMaybe (error $ printf "No %s found." name)) (Configure.findProgram name p)

-- |Compute the default prefix when doing a local install ('~/usr' on Linux).
localPrefix :: IO FilePath
localPrefix
    = do home <- getHomeDirectory
         return (home `joinFileName` "usr")

-- |Compute the local config directory ('~/.cabal-install' on Linux).
localConfigDir :: IO FilePath
localConfigDir
    = getAppUserDataDirectory "cabal-install"

{-|
  Give concrete answers to questions like:

  * where to find \'runhaskell\'.

  * where to find \'tar\'.

  * which compiler to use.

  * which config-directory to use.
-}
mkConfigFlags :: TempFlags -> IO ConfigFlags
mkConfigFlags cfg
    = do runHc <- findProgramOrDie "runhaskell" (tempRunHc cfg)
         tarProg <- findProgramOrDie "tar" (tempTarPath cfg)
         comp <- Configure.configCompiler (tempHcFlavor cfg) (tempHcPath cfg) (tempHcPkg cfg) (tempVerbose cfg)
         localConfig <- localConfigDir
         prefix <- if tempUserIns cfg || tempUser cfg
                      then fmap Just (maybe localPrefix return (tempPrefix cfg))
                      else return Nothing
         confPath <- selectValidConfigDir ( maybe id (:) (tempConfPath cfg)
                                            ["/etc/cabal-install"
                                            ,localConfig] )
         when (tempVerbose cfg > 0) $ printf "Using config dir: %s\n" confPath
         outputGen <- defaultOutputGen (tempVerbose cfg)
         let config = ConfigFlags
                      { configCompiler    = comp
                      , configConfPath    = confPath
                      , configPrefix      = prefix
                      , configServers     = []
                      , configTarPath     = tarProg
                      , configRunHc       = runHc
                      , configOutputGen   = outputGen
                      , configVerbose     = tempVerbose cfg
--                      , configUpgradeDeps = tempUpgradeDeps cfg
                      , configUser        = tempUser cfg
                      , configUserIns     = tempUserIns cfg || tempUser cfg
                      }
         knownServers <- getKnownServers config
         return (config{ configServers = knownServers ++ tempServers cfg})

