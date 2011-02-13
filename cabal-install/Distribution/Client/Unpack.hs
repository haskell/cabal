-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Unpack
-- Copyright   :  (c) Andrea Vezzosi 2008
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Unpack (

    -- * Commands
    unpack,

  ) where

import Distribution.Package
         ( PackageId, packageId )
import Distribution.Simple.Setup
         ( fromFlagOrDefault )
import Distribution.Simple.Utils
         ( notice, die )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)

import Distribution.Client.Setup
         ( GlobalFlags(..), UnpackFlags(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
        ( getAvailablePackages )

import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import Control.Monad
         ( unless, when )
import Data.Monoid
         ( mempty )
import System.FilePath
         ( (</>), addTrailingPathSeparator )


unpack :: Verbosity
       -> [Repo]
       -> GlobalFlags
       -> UnpackFlags
       -> [UserTarget] 
       -> IO ()
unpack verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

unpack verbosity repos globalFlags unpackFlags userTargets = do
  mapM_ checkTarget userTargets

  availableDb   <- getAvailablePackages verbosity repos

  pkgSpecifiers <- resolveUserTargets verbosity
                     globalFlags (packageIndex availableDb) userTargets

  pkgs <- either (die . unlines . map show) return $
            resolveWithoutDependencies
              (resolverParams availableDb pkgSpecifiers)

  unless (null prefix) $
         createDirectoryIfMissing True prefix

  flip mapM_ pkgs $ \pkg -> do
    location <- fetchPackage verbosity (packageSource pkg)
    let pkgid = packageId pkg
    case location of
      LocalTarballPackage tarballPath ->
        unpackPackage verbosity prefix pkgid tarballPath

      RemoteTarballPackage _tarballURL tarballPath ->
        unpackPackage verbosity prefix pkgid tarballPath

      RepoTarballPackage _repo _pkgid tarballPath ->
        unpackPackage verbosity prefix pkgid tarballPath

      LocalUnpackedPackage _ ->
        error "Distribution.Client.Unpack.unpack: the impossible happened."

  where
    resolverParams availableDb pkgSpecifiers =
        --TODO: add commandline constraint and preference args for unpack

        standardInstallPolicy mempty availableDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (unpackDestDir unpackFlags)

checkTarget :: UserTarget -> IO ()
checkTarget target = case target of
    UserTargetLocalDir       dir  -> die (notTarball dir)
    UserTargetLocalCabalFile file -> die (notTarball file)
    _                             -> return ()
  where
    notTarball t =
        "The 'unpack' command is for tarball packages. "
     ++ "The target '" ++ t ++ "' is not a tarball."

unpackPackage :: Verbosity -> FilePath -> PackageId -> FilePath -> IO ()
unpackPackage verbosity prefix pkgid pkgPath = do
    let pkgdirname = display pkgid
        pkgdir     = prefix </> pkgdirname
        pkgdir'    = addTrailingPathSeparator pkgdir
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ die $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die $
     "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
    notice verbosity $ "Unpacking to " ++ pkgdir'
    Tar.extractTarGzFile prefix pkgdirname pkgPath
