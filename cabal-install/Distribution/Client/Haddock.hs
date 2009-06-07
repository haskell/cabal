-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Haddock
-- Copyright   :  (c) Andrea Vezzosi 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Interfacing with Haddock
--
-----------------------------------------------------------------------------
module Distribution.Client.Haddock 
    (
     regenerateHaddockIndex
    )
    where

import Data.Maybe (Maybe(..), listToMaybe)
import Data.List (maximumBy)
import Control.Monad (Monad(return), sequence, guard)
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         renameFile)
import System.FilePath (FilePath, (</>), splitFileName)
import Distribution.Package (Package(..))
import Distribution.Simple.Program (haddockProgram, ProgramConfiguration
                                   , rawSystemProgram, requireProgramVersion)
import Distribution.Version (Version(Version), orLaterVersion)
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Simple.PackageIndex(PackageIndex, allPackages,
                                        allPackagesByName, fromList)
import Distribution.Simple.Utils (comparing, installDirectoryContents
                                 , intercalate, warn, withTempDirectory)
import Distribution.InstalledPackageInfo as InstalledPackageInfo 
    (InstalledPackageInfo,InstalledPackageInfo_(haddockHTMLs, haddockInterfaces, exposed, package))

regenerateHaddockIndex :: Verbosity -> PackageIndex InstalledPackageInfo -> ProgramConfiguration -> FilePath -> IO ()
regenerateHaddockIndex verbosity pkgs conf index = do
      (paths,warns) <- haddockPackagePaths pkgs'
      case warns of
        Nothing -> return ()
        Just m  -> warn verbosity m
      
      (confHaddock, _, _) <-
          requireProgramVersion verbosity haddockProgram
                                    (orLaterVersion (Version [0,6] [])) conf

      createDirectoryIfMissing True destDir

      withTempDirectory verbosity destDir "htemp" $ \tempDir -> do

        let flags = ["--gen-contents", "--gen-index", "--odir="++tempDir] 
                    ++ map (\(i,h) -> "--read-interface=" ++ h ++ "," ++ i) paths
        rawSystemProgram verbosity confHaddock flags
        renameFile (tempDir </> "index.html") (tempDir </> destFile)
        installDirectoryContents verbosity tempDir destDir
      
  where 
    (destDir,destFile) = splitFileName index
    pkgs' = map (maximumBy $ comparing packageId) 
            . allPackagesByName 
            . fromList
            . filter exposed
            . allPackages
            $ pkgs

haddockPackagePaths :: [InstalledPackageInfo_ m]
                       -> IO ([(FilePath, FilePath)], Maybe [Char])
haddockPackagePaths pkgs = do
  interfaces <- sequence
    [ case interfaceAndHtmlPath pkg of
        Just (interface, html) -> do
          exists <- doesFileExist interface
          if exists
            then return (pkgid, Just (interface, html))
            else return (pkgid, Nothing)
        Nothing -> return (pkgid, Nothing)
    | pkg <- pkgs, let pkgid = InstalledPackageInfo.package pkg ]

  let missing = [ pkgid | (pkgid, Nothing) <- interfaces ]

      warning = "The documentation for the following packages are not "
             ++ "installed. No links will be generated to these packages: "
             ++ intercalate ", " (map display missing)

      flags = [ x | (_, Just x) <- interfaces ]

  return (flags, if null missing then Nothing else Just warning)

  where
    interfaceAndHtmlPath pkg = do
      interface <- listToMaybe (InstalledPackageInfo.haddockInterfaces pkg)
      html <- listToMaybe (InstalledPackageInfo.haddockHTMLs pkg)
      guard (not . null $ html)
      return (interface, html)
