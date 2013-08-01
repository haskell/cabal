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

import Data.Maybe (listToMaybe)
import Data.List (maximumBy)
import Control.Monad (guard)
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         renameFile)
import System.FilePath ((</>), splitFileName, isAbsolute)
import Distribution.Package
         ( Package(..), packageVersion )
import Distribution.Simple.Program (haddockProgram, ProgramConfiguration
                                   , rawSystemProgram, requireProgramVersion)
import Distribution.Version (Version(Version), orLaterVersion)
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Simple.PackageIndex
         ( PackageIndex, allPackagesByName )
import Distribution.Simple.Utils
         ( comparing, intercalate, debug
         , installDirectoryContents, withTempDirectory )
import Distribution.InstalledPackageInfo as InstalledPackageInfo
         ( InstalledPackageInfo
         , InstalledPackageInfo_(haddockHTMLs, haddockInterfaces, exposed) )

regenerateHaddockIndex :: Verbosity -> PackageIndex -> ProgramConfiguration -> FilePath -> IO ()
regenerateHaddockIndex verbosity pkgs conf index = do
      (paths,warns) <- haddockPackagePaths pkgs'
      case warns of
        Nothing -> return ()
        Just m  -> debug verbosity m

      (confHaddock, _, _) <-
          requireProgramVersion verbosity haddockProgram
                                    (orLaterVersion (Version [0,6] [])) conf

      createDirectoryIfMissing True destDir

      withTempDirectory verbosity destDir "tmphaddock" $ \tempDir -> do

        let flags = [ "--gen-contents"
                    , "--gen-index"
                    , "--odir=" ++ tempDir
                    , "--title=Haskell modules on this system" ]
                 ++ [ "--read-interface=" ++ mkUrl html ++ "," ++ interface
                    | (interface, html) <- paths ]
        rawSystemProgram verbosity confHaddock flags
        renameFile (tempDir </> "index.html") (tempDir </> destFile)
        installDirectoryContents verbosity tempDir destDir

  where
    (destDir,destFile) = splitFileName index
    pkgs' = [ maximumBy (comparing packageVersion) pkgvers'
            | (_pname, pkgvers) <- allPackagesByName pkgs
            , let pkgvers' = filter exposed pkgvers
            , not (null pkgvers') ]
    -- See https://github.com/haskell/cabal/issues/1064
    mkUrl f =
      if isAbsolute f
        then "file://" ++ f
        else f

haddockPackagePaths :: [InstalledPackageInfo]
                       -> IO ([(FilePath, FilePath)], Maybe String)
haddockPackagePaths pkgs = do
  interfaces <- sequence
    [ case interfaceAndHtmlPath pkg of
        Just (interface, html) -> do
          exists <- doesFileExist interface
          if exists
            then return (pkgid, Just (interface, html))
            else return (pkgid, Nothing)
        Nothing -> return (pkgid, Nothing)
    | pkg <- pkgs, let pkgid = packageId pkg ]

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
