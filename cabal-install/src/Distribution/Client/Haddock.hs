-----------------------------------------------------------------------------

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
module Distribution.Client.Haddock
  ( regenerateHaddockIndex
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.List (maximumBy)
import Distribution.InstalledPackageInfo as InstalledPackageInfo
  ( InstalledPackageInfo (exposed)
  )
import Distribution.Package
  ( packageVersion
  )
import Distribution.Simple.Haddock (haddockPackagePaths)
import Distribution.Simple.PackageIndex
  ( InstalledPackageIndex
  , allPackagesByName
  )
import Distribution.Simple.Program
  ( ProgramDb
  , haddockProgram
  , requireProgramVersion
  , runProgram
  )
import Distribution.Simple.Utils
  ( debug
  , installDirectoryContents
  , withTempDirectory
  )
import Distribution.Version (mkVersion, orLaterVersion)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (splitFileName, (</>))

regenerateHaddockIndex
  :: Verbosity
  -> InstalledPackageIndex
  -> ProgramDb
  -> FilePath
  -> IO ()
regenerateHaddockIndex verbosity pkgs progdb index = do
  (paths, warns) <- haddockPackagePaths pkgs' Nothing
  let paths' = [(interface, html) | (interface, Just html, _, _) <- paths]
  for_ warns (debug verbosity)

  (confHaddock, _, _) <-
    requireProgramVersion
      verbosity
      haddockProgram
      (orLaterVersion (mkVersion [0, 6]))
      progdb

  createDirectoryIfMissing True destDir

  withTempDirectory verbosity destDir "tmphaddock" $ \tempDir -> do
    let flags =
          [ "--gen-contents"
          , "--gen-index"
          , "--odir=" ++ tempDir
          , "--title=Haskell modules on this system"
          ]
            ++ [ "--read-interface=" ++ html ++ "," ++ interface
               | (interface, html) <- paths'
               ]
    runProgram verbosity confHaddock flags
    renameFile (tempDir </> "index.html") (tempDir </> destFile)
    installDirectoryContents verbosity tempDir destDir
  where
    (destDir, destFile) = splitFileName index
    pkgs' :: [InstalledPackageInfo]
    pkgs' =
      [ maximumBy (comparing packageVersion) pkgvers'
      | (_pname, pkgvers) <- allPackagesByName pkgs
      , let pkgvers' = filter exposed pkgvers
      , not (null pkgvers')
      ]
