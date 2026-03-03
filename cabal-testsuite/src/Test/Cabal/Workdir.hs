{-# LANGUAGE DataKinds #-}

-- | Functions for interrogating the current working directory
module Test.Cabal.Workdir where

import Distribution.Simple.Configure
import Distribution.Simple.Setup
import Distribution.Utils.Path
  ( Dist
  , FileOrDir (..)
  , Pkg
  , SymbolicPath
  , getSymbolicPath
  , makeSymbolicPath
  )

import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath

-- | Guess what the dist directory of a running executable is,
-- by using the conventional layout of built executables
-- in relation to the top of a dist directory.  Will not work
-- if the executable has been installed somewhere else.
guessDistDir :: IO (SymbolicPath Pkg (Dir Dist))
guessDistDir = do
  exe_path <- canonicalizePath =<< getExecutablePath
  let dist0 = dropFileName exe_path </> ".." </> ".."
  b <- doesFileExist (dist0 </> "setup-config")
  if b
    then do
      cwd <- getCurrentDirectory
      dist1 <- canonicalizePath dist0
      return $ makeSymbolicPath $ makeRelative (normalise cwd) dist1
    else do
      d <- getSymbolicPath <$> findDistPrefOrDefault NoFlag
      makeSymbolicPath <$> canonicalizePath d
