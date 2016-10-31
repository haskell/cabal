{-# LANGUAGE RecordWildCards #-}
module PackageTests.ForeignLibs.Check where

import Control.Exception
import System.Environment
import System.FilePath
import System.IO.Error

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Types
import Distribution.System
import Distribution.Verbosity
import Distribution.Version

import PackageTests.PackageTester

-- Foreign libraries don't work with GHC 7.2 and earlier
suite :: TestM ()
suite = whenGhcVersion (>= mkVersion [7,4]) . withPackageDb $ do
    cabal_install []
    dist_dir <- distDir
    pkg_dir <- packageDir
    lbi <- liftIO $ getPersistBuildConfig dist_dir
    let installDirs = absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

    -- Link a C program against the library
    (gcc, _) <- liftIO $ requireProgram normal gccProgram (withPrograms lbi)
    _ <- run (Just pkg_dir) (programPath gcc) [
        "-o", "uselib"
      , "UseLib.c"
      , "-l", "myforeignlib"
      , "-L", flibdir installDirs
      ]

    -- Run the C program
    let ldPath = case hostPlatform lbi of
                   Platform _ OSX     -> "DYLD_LIBRARY_PATH"
                   Platform _ Windows -> "PATH"
                   Platform _ _other  -> "LD_LIBRARY_PATH"
    oldLdPath <- liftIO $ getEnv' ldPath
    withEnv [ (ldPath, Just $ flibdir installDirs ++ [searchPathSeparator] ++ oldLdPath) ] $ do
        result <- run (Just pkg_dir)
                      (pkg_dir </> "uselib")
                      []
        assertOutputContains "5678" result
        assertOutputContains "189" result

getEnv' :: String -> IO String
getEnv' = handle handler . getEnv
  where
    handler e = if isDoesNotExistError e
                  then return ""
                  else throw e
