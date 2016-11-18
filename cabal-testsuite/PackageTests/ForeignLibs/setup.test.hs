{-# LANGUAGE RecordWildCards #-}

import Control.Exception
import Control.Monad.IO.Class
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

import Test.Cabal.Prelude

-- Test that foreign libraries work
main = setupAndCabalTest $ do
    -- Foreign libraries don't work with GHC 7.6 and earlier
    skipUnless =<< ghcVersionIs (>= mkVersion [7,8])
    withPackageDb $ do
        setup_install []
        dist_dir <- fmap testDistDir getTestEnv
        lbi <- getLocalBuildInfoM
        let installDirs = absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

        -- Link a C program against the library
        _ <- runProgramM gccProgram
            [ "-o", "uselib"
            , "UseLib.c"
            , "-l", "myforeignlib"
            , "-L", flibdir installDirs ]

        -- Run the C program
        let ldPath = case hostPlatform lbi of
                       Platform _ OSX     -> "DYLD_LIBRARY_PATH"
                       Platform _ Windows -> "PATH"
                       Platform _ _other  -> "LD_LIBRARY_PATH"
        oldLdPath <- liftIO $ getEnv' ldPath
        withEnv [ (ldPath, Just $ flibdir installDirs ++ [searchPathSeparator] ++ oldLdPath) ] $ do
            cwd <- fmap testCurrentDir getTestEnv
            result <- runM (cwd </> "uselib") []
            assertOutputContains "5678" result
            assertOutputContains "189" result

getEnv' :: String -> IO String
getEnv' = handle handler . getEnv
  where
    handler e = if isDoesNotExistError e
                  then return ""
                  else throw e
