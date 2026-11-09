module SetupHooks ( setupHooks ) where

import Distribution.Simple.Flag ( flagToMaybe )
import Distribution.Simple.Setup ( CommonSetupFlags (..), ConfigFlags (..) )
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path ( getSymbolicPath, interpretSymbolicPath, makeRelativePathEx )

import Control.Monad ( unless, when )
import System.Directory ( canonicalizePath, doesFileExist, getCurrentDirectory )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks { preConfPackageHook = Just checkWorkingDir }
    }

checkWorkingDir :: PreConfPackageInputs -> IO PreConfPackageOutputs
checkWorkingDir inputs@PreConfPackageInputs{configFlags = flags} = do
  let mbWorkDir = flagToMaybe $ setupWorkingDir $ configCommonFlags flags
  -- The files of the package are found through the working directory...
  found <- doesFileExist $ interpretSymbolicPath mbWorkDir (makeRelativePathEx "marker.txt")
  unless found $ fail "marker.txt not found through the working directory"
  -- ...and the hooks do not run in the package directory.
  cwd <- canonicalizePath =<< getCurrentDirectory
  pkgDir <- canonicalizePath $ maybe "." getSymbolicPath mbWorkDir
  when (cwd == pkgDir) $ fail "SetupHooks run in the package directory"
  putStrLn "pkg SetupHooks: working directory OK"
  return $ noPreConfPackageOutputs inputs
