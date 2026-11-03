module Main where

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import SetupHelper (helperGhcVersion)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{confHook = myConfHook}

myConfHook
  :: (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook pkg flags = do
  lbi <- confHook simpleUserHooks pkg flags
  let define = "-DHELPER_GHC_VERSION=" ++ show helperGhcVersion
      addDefine bi = bi{cppOptions = define : cppOptions bi}
      pd = localPkgDescr lbi
      exes' = map (\e -> e{buildInfo = addDefine (buildInfo e)}) (executables pd)
  return lbi{localPkgDescr = pd{executables = exes'}}
