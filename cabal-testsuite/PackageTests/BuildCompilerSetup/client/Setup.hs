{-# LANGUAGE CPP #-}
module Main where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Types.BuildInfo

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{confHook = myConfHook}

myConfHook
  :: (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
myConfHook pkg flags = do
  lbi <- confHook simpleUserHooks pkg flags
  -- __GLASGOW_HASKELL__ is the version of GHC compiling this Setup.hs,
  -- i.e. the build compiler in a cross-compile scenario.
  let define = "-DSETUP_GHC_VERSION=" ++ show (__GLASGOW_HASKELL__ :: Int)
      addDefine bi = bi{cppOptions = define : cppOptions bi}
      pd = localPkgDescr lbi
      exes' = map (\e -> e{buildInfo = addDefine (buildInfo e)}) (executables pd)
  return lbi{localPkgDescr = pd{executables = exes'}}
