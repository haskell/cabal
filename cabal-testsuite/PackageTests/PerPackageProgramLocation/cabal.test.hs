import Test.Cabal.Prelude

import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Types.ComponentName (ComponentName (CLibName))
import Distribution.Types.LibraryName (LibraryName (LMainLibName))
import Distribution.Types.LocalBuildInfo (withPrograms)
import Distribution.Types.PackageName (mkPackageName)
import System.Directory
  ( createDirectory
  , getPermissions
  , setOwnerExecutable
  , setPermissions
  )
import Test.Cabal.Plan (DistDirOrBinFile (..), planDistDir)

-- Test for #12535, ensuring that the following is honoured:
--
--     package p
--     hsc2hs-location: /path/to/hsc2hs
--     gcc-location: /path/to/gcc
main = do
  skipIfWindows "uses shell scripts as hsc2hs and gcc"
  cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    ghcInfo <- read . resultOutput <$> runProgramM ghcProgram ["--info"] Nothing
    let ghcInfoField field =
          fromMaybe (error ("ghc --info has no " ++ show field)) $
            lookup field ghcInfo
        ghcCcFlags =
          concatMap
            (words . ghcInfoField)
            ["C compiler flags", "C compiler link flags"]
    skipIf "GHC passes no flags to the C compiler" (null ghcCcFlags)
    hsc2hs <- programPath <$> requireProgramM hsc2hsProgram
    let bin = testCurrentDir env </> "bin"
        fakeHsc2hs = bin </> "hsc2hs"
        fakeGcc = bin </> "gcc"
    liftIO $ do
      createDirectory bin
      writeWrapper fakeHsc2hs hsc2hs
      writeWrapper fakeGcc (ghcInfoField "C compiler command")
      writeFile (testCurrentDir env </> "cabal.project") $
        unlines
          [ "packages: p"
          , "package p"
          , "  hsc2hs-location: " ++ fakeHsc2hs
          , "  gcc-location: " ++ fakeGcc
          ]
    cabal "v2-build" ["p"]
    progs <- withPlan $ do
      Just plan <- testPlan <$> getTestEnv
      let DistDir distDir = planDistDir plan (mkPackageName "p") (CLibName LMainLibName)
      withPrograms <$> liftIO (getPersistBuildConfig Nothing (makeSymbolicPath distDir))
    let configured prog =
          fromMaybe (error (programName prog ++ " is not configured")) $
            lookupProgram prog progs
    assertEqual "hsc2hs location" fakeHsc2hs (programPath (configured hsc2hsProgram))
    assertEqual "gcc location" fakeGcc (programPath (configured gccProgram))
    for_ ghcCcFlags $ \flag ->
      assertBool
        ("gcc has the C compiler flag " ++ show flag ++ " of GHC")
        (flag `elem` programDefaultArgs (configured gccProgram))

writeWrapper :: FilePath -> FilePath -> IO ()
writeWrapper wrapper prog = do
  writeFile wrapper $
    unlines
      [ "#!/bin/sh"
      , "exec " ++ show prog ++ " \"$@\""
      ]
  perms <- getPermissions wrapper
  setPermissions wrapper (setOwnerExecutable True perms)
