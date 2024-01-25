-- Test for #19350, #7339 originally by @bgamari
-- =============================================
--
-- The plan
-- ---------
-- We build a C shared library (`libhello`, contained in ./clib) and then build
-- a Haskell library (`T19350-lib`, in ./lib) which depends upon it via `foreign
-- import`. We make sure that the libhello shared object can only be found via
-- the extra-lib-dirs from the package database registration (which we do by
-- moving libhello.so from its original place).
--
-- Finally, we enter GHCi, load the Haskell library, and try to use it to call
-- into libhello.

import System.Directory
import Distribution.System
import Test.Cabal.Prelude


main = setupTest $ do

  skipIfWindows
  skipUnlessGhcVersion ">= 8.4"

  withPackageDb $ do
    cwd  <- takeDirectory . testCurrentDir <$> getTestEnv
    plat <- testPlatform <$> getTestEnv
    let libExt = case plat of
          Platform _ OSX     -> "dylib"
          Platform _ Windows -> "dll"
          Platform _ _other  -> "so"


    -- Link a C program against the library
    _ <- runProgramM ghcProgram
        [ "-fPIC", "-c", "clib/lib.c"
        , "-o", "clib/lib.o" ]
        Nothing
    _ <- runProgramM ghcProgram
        [ "-shared", "-no-hs-main", "clib/lib.o"
        , "-o", "clib/libhello" <.> libExt ]
        Nothing

    withDirectory "lib" $ do
      setup "configure" ["-v0"
                        , "--extra-lib-dirs=" ++ (cwd </> "clib")
                        , "--extra-lib-dirs=" ++ (cwd </> "clib-install")
                        , "--disable-library-vanilla"
                        , "--enable-shared"]
      setup "build" []
      setup "register" ["--inplace"]

    -- Move libhello from its original place to ensure it isn't found via RPATH
    liftIO $ do
      createDirectoryIfMissing False (cwd </> "clib-install")
      copyFile (cwd </> "clib/libhello" <.> libExt) ( cwd </> "clib-install/libhello" <.> libExt)
      removeFile (cwd </> "clib/libhello" <.> libExt)

    pkgDb <- testPackageDbDir <$> getTestEnv
    ghciScript <- liftIO $ readFile (cwd </> "T19350.script")
    _ <- runProgramM ghcProgram
         [ "--interactive"
         , "-package", "T7339"
         , "-package-db", pkgDb
         ] (Just ghciScript)

    return ()
