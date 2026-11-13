import Distribution.System
import System.Directory
import Test.Cabal.Prelude

-- Test that Cabal does not pass the directories that @pkg-config@ considers
-- "system library directories" as @-L@ options to the linker.  Cabal sets
-- @PKG_CONFIG_ALLOW_SYSTEM_LIBS@ when invoking @pkg-config@, so @pkg-config@
-- does not strip @-L@ options for these directories itself.  They are,
-- however, redundant with the linker's default search path, and when they end
-- up ordered before a more specific @-L@ option they can cause the wrong
-- version of a library to be linked (see #11860).
main = cabalTest $ do
  skipIf "Linux-only" (not isLinux)
  skipIfNoSharedLibraries

  env <- getTestEnv
  testDir <- liftIO $ canonicalizePath (testCurrentDir env)
  let specificDir = testDir </> "specific"
      syslibDir = testDir </> "syslib"

  -- Build two versions of libclang.  The "specific" one (the desired version)
  -- lives in its own directory, while the "system" one lives in the directory
  -- that the fake @pkg-config@ reports as a system library directory.
  liftIO $ do
    createDirectoryIfMissing True specificDir
    createDirectoryIfMissing True syslibDir

  void $
    runProgramM
      gccProgram
      [ "-shared"
      , "-fPIC"
      , "-Wl,-soname,libclang-specific.so"
      , "clang-specific.c"
      , "-o"
      , specificDir </> "libclang-specific.so"
      ]
      Nothing
  void $
    runProgramM
      gccProgram
      [ "-shared"
      , "-fPIC"
      , "-Wl,-soname,libclang-syslib.so"
      , "clang-syslib.c"
      , "-o"
      , syslibDir </> "libclang-syslib.so"
      ]
      Nothing

  liftIO $ do
    createFileLink (specificDir </> "libclang-specific.so") (specificDir </> "libclang.so")
    createFileLink (syslibDir </> "libclang-syslib.so") (syslibDir </> "libclang.so")

  -- The absolute path of the "specific" library directory is only known at
  -- test runtime, so substitute it into dep-clang's .cabal file.
  void $
    shell
      "sed"
      [ "-i"
      , "-e"
      , "s|@LIB_DIR@|" ++ specificDir ++ "|g"
      , "dep-clang/dep-clang.cabal"
      ]

  res <-
    cabal'
      "v2-build"
      [ "--extra-prog-path=" ++ testDir
      , "-v3"
      , "--ghc-options=-v"
      , "all"
      ]

  assertOutputContains ("-L" ++ specificDir) res
  assertOutputDoesNotContain ("-L" ++ syslibDir) res
