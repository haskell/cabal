import Test.Cabal.Prelude

import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, executable, getPermissions, setPermissions)
import System.FilePath ((</>))

main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let pwd = testCurrentDir env
      extraPath = pwd </> "program-cache-empty-path"
      toolName = "versioned-haddock" ++ if isWindows then ".bat" else ""
      toolPath = pwd </> toolName
      buildArgs = ["all", "--with-haddock=" ++ toolPath]
  writeSourceFile "program-cache.cabal" packageDescription
  writeSourceFile "M.hs" "module M where\n"
  writeSourceFile "cabal.project" "packages: .\n"
  liftIO $ createDirectoryIfMissing True extraPath

  cabal "build" ["all"]
  cached <- cabal' "build" ["all"]
  assertOutputContains "Up to date" cached
  -- No executable is added or moved. The captured PATH alone must still
  -- invalidate configuration, preserving the REPL fix from #2015.
  addToPath extraPath $ do
    changed <- cabal' "build" ["all"]
    assertConfigured changed
    cached <- cabal' "build" ["all"]
    assertOutputContains "Up to date" cached
    repl <-
      cabalWithStdin
        "repl"
        ["lib:one"]
        "import System.Environment\ngetEnv \"PATH\" >>= putStrLn\n:q\n"
    assertOutputContains "program-cache-empty-path" repl

  -- Configure a tool whose version can change while its location and
  -- arguments stay fixed. It is never executed for building these libraries.
  writeSourceFile toolName $
    if isWindows
      then "@type \"%~dp0haddock-version\"\r\n"
      else "#!/bin/sh\ncat \"$(dirname \"$0\")/haddock-version\"\n"
  permissions <- liftIO $ getPermissions toolPath
  liftIO $ setPermissions toolPath permissions{executable = True}
  writeSourceFile "haddock-version" "Haddock version 2.30.0\n"
  cabal "build" buildArgs
  cached <- cabal' "build" buildArgs
  assertOutputContains "Up to date" cached
  -- A redundant constraint forces elaboration without changing the package
  -- configuration. Check that it causes no reconfiguration on its own.
  writeSourceFile "cabal.project" "packages: .\nconstraints: base >= 0\n"
  unchanged <- cabal' "build" buildArgs
  assertOutputContains "Up to date" unchanged
  delay
  writeSourceFile "haddock-version" "Haddock version 2.31.0\n"
  writeSourceFile "cabal.project" "packages: .\nconstraints: base < 100\n"
  changed <- cabal' "build" buildArgs
  assertConfigured changed
  cached <- cabal' "build" buildArgs
  assertOutputContains "Up to date" cached

assertConfigured result = do
  assertOutputContains "Configuring library 'one'" result
  assertOutputContains "Configuring library 'two'" result

packageDescription =
  unlines
    [ "cabal-version: 3.0"
    , "name: program-cache"
    , "version: 0"
    , "build-type: Simple"
    , "common options"
    , "  default-language: Haskell2010"
    , "  build-depends: base < 5"
    , "library one"
    , "  import: options"
    , "  visibility: public"
    , "  exposed-modules: M"
    , "library two"
    , "  import: options"
    , "  visibility: public"
    , "  exposed-modules: M"
    ]
