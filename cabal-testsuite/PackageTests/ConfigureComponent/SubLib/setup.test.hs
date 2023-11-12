import Test.Cabal.Prelude
-- NB: This currently doesn't work with cabal-install, as the depsolver
-- doesn't know to compute a dependency for sublib in exe, resulting in
-- Setup not being called with enough dependencies.  Shout if this is
-- a problem for you; the advised workaround is to use Setup directly
-- if you need per-component builds.
main = setupTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
        setup_install ["sublib"]
        setup_install ["exe"]
        runExe' "exe" [] >>= assertOutputContains "OK"
