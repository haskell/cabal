import Test.Cabal.Prelude
import Data.Foldable (traverse_)

-- Test that program affixes options result in successful installation:
-- • Valid symlinks (--install-method=symlink)
-- • Valid copy of executables (--install-method=copy)
main = cabalTest $ do
  env <- getTestEnv
  recordMode DoNotRecord $ do
    let installdir = testPrefixDir env </> "bin"
        commonOpts = ["p", "--installdir", installdir, "--overwrite-policy=always"]
        testAllAffixes installMethod = do
          let testAffixes' = testAffixes
                (commonOpts ++ ["--install-method", installMethod])
          testAffixes' Nothing Nothing
          testAffixes' (Just "a-") Nothing
          testAffixes' Nothing (Just "-z")
          testAffixes' (Just "a-") (Just "-z")
    traverse_ testAllAffixes ["symlink", "copy"]
  where
    mkAffixOption option = maybe [] (\a -> ["--program-" ++ option, a])
    mkProgramName p s = maybe [] id p ++ "p" ++ maybe [] id s
    testAffixes commonOpts prefix suffix = do
      cabal "install" (  commonOpts
                      ++ mkAffixOption "prefix" prefix
                      ++ mkAffixOption "suffix" suffix)
      runInstalledExe (mkProgramName prefix suffix) []
