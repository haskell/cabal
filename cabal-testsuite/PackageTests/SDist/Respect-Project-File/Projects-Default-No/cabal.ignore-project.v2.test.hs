import Test.Cabal.Prelude

-- This test should have written sdist/uv-0.1.tar.gz for the uv.cabal package
-- but instead it probed up the directory tree, found a default cabal.project
-- and wrote sdist/p-0.1.tar.gz. That is incorrect. It didn't ignore the
-- project.
main = cabalTest $ do
    cabal "v2-sdist" ["all", "--ignore-project"]
