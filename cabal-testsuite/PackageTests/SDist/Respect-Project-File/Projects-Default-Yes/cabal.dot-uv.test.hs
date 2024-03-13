import Test.Cabal.Prelude

-- cabal.dot-uv.project has "packages: .". That package is uv.cabal but "cabal
-- sdist" writes sdist/p-0.1.tar.gz and sdist/q-0.1.tar.gz instead of the
-- expected sdist/uv-0.1.tar.gz.  That is wrong, "cabal sdist" should respect
-- the "--project-file" option but instead picks up the default "cabal.project"
-- that has "packages: p/ q/".
--
-- TODO: Fix this behaviour and apply the patch cabal.dot-uv.patch to update the
-- expected output to what we'd expect if "cabal sdist" respected the project.
main = cabalTest . withProjectFile "cabal.dot-uv.project" $ do
    cabal "sdist" ["all"]
