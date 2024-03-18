import Test.Cabal.Prelude

-- cabal.sub-pq.project has "packages: p/ q/" but "cabal sdist" only writes
-- sdist/p-0.1.tar.gz instead of the expected sdist/p-0.1.tar.gz and
-- sdist/q-0.1.tar.gz. That is wrong, "cabal sdist" should respect the
-- "--project-file" option but instead probes the parent directory and picks up
-- "../cabal.project" that has "packages: Projects-Default-No/p".
--
-- TODO: Fix this behaviour and apply the patch cabal.sub-pq.patch to update the
-- expected output to what we'd expect if "cabal sdist" respected the project.
main = cabalTest . withProjectFile "cabal.sub-pq.project" $ do
    cabal "sdist" ["all"]
