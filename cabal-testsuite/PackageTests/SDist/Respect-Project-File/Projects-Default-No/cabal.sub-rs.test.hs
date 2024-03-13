import Test.Cabal.Prelude

-- cabal.sub-rs.project has "packages: r/ s/" but "cabal sdist" writes
-- sdist/p-0.1.tar.gz instead of the expected sdist/r-0.1.tar.gz and
-- sdist/s-0.1.tar.gz. That is wrong, "cabal sdist" should respect the
-- "--project-file" option but instead probes the parent directory and picks up
-- "../cabal.project" that has "packages: Projects-Default-No/p".
--
-- TODO: Fix this behaviour and apply the patch cabal.sub-rs.patch to update the
-- expected output to what we'd expect if "cabal sdist" respected the project.
main = cabalTest . withProjectFile "cabal.sub-rs.project" $ do
    cabal "sdist" ["all"]
