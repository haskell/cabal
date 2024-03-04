import Test.Cabal.Prelude

-- We are testing if the build-tools program is found in path before programs e.g. in extra-prog-path or the system path
-- For that, we need
--  * A repo with a build tool that is up to date
--  * An older version of the build tool in the extra-prog-path
--  * A project that requires the more up-to-date version of the build-tool

main = cabalTest $ withRepo "repo" $ do
  dir <- testWorkDir <$> getTestEnv
  cabal "v2-install" ["mybuilder-0.1.0.0", "--installdir=" ++ dir ++ "/install", "--overwrite-policy=always"]
  cabal "v2-build" ["cabal-bug-build-tool", "--extra-prog-path=" ++ dir ++ "/install"]

