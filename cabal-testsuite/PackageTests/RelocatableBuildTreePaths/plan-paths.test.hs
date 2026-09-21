-- #12145: the project-local paths in @plan.json@ are recorded relative to the
-- build tree root, so that the tree stays valid when it is relocated.
--
-- That is only sound for paths which move along /with/ the tree. A package
-- which lives outside the project does not, so its path has to stay absolute:
-- relativising it would silently repoint it at some other directory once the
-- tree is moved.
--
-- The project here has one package in a subdirectory (@pkgs/inside@, which
-- must be relativised) and one outside the project root (@../external@, which
-- must not be).
import Test.Cabal.Prelude
import Test.Cabal.Plan (planPkgSrcPaths, readPlanRaw)
import Distribution.Package (mkPackageName)

import Control.Monad.Trans.Reader (withReaderT)

main =
  cabalTest $
    recordMode DoNotRecord $
      -- The project root is 'proj'; 'external' sits next to it, outside.
      withReaderT (\env -> env{testRelativeCurrentDir = "proj"}) $ do
        -- A *relative* --builddir puts the dist tree inside the project, which
        -- is the only arrangement in which the recorded paths are relocatable
        -- at all, and hence the only one in which they are made relative.
        cabal "build" ["all", "--builddir=dist"]

        env <- getTestEnv
        plan <-
          liftIO . readPlanRaw $
            testCurrentDir env </> "dist" </> "cache" </> "plan.json"
        let srcs = planPkgSrcPaths plan

        let pkgSrcOf name =
              case [path | (name', path) <- srcs, name' == mkPackageName name] of
                [path] -> return path
                paths ->
                  assertFailure $
                    "expected exactly one pkg-src for " ++ name ++ ", got " ++ show paths

        inside <- pkgSrcOf "inside"
        external <- pkgSrcOf "external"

        assertBool
          ( "a package inside the project should be recorded relative to the "
              ++ "build tree root, but was: "
              ++ inside
          )
          (isRelative inside)
        assertBool
          ( "a package outside the project must stay absolute, since it does "
              ++ "not move with the build tree, but was: "
              ++ external
          )
          (isAbsolute external)
