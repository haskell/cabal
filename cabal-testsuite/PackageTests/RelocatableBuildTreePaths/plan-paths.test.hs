-- #12145: the project-local paths in @plan.json@ are recorded relative to the
-- build tree root, so that the tree stays valid when it is relocated.
--
-- This doesn't apply in the case of a package outside the project root.
import Test.Cabal.Prelude
import Test.Cabal.Plan (planPkgSrcPaths, readPlanRaw)
import Distribution.Package (mkPackageName)

import Control.Monad.Trans.Reader (withReaderT)

main =
  cabalTest $
    recordMode DoNotRecord $
      -- The project root is 'proj'; 'external' sits next to it, outside.
      withReaderT (\env -> env{testRelativeCurrentDir = "proj"}) $ do
        env <- getTestEnv
        let external = takeDirectory (testCurrentDir env) </> "external"
        writeSourceFile "cabal.project" $
          unlines
            [ "packages: pkgs/inside"
            , "          " ++ external
            ]

        cabal "build" ["all", "--builddir=dist"]

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
