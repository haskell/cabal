
import Test.Cabal.Prelude

-- --extra-include-dirs have to be absolute paths so this cannot
-- be specified in the cabal.project nor cabal.config.
includeDirs :: [String]
includeDirs =
  [ "deps/my01/include"
  , "deps/my02/include"
  , "deps/my03/include"
  , "deps/my04/include"
  , "deps/my05/include"
  , "deps/my06/include"
  , "deps/my07/include"
  , "deps/my08/include"
  , "deps/my09/include"
  , "deps/my10/include"
  , "deps/my-dep01/include"
  , "deps/my-dep02/include"
  , "deps/my-dep03/include"
  , "deps/my-dep04/include"
  , "deps/my-dep05/include"
  , "deps/my-dep06/include"
  , "deps/my-dep07/include"
  , "deps/my-dep08/include"
  , "deps/my-dep09/include"
  , "deps/my-dep10/include"
  , "deps/my-dep-dep01/include"
  , "deps/my-dep-dep02/include"
  , "deps/my-dep-dep03/include"
  , "deps/my-dep-dep04/include"
  , "deps/my-dep-dep05/include"
  , "deps/my-dep-dep06/include"
  , "deps/my-dep-dep07/include"
  , "deps/my-dep-dep08/include"
  , "deps/my-dep-dep09/include"
  , "deps/my-dep-dep10/include"
  ]

-- --extra-lib-dirs have to be absolute paths so this cannot
-- be specified in the cabal.project nor cabal.config.
libDirs :: [String]
libDirs =
  [ "deps/my01/lib"
  , "deps/my02/lib"
  , "deps/my03/lib"
  , "deps/my04/lib"
  , "deps/my05/lib"
  , "deps/my06/lib"
  , "deps/my07/lib"
  , "deps/my08/lib"
  , "deps/my09/lib"
  , "deps/my10/lib"
  , "deps/my-dep01/lib"
  , "deps/my-dep02/lib"
  , "deps/my-dep03/lib"
  , "deps/my-dep04/lib"
  , "deps/my-dep05/lib"
  , "deps/my-dep06/lib"
  , "deps/my-dep07/lib"
  , "deps/my-dep08/lib"
  , "deps/my-dep09/lib"
  , "deps/my-dep10/lib"
  , "deps/my-dep-dep01/lib"
  , "deps/my-dep-dep02/lib"
  , "deps/my-dep-dep03/lib"
  , "deps/my-dep-dep04/lib"
  , "deps/my-dep-dep05/lib"
  , "deps/my-dep-dep06/lib"
  , "deps/my-dep-dep07/lib"
  , "deps/my-dep-dep08/lib"
  , "deps/my-dep-dep09/lib"
  , "deps/my-dep-dep10/lib"
  ]


main :: IO ()
main = cabalTest $ do
  cwd <- testSourceDir <$> getTestEnv

  -- The intention is to pass these flags via cabal.project or cabal.config,
  -- but these directories must be absolute paths so we cannot commit
  -- necessary cabal.project in the repository and have to resort to
  -- the command-line arguments instead.
  let includeFlags =
        concatMap (\x -> ["--extra-include-dirs", cwd </> x]) includeDirs
      libFlags =
        concatMap (\x -> ["--extra-lib-dirs", cwd </> x]) libDirs

  let customCC = cwd </> "custom-cc"

  -- Parallel flag means output of this test is nondeterministic
  _<- recordMode DoNotRecord $
    cabal "v2-build" $
      ["-j", "my-toplevel:exe:my-executable"]
        -- Don’t validate absence of duplicates on Windows because
        -- it’s too inconvenient to do in bat files. Let’s just
        -- assume that deduplication will happen on Windows but
        -- still try to run the test to see whether command-line
        -- argument length limit is not hit.
        ++ ["--with-gcc=" ++ customCC | not isWindows]
        ++ includeFlags
        ++ libFlags
  r <- withPlan $ runPlanExe' "my-toplevel" "my-executable" []
  assertOutputContains
    ("Result = " ++ show (42 + 55 + 10 * (55 + 10 * 55) + 3 * 55))
    r
