import Test.Cabal.Prelude

main = cabalTest $
    withSourceCopy $ do
        cabal' "v2-configure" [] >>=
            assertOutputContains "backing it up to 'cabal.project.local~'"
        cabal' "v2-configure" [] >>=
            assertOutputContains "backing it up to 'cabal.project.local~0'"
        cabal' "v2-configure" [] >>=
            assertOutputContains "backing it up to 'cabal.project.local~1'"

        -- With --project-file
        cabal' "v2-configure" ["--project-file", "foo.project"] >>=
            assertOutputContains
                "'foo.project.local' already exists, backing it up to 'foo.project.local~'"
        cabal' "v2-configure" ["--project-file", "foo.project"] >>=
            assertOutputContains
                "'foo.project.local' already exists, backing it up to 'foo.project.local~0'"
        cabal' "v2-configure" ["--project-file", "foo.project"] >>=
            assertOutputContains
                "'foo.project.local' already exists, backing it up to 'foo.project.local~1'"
