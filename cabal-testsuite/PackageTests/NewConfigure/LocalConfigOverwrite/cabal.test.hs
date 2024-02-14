import Test.Cabal.Prelude

main = cabalTest $ do
        cabal' "v2-configure" [] >>=
            assertOutputContains "backing it up to 'cabal.project.local~'"

        -- With --project-file
        cabal' "v2-configure" ["--project-file", "foo.project"] >>=
            assertOutputContains
                "'foo.project.local' already exists, backing it up to 'foo.project.local~'"
