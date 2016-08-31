module PackageTests.AutogenSources.Package.Check where

import PackageTests.PackageTester

suite :: TestM ()
suite = do

        configureResult <- shouldFail $ cabal' "configure" []
        sdistResult <- cabal' "sdist" []

        -- Package check messages.
        let libAutogenCMsg = "An 'autogen-c-sources' is not on 'c-sources'"
        let libAutogenJsMsg = "An 'autogen-js-sources' is not on 'js-sources'"
        let exeAutogenCMsg =
                   "On executable 'Exe' an 'autogen-c-sources' is not on "
                ++ "'c-sources'"
        let exeAutogenJsMsg =
                   "On executable 'Exe' an 'autogen-js-sources' is not on "
                ++ "'js-sources'"
        let testAutogenCMsg =
                   "On test suite 'Test' an 'autogen-c-sources' is not on "
                ++ "'c-sources'"
        let testAutogenJsMsg =
                   "On test suite 'Test' an 'autogen-js-sources' is not on "
                ++ "'js-sources'"
        let benchAutogenCMsg =
                   "On benchmark 'Bench' an 'autogen-c-sources' is not on "
                ++ "'c-sources'"
        let benchAutogenJsMsg =
                   "On benchmark 'Bench' an 'autogen-js-sources' is not on "
                ++ "'js-sources'"

        -- Asserts for the desired check messages after configure.
        assertOutputContains libAutogenCMsg    configureResult
        assertOutputContains libAutogenJsMsg   configureResult
        assertOutputContains exeAutogenCMsg    configureResult
        assertOutputContains exeAutogenJsMsg   configureResult
        assertOutputContains testAutogenCMsg   configureResult
        assertOutputContains testAutogenJsMsg  configureResult
        assertOutputContains benchAutogenCMsg  configureResult
        assertOutputContains benchAutogenJsMsg configureResult

        -- Asserts for the desired check messages after sdist.
        assertOutputContains "Distribution quality errors:" sdistResult
        assertOutputContains libAutogenCMsg    sdistResult
        assertOutputContains libAutogenJsMsg   sdistResult
        assertOutputContains exeAutogenCMsg    sdistResult
        assertOutputContains exeAutogenJsMsg   sdistResult
        assertOutputContains testAutogenCMsg   sdistResult
        assertOutputContains testAutogenJsMsg  sdistResult
        assertOutputContains benchAutogenCMsg  sdistResult
        assertOutputContains benchAutogenJsMsg sdistResult
        -- Asserts for the undesired check messages after sdist.
        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        return ()
