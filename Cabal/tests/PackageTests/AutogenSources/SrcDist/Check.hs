module PackageTests.AutogenSources.SrcDist.Check where

import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import PackageTests.PackageTester
import qualified System.FilePath as FilePath

suite :: TestM ()
suite = do

        dist_dir <- distDir

        -- Calling sdist without running configure first makes test fail with:
        -- "Exception: Run the 'configure' command first."
        -- This is becuase we are calling getPersistBuildConfig

        configureResult <- cabal' "configure" []
        sdistResult <- cabal' "sdist" []

        -- Now check that all the correct modules were parsed.
        lbi <- liftIO $ getPersistBuildConfig dist_dir
        let (Just gotLibrary) = library (localPkgDescr lbi)
        let gotExecutable = head $ executables (localPkgDescr lbi)
        let gotTestSuite  = head $ testSuites  (localPkgDescr lbi)
        let gotBenchmark  = head $ benchmarks  (localPkgDescr lbi)
        assertEqual
            ("library 'autogen-c-sources' field does not match expected")
            ["my_autogen_sources.c"]
            (autogenCSources $ libBuildInfo gotLibrary)
        assertEqual
            ("library 'autogen-js-sources' field does not match expected")
            ["my_autogen_sources.js"]
            (autogenJsSources $ libBuildInfo gotLibrary)
        assertEqual
            ("executable 'autogen-c-sources' field does not match expected")
            ["my_autogen_sources.c"]
            (autogenCSources $ buildInfo gotExecutable)
        assertEqual
            ("executable 'autogen-js-sources' field does not match expected")
            ["my_autogen_sources.js"]
            (autogenJsSources $ buildInfo gotExecutable)
        assertEqual
            ("test-suite 'autogen-c-sources' field does not match expected")
            ["my_autogen_sources.c"]
            (autogenCSources $ testBuildInfo gotTestSuite)
        assertEqual
            ("test-suite 'autogen-js-sources' field does not match expected")
            ["my_autogen_sources.js"]
            (autogenJsSources $ testBuildInfo gotTestSuite)
        assertEqual
            ("benchmark 'autogen-c-sources' field does not match expected")
            ["my_autogen_sources.c"]
            (autogenCSources $ benchmarkBuildInfo gotBenchmark)
        assertEqual
            ("benchmark 'autogen-js-sources' field does not match expected")
            ["my_autogen_sources.js"]
            (autogenJsSources $ benchmarkBuildInfo gotBenchmark)

        -- Package check messages.
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

        -- Asserts for the undesired check messages after configure.
        assertOutputDoesNotContain libAutogenCMsg    configureResult
        assertOutputDoesNotContain libAutogenJsMsg   configureResult
        assertOutputDoesNotContain exeAutogenCMsg    configureResult
        assertOutputDoesNotContain exeAutogenJsMsg   configureResult
        assertOutputDoesNotContain testAutogenCMsg   configureResult
        assertOutputDoesNotContain testAutogenJsMsg  configureResult
        assertOutputDoesNotContain benchAutogenCMsg  configureResult
        assertOutputDoesNotContain benchAutogenJsMsg configureResult

        -- Asserts for the undesired check messages after sdist.
        assertOutputDoesNotContain "Distribution quality errors:" sdistResult
        assertOutputDoesNotContain libAutogenCMsg    sdistResult
        assertOutputDoesNotContain libAutogenJsMsg   sdistResult
        assertOutputDoesNotContain exeAutogenCMsg    sdistResult
        assertOutputDoesNotContain exeAutogenJsMsg   sdistResult
        assertOutputDoesNotContain testAutogenCMsg   sdistResult
        assertOutputDoesNotContain testAutogenJsMsg  sdistResult
        assertOutputDoesNotContain benchAutogenCMsg  sdistResult
        assertOutputDoesNotContain benchAutogenJsMsg sdistResult
        assertOutputDoesNotContain "Distribution quality warnings:" sdistResult

        -- Assert sdist --list-sources output.
        -- If called before configure fails, dist directory is not created.
        let listSourcesFileGot = dist_dir ++ "/" ++ "list-sources.txt"
        cabal "sdist" ["--list-sources=" ++ listSourcesFileGot]
        let listSourcesStrExpected =
                   ("." FilePath.</> "MyLibrary.hs\n")
                ++ ("." FilePath.</> "MyLibModule.hs\n")
                ++ "my_sources.c\n"
                ++ "my_sources.js\n"
                ++ ("." FilePath.</> "Dummy.hs\n")
                ++ ("." FilePath.</> "MyExeModule.hs\n")
                ++ "my_sources.c\n"
                ++ "my_sources.js\n"
                ++ ("." FilePath.</> "Dummy.hs\n")
                ++ ("." FilePath.</> "MyTestModule.hs\n")
                ++ "my_sources.c\n"
                ++ "my_sources.js\n"
                ++ ("." FilePath.</> "Dummy.hs\n")
                ++ ("." FilePath.</> "MyBenchModule.hs\n")
                ++ "my_sources.c\n"
                ++ "my_sources.js\n"
                ++ "LICENSE\n"
                ++ ("." FilePath.</> "my.cabal\n")
        listSourcesStrGot <- liftIO $ readFile listSourcesFileGot
        assertEqual "sdist --list-sources does not match the expected files"
                listSourcesStrExpected
                listSourcesStrGot

        return ()
