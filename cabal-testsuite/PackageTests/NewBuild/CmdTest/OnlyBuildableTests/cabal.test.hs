import Test.Cabal.Prelude

main = cabalTest $ do
    -- "--enable-tests" and "--disable-tests" are not the only two
    -- possibilities, they are only the two extremes: build _all_ the tests,
    -- and build _none_ of the tests.
    --
    -- The default, "--enable-tests-when-possible", is to only build the tests
    -- for which a build plan can be found, and to silently ignore the tests
    -- for which a build plan cannot be found.
    
    -- This project has two package; one with a buildable test, and one with an
    -- unbuildable test. If we request both tests to be built and run, then
    -- "cabal test" command should fail because of the unbuildable test.
    fails $ cabal "v2-test" ["--enable-tests", "all"]
    
    -- If we request zero tests to be built, then "cabal test" should fail
    -- because there are no tests to run.
    fails $ cabal "v2-test" ["--disable-tests", "all"]

    -- If we request the buildable tests to be built and run, then "cabal test"
    -- should successfully build one test.
    cabal "v2-test" ["--enable-tests-when-possible", "all"]
