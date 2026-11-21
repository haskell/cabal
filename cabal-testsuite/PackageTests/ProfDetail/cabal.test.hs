import Test.Cabal.Prelude

-- The library of a package is built with library-profiling-detail,
-- and its executables with profiling-detail (#12528).
main = cabalTest $ recordMode DoNotRecord $ do
  skipIfNoProfiledLibraries
  let flags =
        [ "-v2"
        , "--enable-profiling"
        , "--library-profiling-detail=all-functions" -- -fprof-auto
        , "--profiling-detail=toplevel-functions" -- -fprof-auto-top
        ]
  lib <- cabal' "build" ("lib:p" : flags)
  assertOutputMatches "-fprof-auto([\" ]|$)" lib
  assertOutputDoesNotContain "-fprof-auto-top" lib
  exe <- cabal' "build" ("exe:p-exe" : flags)
  assertOutputContains "-fprof-auto-top" exe
  assertOutputDoesNotMatch "-fprof-auto([\" ]|$)" exe
