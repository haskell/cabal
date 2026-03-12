import Test.Cabal.Prelude
import Test.Cabal.OutputNormalizer
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf)

-- The project is named yops as it is like hops but with y's for forks.
-- +-- yops-0.project
--  +-- yops/yops-1.config
--   +-- yops-2.config
--    +-- yops/yops-3.config
--     +-- yops-4.config
--      +-- yops/yops-5.config
--       +-- yops-6.config
--        +-- yops/yops-7.config
--         +-- yops-8.config
--          +-- yops/yops-9.config (no further imports)
--  +-- yops/yops-3.config
--   +-- yops-4.config
--    +-- yops/yops-5.config
--     +-- yops-6.config
--      +-- yops/yops-7.config
--       +-- yops-8.config
--        +-- yops/yops-9.config (no further imports)
--  +-- yops/yops-5.config
--   +-- yops-6.config
--    +-- yops/yops-7.config
--     +-- yops-8.config
--      +-- yops/yops-9.config (no further imports)
--  +-- yops/yops-7.config
--   +-- yops-8.config
--    +-- yops/yops-9.config (no further imports)
--  +-- yops/yops-9.config (no further imports)
main = do
  -- NOTE: Options are --project-file-parser=<legacy|default|parsec|fallback|compare>
  cabalTest' "parser-legacy" . recordMode RecordMarked $ runTest (yProjectOpts "legacy") (wProjectOpts "legacy")
  cabalTest' "parser-default" . recordMode RecordMarked $ runTest (yProjectOpts "default") (wProjectOpts "default")
  cabalTest' "parser-parsec" . recordMode RecordMarked $ runTest (yProjectOpts "parsec") (wProjectOpts "parsec")
  cabalTest' "parser-fallback" . recordMode RecordMarked $ runTest (yProjectOpts "fallback") (wProjectOpts "fallback")

  cabalTest' "script-parser-legacy" . recordMode RecordMarked $ runTest (yScriptOpts "legacy") (wScriptOpts "legacy")
  cabalTest' "script-parser-default" . recordMode RecordMarked $ runTest (yScriptOpts "default") (wScriptOpts "default")
  cabalTest' "script-parser-parsec" . recordMode RecordMarked $ runTest (yScriptOpts "parsec") (wScriptOpts "parsec")
  cabalTest' "script-parser-fallback" . recordMode RecordMarked $ runTest (yScriptOpts "fallback") (wScriptOpts "fallback")
  where
    log = recordHeader . pure

    yProjectOpts parser = [ "--project-file=yops-0.project", "--project-file-parser=" ++ parser, "--dry-run" ]
    wProjectOpts parser = [ "--project-file=woops-0.project", "--project-file-parser=" ++ parser, "--dry-run" ]
    yScriptOpts parser = [ "yops-0.script.hs", "--project-file-parser=" ++ parser, "--dry-run" ]
    wScriptOpts parser = [ "woops-0.script.hs", "--project-file-parser=" ++ parser, "--dry-run" ]

    runTest yOpts wOpts = do
      log "checking that we detect when the same config is imported via many different paths"
      yopping <- cabal' "v2-build" yOpts
      assertOutputContains "Warning: 2 imports of yops-4.config" yopping
      assertOutputContains "Warning: 3 imports of yops-6.config" yopping
      assertOutputContains "Warning: 4 imports of yops-8.config" yopping
      assertOutputContains ("Warning: 2 imports of " ++ ("yops" </> "yops-3.config")) yopping
      assertOutputContains ("Warning: 3 imports of " ++ ("yops" </> "yops-5.config")) yopping
      assertOutputContains ("Warning: 4 imports of " ++ ("yops" </> "yops-7.config")) yopping
      assertOutputContains ("Warning: 5 imports of " ++ ("yops" </> "yops-9.config")) yopping

      log "checking that we detect when the same config is imported via many different paths"
      wooping <- cabal' "v2-build" wOpts
      assertOutputContains "Warning: 2 imports of woops-4.config" wooping
      assertOutputContains "Warning: 3 imports of woops-6.config" wooping
      assertOutputContains "Warning: 4 imports of woops-8.config" wooping
      assertOutputContains ("Warning: 2 imports of " ++ ("woops" </> "woops-3.config")) wooping
      assertOutputContains ("Warning: 3 imports of " ++ ("woops" </> "woops-5.config")) wooping
      assertOutputContains ("Warning: 4 imports of " ++ ("woops" </> "woops-7.config")) wooping
      assertOutputContains ("Warning: 5 imports of " ++ ("woops" </> "woops-9.config")) wooping
      assertOutputContains ("Warning: 25 imports of " ++ ("www-stackage-org" </> "lts-21.25.config")) wooping
