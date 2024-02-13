import Test.Cabal.Prelude
import Test.Cabal.OutputNormalizer
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf)

main = cabalTest . recordMode RecordMarked $ do
  let log = recordHeader . pure

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
  log "checking that we detect when the same config is imported via many different paths"
  yopping <- cabal' "v2-build" [ "--project-file=yops-0.project", "--project-file-parser=legacy", "--dry-run" ]
  assertOutputContains "Warning: 2 imports of yops-4.config" yopping
  assertOutputContains "Warning: 2 imports of yops-6.config" yopping
  assertOutputContains "Warning: 2 imports of yops-8.config" yopping
  assertOutputContains ("Warning: 2 imports of " ++ ("yops" </> "yops-3.config")) yopping
  assertOutputContains ("Warning: 2 imports of " ++ ("yops" </> "yops-5.config")) yopping
  assertOutputContains ("Warning: 2 imports of " ++ ("yops" </> "yops-7.config")) yopping
  assertOutputContains ("Warning: 2 imports of " ++ ("yops" </> "yops-9.config")) yopping

  log "checking that we detect when the same config is imported via many different paths"
  wooping <- cabal' "v2-build" [ "--project-file=woops-0.project", "--project-file-parser=legacy", "--dry-run" ]
  assertOutputContains "Warning: 2 imports of woops-4.config" wooping
  assertOutputContains "Warning: 2 imports of woops-6.config" wooping
  assertOutputContains "Warning: 2 imports of woops-8.config" wooping
  assertOutputContains ("Warning: 2 imports of " ++ ("woops" </> "woops-3.config")) wooping
  assertOutputContains ("Warning: 2 imports of " ++ ("woops" </> "woops-5.config")) wooping
  assertOutputContains ("Warning: 2 imports of " ++ ("woops" </> "woops-7.config")) wooping
  assertOutputContains ("Warning: 2 imports of " ++ ("woops" </> "woops-9.config")) wooping
  assertOutputContains ("Warning: 10 imports of " ++ ("www-stackage-org" </> "lts-21.25.config")) wooping

  return ()
