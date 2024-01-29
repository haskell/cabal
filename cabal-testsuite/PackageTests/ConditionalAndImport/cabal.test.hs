import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
  let log = recordHeader . pure

  cabal "v2-run" [ "some-exe" ]

  -- +-- cyclical-0-self.project (imports cyclical-0-self.project)
  -- +-- cyclical-0-self.project (already processed)
  -- +-- etc
  log "checking cyclical loopback of a project importing itself"
  cyclical0 <- fails $ cabal' "v2-build" [ "--project-file=cyclical-0-self.project" ]
  assertOutputContains "cyclical import of cyclical-0-self.project" cyclical0

  -- +-- cyclical-1-out-back.project
  --  +-- cyclical-1-out-back.config (imports cyclical-1-out-back.project)
  -- +-- cyclical-1-out-back.project (already processed)
  --  +-- etc
  log "checking cyclical with hops; out and back"
  cyclical1a <- fails $ cabal' "v2-build" [ "--project-file=cyclical-1-out-back.project" ]
  assertOutputContains "cyclical import of cyclical-1-out-back.config" cyclical1a

  -- +-- cyclical-1-out-self.project
  --  +-- cyclical-1-out-self.config (imports cyclical-1-out-self.config)
  --  +-- cyclical-1-out-self.config (already processed)
  --  +-- etc
  log "checking cyclical with hops; out to a config that imports itself"
  cyclical1b <- fails $ cabal' "v2-build" [ "--project-file=cyclical-1-out-self.project" ]
  assertOutputContains "cyclical import of cyclical-1-out-self.config" cyclical1b

  -- +-- cyclical-2-out-out-backback.project
  --  +-- cyclical-2-out-out-backback-a.config
  --   +-- cyclical-2-out-out-backback-b.config (imports cyclical-2-out-out-backback.project)
  -- +-- cyclical-2-out-out-backback.project (already processed)
  --  +-- etc
  log "checking cyclical with hops; out, out, twice back"
  cyclical2a <- fails $ cabal' "v2-build" [ "--project-file=cyclical-2-out-out-backback.project" ]
  assertOutputContains "cyclical import of cyclical-2-out-out-backback-a.config" cyclical2a

  -- +-- cyclical-2-out-out-back.project
  --  +-- cyclical-2-out-out-back-a.config
  --   +-- cyclical-2-out-out-back-b.config (imports cyclical-2-out-out-back-a.config)
  --  +-- cyclical-2-out-out-back-a.config (already processed)
  --   +-- etc
  log "checking cyclical with hops; out, out, once back"
  cyclical2b <- fails $ cabal' "v2-build" [ "--project-file=cyclical-2-out-out-back.project" ]
  assertOutputContains "cyclical import of cyclical-2-out-out-back-a.config" cyclical2b

  -- +-- cyclical-2-out-out-self.project
  --  +-- cyclical-2-out-out-self-a.config
  --   +-- cyclical-2-out-out-self-b.config (imports cyclical-2-out-out-self-b.config)
  --   +-- cyclical-2-out-out-self-b.config (already processed)
  --   +-- etc
  log "checking cyclical with hops; out, out to a config that imports itself"
  cyclical2c <- fails $ cabal' "v2-build" [ "--project-file=cyclical-2-out-out-self.project" ]
  assertOutputContains "cyclical import of cyclical-2-out-out-self-b.config" cyclical2c

  log "checking bad conditional"
  badIf <- fails $ cabal' "v2-build" [ "--project-file=bad-conditional.project" ]
  assertOutputContains "Cannot set compiler in a conditional clause of a cabal project file" badIf

  return ()
