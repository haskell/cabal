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
  assertOutputContains "cyclical import of cyclical-1-out-back.project" cyclical1a

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
  assertOutputContains "cyclical import of cyclical-2-out-out-backback.project" cyclical2a

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

  -- +-- noncyclical-same-filename-a.project
  --  +-- noncyclical-same-filename-a.config
  --    +-- same-filename/noncyclical-same-filename-a.config (no further imports so not cyclical)
  log "checking that cyclical check doesn't false-positive on same file names in different folders; hoping within a folder and then into a subfolder"
  cyclical3b <- cabal' "v2-build" [ "--project-file=noncyclical-same-filename-a.project" ]
  assertOutputDoesNotContain "cyclical import of" cyclical3b

  -- +-- noncyclical-same-filename-b.project
  --  +-- same-filename/noncyclical-same-filename-b.config
  --    +-- noncyclical-same-filename-b.config (no further imports so not cyclical)
  log "checking that cyclical check doesn't false-positive on same file names in different folders; hoping into a subfolder and then back out again"
  cyclical3c <- cabal' "v2-build" [ "--project-file=noncyclical-same-filename-b.project" ]
  assertOutputDoesNotContain "cyclical import of" cyclical3c

  -- +-- cyclical-same-filename-out-out-self.project
  --  +-- cyclical-same-filename-out-out-self.config
  --    +-- same-filename/cyclical-same-filename-out-out-self.config
  --    +-- same-filename/cyclical-same-filename-out-out-self.config (already processed)
  --    +-- etc
  log "checking that cyclical check catches a same file name that imports itself"
  cyclical4a <- fails $ cabal' "v2-build" [ "--project-file=cyclical-same-filename-out-out-self.project" ]
  assertOutputContains "cyclical import of same-filename/cyclical-same-filename-out-out-self.config" cyclical4a

  -- +-- cyclical-same-filename-out-out-backback.project
  --  +-- cyclical-same-filename-out-out-backback.config
  --    +-- same-filename/cyclical-same-filename-out-out-backback.config
  -- +-- cyclical-same-filename-out-out-backback.project (already processed)
  -- +-- etc
  log "checking that cyclical check catches importing its importer (with the same file name)"
  cyclical4b <- fails $ cabal' "v2-build" [ "--project-file=cyclical-same-filename-out-out-backback.project" ]
  assertOutputContains "cyclical import of cyclical-same-filename-out-out-backback.project" cyclical4b

  -- +-- cyclical-same-filename-out-out-back.project
  --  +-- cyclical-same-filename-out-out-back.config
  --    +-- same-filename/cyclical-same-filename-out-out-back.config
  --  +-- cyclical-same-filename-out-out-back.config (already processed)
  --  +-- etc
  log "checking that cyclical check catches importing its importer's importer (hopping over same file names)"
  cyclical4c <- fails $ cabal' "v2-build" [ "--project-file=cyclical-same-filename-out-out-back.project" ]
  assertOutputContains "cyclical import of cyclical-same-filename-out-out-back.config" cyclical4c

  -- +-- hops-0.project
  --  +-- hops/hops-1.config
  --   +-- hops-2.config
  --    +-- hops/hops-3.config
  --     +-- hops-4.config
  --      +-- hops/hops-5.config
  --       +-- hops-6.config
  --        +-- hops/hops-7.config
  --         +-- hops-8.config
  --          +-- hops/hops-9.config (no further imports so not cyclical)
  log "checking that imports work skipping into a subfolder and then back out again and again"
  hopping <- cabal' "v2-build" [ "--project-file=hops-0.project" ]
  assertOutputContains "this build was affected by the following (project) config files:" hopping
  assertOutputContains "- hops-0.project" hopping

  assertOutputContains
    "- hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops-4.config \
    \    imported by: hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops-6.config \
    \    imported by: hops/hops-5.config \
    \    imported by: hops-4.config \
    \    imported by: hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops-8.config \
    \    imported by: hops/hops-7.config \
    \    imported by: hops-6.config \
    \    imported by: hops/hops-5.config \
    \    imported by: hops-4.config \
    \    imported by: hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops/hops-5.config \
    \    imported by: hops-4.config \
    \    imported by: hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops/hops-7.config \
    \    imported by: hops-6.config \
    \    imported by: hops/hops-5.config \
    \    imported by: hops-4.config \
    \    imported by: hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  assertOutputContains
    "- hops/hops-9.config \
    \    imported by: hops-8.config \
    \    imported by: hops/hops-7.config \
    \    imported by: hops-6.config \
    \    imported by: hops/hops-5.config \
    \    imported by: hops-4.config \
    \    imported by: hops/hops-3.config \
    \    imported by: hops-2.config \
    \    imported by: hops/hops-1.config \
    \    imported by: hops-0.project"
    hopping

  -- The project is named oops as it is like hops but has conflicting constraints.
  -- +-- oops-0.project
  --  +-- oops/oops-1.config
  --   +-- oops-2.config
  --    +-- oops/oops-3.config
  --     +-- oops-4.config
  --      +-- oops/oops-5.config
  --       +-- oops-6.config
  --        +-- oops/oops-7.config
  --         +-- oops-8.config
  --          +-- oops/oops-9.config (has conflicting constraints)
  log "checking conflicting constraints skipping into a subfolder and then back out again and again"
  oopsing <- fails $ cabal' "v2-build" [ "all", "--project-file=oops-0.project" ]
  assertOutputContains "rejecting: hashable-1.4.2.0" oopsing
  assertOutputContains "rejecting: hashable-1.4.3.0" oopsing
  assertOutputContains "(constraint from oops-0.project requires ==1.4.3.0)" oopsing

  assertOutputContains
    "      (constraint from oops/oops-9.config requires ==1.4.2.0) \
    \        imported by: oops-8.config \
    \        imported by: oops/oops-7.config \
    \        imported by: oops-6.config \
    \        imported by: oops/oops-5.config \
    \        imported by: oops-4.config \
    \        imported by: oops/oops-3.config \
    \        imported by: oops-2.config \
    \        imported by: oops/oops-1.config \
    \        imported by: oops-0.project"
    oopsing

  log "checking bad conditional"
  badIf <- fails $ cabal' "v2-build" [ "--project-file=bad-conditional.project" ]
  assertOutputContains "Cannot set compiler in a conditional clause of a cabal project file" badIf

  return ()
