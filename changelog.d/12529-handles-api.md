---
synopsis: Clean up the API for logging handles
packages: [Cabal, Cabal-hooks]
prs: 12533
issues: 12529
---

A Setup script always logs to the stdout and stderr of its own process.
Accordingly, the following are removed:

  - `defaultMainArgsWithHandles`, and the `VerbosityHandles` argument of
    `defaultMainWithSetupHooksArgs`;
  - `simpleUserHooksWithHandles`.

Code running SetupHooks within another process now logs to the handles of its
caller:

  - The new `autoconfSetupHooksWithHandles` is a version of
    `autoconfSetupHooks` that logs to the given handles.
  - `findAndMonitorSourceDirsFileExts` logs to the handles of the rules
    environment.
  - Pre-build rules run by a hooks executable now use the verbosity of the
    build, instead of always using the normal verbosity.
