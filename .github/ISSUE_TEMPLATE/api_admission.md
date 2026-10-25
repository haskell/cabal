---
name: cabal-install API admission
about: Request to admit a use case into the stable public API tier (`Distribution.Client.API.*`)
title: '[API admission] <tool name>: <use case in one line>'
labels: ["type: cabal-install-api"]
assignees: ''

---

**The tool and its use case**
One paragraph: what the tool does and why it needs the API, with a link to the repository or Hackage page.

**Internal definitions imported today**
The exact modules and definitions your tool imports from `cabal-install-internal` (or from the legacy `cabal-install` library), e.g. `Distribution.Client.Config (loadConfig, SavedConfig)`.

**Proposed public additions**
New functions/types for the existing `API.*` modules — or an argument for a new module. For each addition: the proposed name, a signature written in API types only (see the design principles of the cabal-install API proposal), and why it is general enough to freeze as a public contract rather than stay internal.

**Demonstration**
Ideally, a link to a branch of your tool already building against the proposed API.

**Additional context**
Add any other relevant context here.
