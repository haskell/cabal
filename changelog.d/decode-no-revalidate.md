---
synopsis: Don't re-validate serialised graphs and install plans on decode
packages: [Cabal-syntax, cabal-install]
prs: 12169
significance:
---

- The `Binary` and `Read` instances of `Distribution.Compat.Graph.Graph` no
  longer re-check key uniqueness when decoding, and consequently no longer
  require `Show (Key a)`. Decoding now simply inverts encoding, trusting that
  serialised data was produced from a valid graph. This is a backwards-compatible
  widening of the instance contexts.
- `Binary (GenericInstallPlan …)` likewise reconstructs the plan directly
  instead of routing through `mkInstallPlan`, so it no longer re-runs the
  plan-validity check (which `error`ed rather than failing the decode) on cached
  data.
