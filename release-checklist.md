# For major release

- Add new SPDX License list data

# For release for new GHC version:

- Update GHC flags in `normaliseGhcArgs`, and add the GHC version to
  `supportedGHCVersions` (`Distribution.Simple.Program.GHC`)
- Update `Language.Haskell.Extension` list, if there are new GHC extensions
- Update `setupMinCabalVersionConstraint` (in `Distribution.Client.ProjectPlanning`)
- Update `Cabal.Distribution.Simple.GHC` to include new GHC version
