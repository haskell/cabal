module Main where

import Distribution.Simple

pkg_descr = emptyPackageDescription {
  package = PackageIdentifier "test" (Version [1,0] []),
  allModules     = ["A.hs", "B/A.hs"],
  exposedModules = ["A.hs"]
  }

main = defaultMain pkg_descr
