module Main where

import Distribution.Simple

pkg_descr = emptyPackageDescription {
  package = PackageIdentifier "test" (Version [1,0] []),
  allModules     = ["A"],
  exposedModules = ["A"]
  }

main = defaultMain pkg_descr
