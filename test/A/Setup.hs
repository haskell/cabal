module Main where

import Distribution.Simple

pkg_descr = emptyPackageDescription {
  package = PackageIdentifier "test" (Version [1,0] []),
  allModules     = ["A", "B.A"],
  exposedModules = ["A"]
  }

main = defaultMainNoRead pkg_descr
