module Main where

import Distribution.Simple

pkgconfig = emptyPackageConfig {
  package = PackageIdentifier "test" (Version [1,0] [])
  }

main = defaultMain pkgconfig
