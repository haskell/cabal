#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple

> pkg_descr = emptyPackageDescription {
>   package = PackageIdentifier "HUnit" (Version [1,0] []),
>   allModules     = ["HUnitText", "HUnit", "HUnitLang",
>                     "HUnitTestBase", "Terminal", "HUnitBase"],
>   exposedModules = ["HUnit"],
>   buildDepends = [Dependency "haskell-src" AnyVersion],
>   hsSourceDir    = "src"
>   }

> main :: IO ()
> main = do defaultMain pkg_descr
