#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple

> pkg_descr = emptyPackageDescription {
>   package = PackageIdentifier "Cabal" (Version [0,1] []),
>   allModules     = ["Distribution.Package",
>                     "Distribution.Version",
>                     "Distribution.Misc",
>                     "Distribution.Setup",
>                     "Distribution.InstalledPackageInfo",
>                     "Distribution.Make",
>
>                     "Distribution.Simple",
>                     "Distribution/Simple.Build",
>                     "Distribution.Simple.Install",
>                     "Distribution.Simple.SrcDist",
>                     "Distribution.Simple.Configure",
>                     "Distribution.Simple.Utils",
>                     "Distribution.Simple.Register",
>                     "Distribution.Simple.GHCPackageConfig",
>                     "Distribution.GetOpt"],
>
>   buildDepends = [Dependency "haskell-src" AnyVersion,
>                   Dependency "HUnit-1.0" AnyVersion
>                  ]
>   }

> main :: IO ()
> main = do defaultMain pkg_descr
