#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple
> import Distribution.PackageDescription (readPackageDescription)
> import Distribution.Compat.Directory (copyFile)
> import Distribution.Simple.Utils (defaultHookedPackageDesc)
> import System.Directory (removeFile)
> import System.Exit(ExitCode(..))
> import Control.Monad(when)
> import Data.Maybe(fromJust, isNothing)

> myPreConf (h:_) _ = do when (h /= "--woohoo")
>                         (error "--woohoo flag (for testing) not passed to ./setup configure.")
>                        copyFile "Setup.buildinfo.in" "Setup.buildinfo"
>                        m <- defaultHookedPackageDesc
>                        when (isNothing m) (error "can't open hooked package description!")
>                        d <- readPackageDescription (fromJust m)
>                        return $ Just d
> myPreConf [] _ = error "--woohoo flag (for testing) not passed to ./setup configure."

> main :: IO ()
> main = defaultMainWithHooks defaultUserHooks
>        {preConf=myPreConf,
>         postClean=removeFile "Setup.buildinfo" >> return ExitSuccess
>        }
