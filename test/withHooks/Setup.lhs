#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple
> import Distribution.PackageDescription (readPackageDescription)
> import Distribution.Compat.Directory (copyFile)
> import System.Directory (removeFile)
> import System.Exit(ExitCode(..))
> import Control.Monad(when)

> myPreConf (h:_) _ = do when (h /= "--woohoo")
>                         (error "--woohoo flag (for testing) not passed to ./setup configure.")
>                        copyFile "Setup.buildinfo.in" "Setup.buildinfo"
>                        d <- readPackageDescription hookedPackageDesc
>                        return $ Just d
> myPreConf [] _ = error "--woohoo flag (for testing) not passed to ./setup configure."

> main :: IO ()
> main = defaultMainWithHooks defaultUserHooks
>        {preConf=myPreConf,
>         postClean=removeFile "Setup.buildinfo" >> return ExitSuccess
>        }
