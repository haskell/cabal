#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple
> import Distribution.PackageDescription (readPackageDescription)
> import Distribution.Compat.Directory (copyFile)
> import System.Directory (removeFile)
> import System.Exit(ExitCode(..))

> myPreConf _ _ = do copyFile "Setup.buildinfo.in" "Setup.buildinfo"
>                    d <- readPackageDescription hookedPackageDesc
>                    return $ Just d

> main :: IO ()
> main = defaultMainWithHooks defaultUserHooks
>        {preConf=myPreConf,
>         postClean=removeFile "Setup.buildinfo" >> return ExitSuccess
>        }
