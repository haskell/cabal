#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple
> import Distribution.PackageDescription (readPackageDescription, readHookedBuildInfo)
> import Distribution.Compat.Directory (copyFile)
> import Distribution.Simple.Utils (defaultHookedPackageDesc)
> import System.Directory (removeFile)
> import System.Exit(ExitCode(..))
> import Control.Monad(when)
> import Data.Maybe(fromJust, isNothing)

 myPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo

> myPreConf (h:_) _ = do when (h /= "--woohoo")
>                         (error "--woohoo flag (for testing) not passed to ./setup configure.")
>                        copyFile "Setup.buildinfo.in" "Setup.buildinfo"
>                        m <- defaultHookedPackageDesc
>                        when (isNothing m) (error "can't open hooked package description!")
>                        readHookedBuildInfo (fromJust m)
>
> myPreConf [] _ = error "--woohoo flag (for testing) not passed to ./setup configure."

> ppTestHandler :: a -> b -> FilePath -- ^InFile
>               -> FilePath -- ^OutFile
>               -> Int      -- ^verbose
>               -> IO ExitCode
> ppTestHandler _ _ inFile outFile verbose
>     = do when (verbose > 0) $
>            putStrLn (inFile++" has been preprocessed as a test to "++outFile)
>          stuff <- readFile inFile
>          writeFile outFile ("-- this file has been preprocessed as a test\n\n" ++ stuff)
>          return ExitSuccess


Override "gc" to test the overriding mechanism.

> main :: IO ()
> main = defaultMainWithHooks defaultUserHooks
>        {preConf=myPreConf,
>         postConf=(\_ _ _-> return ExitSuccess),
>         hookedPreProcessors=  [("testSuffix", ppTestHandler), ("gc", ppTestHandler)],
>         postClean=(\_ _ _ -> removeFile "Setup.buildinfo" >> return ExitSuccess)
>        }
