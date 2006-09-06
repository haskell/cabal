#!/usr/bin/runhugs

> module Main where

> import Distribution.Simple
> import Distribution.PackageDescription (PackageDescription,
>                                         readPackageDescription, readHookedBuildInfo)
> import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
> import Distribution.Setup(CopyFlags(..), CopyDest(..))
> import Distribution.Compat.Directory (copyFile)
> import Distribution.Compat.FilePath(joinPaths)
> import Distribution.Simple.Utils (defaultHookedPackageDesc)
> import Distribution.Program(simpleProgram, rawSystemProgramConf)
> import System.Directory (removeFile, createDirectoryIfMissing)
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

> testing :: Args -> Bool -> a -> b -> IO ExitCode
> testing [] _ _ _ = return ExitSuccess
> testing a@(h:_) _ _ _ = do putStrLn $ "testing: " ++ (show a)
>                            if h == "--pass"
>                               then return ExitSuccess
>                               else return (ExitFailure 1)

> myCopyHook :: PackageDescription
>            -> LocalBuildInfo
>            -> Maybe UserHooks
>            -> CopyFlags -- ^install-prefix, verbose
>            -> IO ()
> myCopyHook a b c d@(CopyFlags (CopyPrefix p) _) = do
>   -- call 'ls' from our hookedPrograms hook... pointless except as a demo
>   rawSystemProgramConf 0 "ls" (withPrograms b) []
>   let copySource = case compilerFlavor $ compiler b of
>        GHC  -> foldl1 joinPaths ["dist", "build", "withHooks", "withHooks"]
>        Hugs -> foldl1 joinPaths ["dist", "build", "Main.hs"] -- some random file
>   createDirectoryIfMissing True p
>   copyFile copySource (p `joinPaths` "withHooks")

>   -- now call the default copy hook so the rest of the test case works nice ... so tricky ;)
>   (copyHook defaultUserHooks) a b c d
> myCopyHook _ _ _ _ = error "Please use --copy-prefix."

Override "gc" to test the overriding mechanism.

> main :: IO ()
> main = defaultMainWithHooks defaultUserHooks
>        {preConf=myPreConf,
>         hookedPrograms=[simpleProgram "ls"],
>         runTests=testing,
>         postConf=(\_ _ _ _ -> return ExitSuccess),
>         hookedPreProcessors=  [("testSuffix", ppTestHandler), ("gc", ppTestHandler)],
>         postClean=(\_ _ _ _ -> removeFile "Setup.buildinfo" >> return ExitSuccess),
>         copyHook=myCopyHook
>        }
