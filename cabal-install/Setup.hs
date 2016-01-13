import Distribution.PackageDescription ( PackageDescription )
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , postBuild
                           , postCopy
                           , postInst
                           )
import Distribution.Simple.InstallDirs ( mandir
                                       , CopyDest (NoCopyDest)
                                       )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..)
                                          , absoluteInstallDirs
                                          )
import Distribution.Simple.Utils ( copyFiles
                                 , notice )
import Distribution.Simple.Setup ( buildVerbosity
                                 , copyDest
                                 , copyVerbosity
                                 , fromFlag
                                 , installVerbosity
                                 )
import Distribution.Verbosity ( Verbosity )

import System.IO ( openFile
                 , IOMode (WriteMode)
                 )
import System.Process ( runProcess )
import System.FilePath ( (</>) )


main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = \ _ flags _ lbi ->
      buildManpage lbi (fromFlag $ buildVerbosity flags)
  , postCopy = \ _ flags pkg lbi ->
      installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags)
  , postInst = \ _ flags pkg lbi ->
      installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
  }

buildManpage :: LocalBuildInfo -> Verbosity -> IO ()
buildManpage lbi verbosity = do
  let cabal = buildDir lbi </> "cabal/cabal"
      manpage = buildDir lbi </> "cabal/cabal.1"
  manpageHandle <- openFile manpage WriteMode
  notice verbosity ("Generating manual page " ++ manpage ++ " ...")
  _ <- runProcess cabal ["manpage"] Nothing Nothing Nothing (Just manpageHandle) Nothing
  return ()

installManpage :: PackageDescription -> LocalBuildInfo -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy = do
  let destDir = mandir (absoluteInstallDirs pkg lbi copy) </> "man1"
  copyFiles verbosity destDir [(buildDir lbi </> "cabal", "cabal.1")]
