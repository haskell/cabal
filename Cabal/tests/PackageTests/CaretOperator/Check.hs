module PackageTests.CaretOperator.Check where

import PackageTests.PackageTester

import Distribution.Version
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.PackageDescription
import Language.Haskell.Extension (Language(..))


suite :: TestM ()
suite = do
    assertOutputDoesNotContain "Parse of field 'build-depends' failed"
        =<< cabal' "configure" []
    dist_dir <- distDir
    lbi <- liftIO $ getPersistBuildConfig dist_dir

    let anticipatedLib = emptyLibrary
           { libBuildInfo = emptyBuildInfo
               { defaultLanguage = Just Haskell2010
               , targetBuildDepends =
                     [ Dependency (PackageName{unPackageName = "base"})
                       (withinVersion (Version [4] []))
                     , Dependency (PackageName{unPackageName = "pretty"})
                       (majorBoundVersion (Version [1,1,1,0] []))
                     ]
               , hsSourceDirs = ["."]
               }
           }
        Just gotLib = library (localPkgDescr lbi)
    assertEqual "parsed library component does not match anticipated"
                            anticipatedLib gotLib
    return ()
