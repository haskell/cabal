module PackageTests.CaretOperator.Check where

import PackageTests.PackageTester

import Distribution.Version
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.Types.Dependency
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
                     [ Dependency (mkPackageName "base")
                       (withinVersion (mkVersion [4]))
                     , Dependency (mkPackageName "pretty")
                       (majorBoundVersion (mkVersion [1,1,1,0]))
                     ]
               , hsSourceDirs = ["."]
               }
           }
        Just gotLib = library (localPkgDescr lbi)
    assertEqual "parsed library component does not match anticipated"
                            anticipatedLib gotLib
    return ()
