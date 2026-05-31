import Distribution.System
  ( OS (Windows)
  , buildOS
  )
import System.Directory
  ( createDirectoryIfMissing
  )
import System.Exit
  ( ExitCode (ExitSuccess)
  )
import System.FilePath
  ( (</>)
  )
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Test.Cabal.Prelude

main :: IO ()
main = cabalTest $ withShorterPathForNewBuildStore $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let upstreamPath = testCurrentDir env </> "upstream"

  createDir "upstream"
  (pkgACommit, pkgBCommit) <- withDirectory "upstream" $ do
    git "init" []
    git "config" ["user.email", "testsuite@example.invalid"]
    git "config" ["user.name", "Cabal Testsuite"]

    createDir "pkg-a/src"
    writeSourceFile "pkg-a/pkg-a.cabal" $
      unlines
        [ "cabal-version: 3.0"
        , "name: pkg-a"
        , "version: 0.1.0.0"
        , "build-type: Simple"
        , ""
        , "library"
        , "  exposed-modules: PkgA"
        , "  hs-source-dirs: src"
        , "  build-depends: base >=4.14 && <5"
        , "  default-language: Haskell2010"
        ]
    writeSourceFile "pkg-a/src/PkgA.hs" $
      unlines
        [ "module PkgA where"
        , ""
        , "pkgA :: String"
        , "pkgA = \"a\""
        ]
    git "add" ["pkg-a"]
    git "commit" ["-m", "Add pkg-a"]
    pkgACommit <- commitHash

    createDir "pkg-b/src"
    writeSourceFile "pkg-b/pkg-b.cabal" $
      unlines
        [ "cabal-version: 3.0"
        , "name: pkg-b"
        , "version: 0.1.0.0"
        , "build-type: Simple"
        , ""
        , "library"
        , "  exposed-modules: PkgB"
        , "  hs-source-dirs: src"
        , "  build-depends: base >=4.14 && <5"
        , "  default-language: Haskell2010"
        ]
    writeSourceFile "pkg-b/src/PkgB.hs" $
      unlines
        [ "module PkgB where"
        , ""
        , "pkgB :: String"
        , "pkgB = \"b\""
        ]
    git "add" ["pkg-b"]
    git "commit" ["-m", "Add pkg-b"]
    pkgBCommit <- commitHash

    pure (pkgACommit, pkgBCommit)

  createDir "dummy-app/app"
  writeSourceFile "dummy-app/dummy-app.cabal" $
    unlines
      [ "cabal-version: 3.0"
      , "name: dummy-app"
      , "version: 0.1.0.0"
      , "build-type: Simple"
      , ""
      , "executable dummy-app"
      , "  main-is: Main.hs"
      , "  hs-source-dirs: app"
      , "  build-depends: base >=4.14 && <5, pkg-a, pkg-b"
      , "  default-language: Haskell2010"
      ]
  writeSourceFile "dummy-app/app/Main.hs" $
    unlines
      [ "module Main where"
      , ""
      , "import PkgA (pkgA)"
      , "import PkgB (pkgB)"
      , ""
      , "main :: IO ()"
      , "main = putStrLn (pkgA ++ pkgB)"
      ]

  writeSourceFile "cabal.project" $
    unlines
      [ "packages: dummy-app"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ fileUriFromPath upstreamPath
      , "  tag: " ++ pkgACommit
      , "  subdir: pkg-a"
      , ""
      , "source-repository-package"
      , "  type: git"
      , "  location: " ++ fileUriFromPath upstreamPath
      , "  tag: " ++ pkgBCommit
      , "  subdir: pkg-b"
      ]

  result <- cabal' "v2-build" ["all", "-v"]
  assertExitCode ExitSuccess result
  assertOutputDoesNotContain "reference repository" result
  where
    commitHash = do
      result <- git' "rev-parse" ["HEAD"]
      pure (head (lines (resultOutput result)))

    createDir relativePath = do
      cwd <- fmap testCurrentDir getTestEnv
      liftIO $ createDirectoryIfMissing True (cwd </> relativePath)

    fileUriFromPath path =
      case buildOS of
        Windows -> "file:///" ++ toPosixPath path
        _ -> "file://" ++ toPosixPath path

    toPosixPath = map toPosixSeparator

    toPosixSeparator pathSeparator
      | pathSeparator == Windows.pathSeparator = Posix.pathSeparator
      | otherwise = pathSeparator
