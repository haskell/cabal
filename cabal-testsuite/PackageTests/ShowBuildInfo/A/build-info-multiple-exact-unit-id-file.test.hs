import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ withSourceCopy $ do
    cwd <- fmap testCurrentDir getTestEnv
    let fp = cwd </> "unit.json"
    _ <- cabal' "show-build-info" ["--buildinfo-json-output=" ++ fp, "--unit-ids-json=A-0.1.0.0-inplace A-0.1.0.0-inplace-A", "-v0"]
    buildInfo <- decodeBuildInfoFile fp
    assertEqual "Cabal Version" cabalVersionLibrary (cabalVersion buildInfo)
    assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
    assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
    assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)
    assertEqual "Components, exactly two" 2 (length $ components buildInfo)
    let [libBuildInfo, exeBuildInfo] = components buildInfo
    assertExe exeBuildInfo
    assertLib libBuildInfo
    where
      assertExe :: ComponentInfo -> TestM ()
      assertExe component = do
        assertEqual "Component type" "exe" (componentType component)
        assertEqual "Component name" "exe:A" (componentName component)
        assertEqual "Component unit-id" "A-0.1.0.0-inplace-A" (componentUnitId component)
        assertBool "Component compiler args are non-empty" (not . null $ componentCompilerArgs component)
        assertEqual "Component modules" [] (componentModules component)
        assertEqual "Component source files" ["Main.hs"] (componentSrcFiles component)
        assertEqual "Component source directories" ["src"] (componentHsSrcDirs component)

      assertLib :: ComponentInfo -> TestM ()
      assertLib component = do
        assertEqual "Component type" "lib" (componentType component)
        assertEqual "Component name" "lib" (componentName component)
        assertEqual "Component unit-id" "A-0.1.0.0-inplace" (componentUnitId component)
        assertBool "Component compiler args are non-empty" (not . null $ componentCompilerArgs component)
        assertEqual "Component modules" ["A"] (componentModules component)
        assertEqual "Component source files" [] (componentSrcFiles component)
        assertEqual "Component source directories" ["src"] (componentHsSrcDirs component)
