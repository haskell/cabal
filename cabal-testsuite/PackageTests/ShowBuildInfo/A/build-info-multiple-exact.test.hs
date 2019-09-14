import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfos <- runShowBuildInfo ["exe:A", "lib:A", "-v0"]
    assertEqual "Build Infos, exactly two " 2  (length buildInfos)
    let [libBuildInfo, exeBuildInfo] = buildInfos
    assertExe exeBuildInfo
    assertLib libBuildInfo
    where
      assertExe :: BuildInfo -> TestM ()
      assertExe buildInfo = do
        assertEqual "Cabal Version" cabalVersionLibrary (cabalVersion buildInfo)
        assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
        assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
        assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)
        assertEqual "Components, exactly one" 1 (length $ components buildInfo)
        let [component] = components buildInfo
        assertEqual "Component type" "exe" (componentType component)
        assertEqual "Component name" "exe:A" (componentName component)
        assertEqual "Component unit-id" "A-0.1.0.0-inplace-A" (componentUnitId component)
        assertBool "Component compiler args are non-empty" (not . null $ componentCompilerArgs component)
        assertEqual "Component modules" [] (componentModules component)
        assertEqual "Component source files" ["Main.hs"] (componentSrcFiles component)
        assertEqual "Component source directories" ["src"] (componentSrcDirs component)

      assertLib :: BuildInfo -> TestM ()
      assertLib buildInfo = do
        assertEqual "Cabal Version" cabalVersionLibrary (cabalVersion buildInfo)
        assertEqual "Compiler flavour" "ghc" (flavour $ compiler buildInfo)
        assertBool "Compiler id" (and $ zipWith (==) "ghc" (compilerId $ compiler buildInfo))
        assertBool "Compiler path non-empty" (not . null . path $ compiler buildInfo)
        assertEqual "Components, exactly one" 1 (length $ components buildInfo)
        let [component] = components buildInfo
        assertEqual "Component type" "lib" (componentType component)
        assertEqual "Component name" "lib" (componentName component)
        assertEqual "Component unit-id" "A-0.1.0.0-inplace" (componentUnitId component)
        assertBool "Component compiler args are non-empty" (not . null $ componentCompilerArgs component)
        assertEqual "Component modules" ["A"] (componentModules component)
        assertEqual "Component source files" [] (componentSrcFiles component)
        assertEqual "Component source directories" ["src"] (componentSrcDirs component)
