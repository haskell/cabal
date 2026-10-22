import Test.Cabal.Prelude

main :: IO ()
main = cabalTest . recordMode DoNotRecord $ do
  withBuildCompiler $ \bc -> do
    cabal "v2-build" ["--with-build-compiler=" ++ bc, "all"]
    -- Ask the build compiler for its numeric version (e.g. "9.10.2")
    -- and convert it to the __GLASGOW_HASKELL__ integer format (e.g. 910).
    vr <- runM bc ["--numeric-version"] Nothing
    let bcInt = ghcNumericVersionToGHC (head (lines (resultOutput vr)))
    withPlan $ do
      r <- runPlanExe' "client" "check-compiler" []
      assertOutputContains ("setup-ghc: " ++ show bcInt) r

-- Convert a GHC numeric version string "M.N.P" to the __GLASGOW_HASKELL__
-- integer M*100 + N (e.g. "9.10.2" -> 910).
ghcNumericVersionToGHC :: String -> Int
ghcNumericVersionToGHC s = major * 100 + minor
  where
    (majorStr, rest) = span (/= '.') s
    (minorStr, _) = span (/= '.') (drop 1 rest)
    major = read majorStr
    minor = read minorStr
