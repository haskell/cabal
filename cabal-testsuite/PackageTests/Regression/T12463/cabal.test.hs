import Test.Cabal.Prelude

main = cabalTest $ do
  writeSourceFile "shared-source.cabal" (packageDescription "-Wall")
  cabal "build" ["all"]
  cabal "build" ["all"]
  delay
  -- Changing an inactive branch leaves the finalized PackageDescription
  -- unchanged, but must still invalidate the configuration cache.
  writeSourceFile "shared-source.cabal" (packageDescription "-Wextra")
  cabal "build" ["all"]
  cabal "build" ["all"]

packageDescription :: String -> String
packageDescription warning =
  unlines $
    [ "cabal-version: 3.0"
    , "name: shared-source"
    , "version: 0"
    , "build-type: Simple"
    , "flag extra-warnings"
    , "  default: False"
    , "  manual: True"
    , "common options"
    , "  default-language: Haskell2010"
    , "  build-depends: base < 5"
    , "  if flag(extra-warnings)"
    , "    ghc-options: " ++ warning
    ]
      ++ concat
        [ [ "library " ++ name
          , "  import: options"
          , "  visibility: public"
          , "  exposed-modules: M"
          ]
        | name <- ["one", "two", "three"]
        ]
