import Test.Cabal.Prelude

-- Test that 'cabal v2-configure' generates the config file appropriately
main =
  cabalTest . withShorterPathForNewBuildStore $ do
    cwd <- fmap testCurrentDir getTestEnv
    let configFile = cwd </> "cabal.project.local"

    shouldNotExist configFile

    -- should not create config file with --dry-run or --only-download
    cabal "v2-configure" ["--dry-run"]
    cabal "v2-configure" ["--only-download"]
    shouldNotExist configFile

    -- should create the config file
    cabal "v2-configure" []
    shouldExist configFile
