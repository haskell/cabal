import Test.Cabal.Prelude

-- Test that 'cabal v2-configure' generates the config file appropriately
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do
    cwd <- fmap testCurrentDir getTestEnv
    let configFile = cwd </> "cabal.project.local"

    shouldNotExist configFile

    -- should not create config file with --dry-run or --only-download
    cabalG ["--store-dir=" ++ storeDir] "v2-configure" ["--dry-run"]
    cabalG ["--store-dir=" ++ storeDir] "v2-configure" ["--only-download"]
    shouldNotExist configFile

    -- should create the config file
    cabalG ["--store-dir=" ++ storeDir] "v2-configure" []
    shouldExist configFile
