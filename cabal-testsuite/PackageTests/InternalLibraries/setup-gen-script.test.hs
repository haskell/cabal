import Test.Cabal.Prelude
-- Test to see if --gen-script
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "p" $ do
            setup_build []
            setup "copy" []
            setup "register" ["--gen-script"]
            _ <- if isWindows
                    then shell "cmd" ["/C", "register.bat"]
                    else shell "sh" ["register.sh"]
            return ()
        -- Make sure we can see p
        withDirectory "r" $ setup_install []
