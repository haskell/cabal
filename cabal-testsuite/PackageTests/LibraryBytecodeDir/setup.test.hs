import Control.Monad (unless)
import System.FilePath ((</>))
import Test.Cabal.Prelude

main = setupTest $ recordMode DoNotRecord $ withPackageDb $ do
    skipUnlessGhcVersion ">= 9.15"

    env <- getTestEnv
    let bytecodeDir = testWorkDir env </> "bytecode-libs"

    setup_install
        [ "--enable-library-bytecode"
        , "--bytecodelibdir=" ++ bytecodeDir
        ]

    shouldDirectoryExist bytecodeDir

    bytecodeLibs <- liftIO $ matchGlob bytecodeDir "*.bytecodelib"
    when (null bytecodeLibs) $
        assertFailure "Expected to find at least one .bytecodelib in the configured bytecode library directory"

    -- TODO: Re-enable this when ghc-pkg supports library-bytecode-dirs
    -- ghcPkg' "field" ["library-bytecode-dir", "bytecode-library-dirs"]
    --    >>= assertOutputContains bytecodeDir
