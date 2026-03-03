import Test.Cabal.Prelude
import Data.Foldable (traverse_)
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
main = do
  -- Most of these are magic on Windows, so don't bother testing there.
  skipIfWindows "uninteresting"
  cabalTest $
    -- Test the forbidden characters except NUL. Reference:
    -- https://www.gnu.org/software/autoconf/manual/autoconf.html#File-System-Conventions
    --
    -- Note: we bundle the configure script so no need to autoreconf
    -- while building

    traverse_ check
      [ "foo bar"
      , "foo\tbar"
      , "foo\nbar"
      , "foo\"bar"
      , "foo#bar"
      , "foo$bar"
      , "foo&bar"
      , "foo'bar"
      , "foo(bar"
      , "foo)bar"
      , "foo*bar"
      , "foo;bar"
      , "foo<bar"
      , "foo=bar"
      , "foo>bar"
      , "foo?bar"
      , "foo[bar"
      , "foo\\bar"
      , "foo`bar"
      , "foo|bar"
      ]
  where
    setup dir = do
      env <- getTestEnv
      let cwd = testCurrentDir env
      liftIO $ createDirectory (testCurrentDir env </> dir)
      liftIO $
        copyFiles
          (mkVerbosity defaultVerbosityHandles silent)
          (testCurrentDir env </> dir)
          [ (cwd, "configure")
          , (cwd, "Setup.hs")
          , (cwd, "test.cabal")
          ]
    -- 'cabal' from the prelude requires the command to succeed; we
    -- don't mind if it fails, so long as we get the warning. This is
    -- an inlined+specialised version of 'cabal' for v1-configure.
    check dir =
      defaultRecordMode RecordMarked $ do
        setup dir
        recordHeader ["cabal", "v1-configure"]
        env <- getTestEnv
        let args =
              [ "v1-configure"
              , "-vverbose +markoutput +nowrap"
              , "--builddir"
              , testDistDir env
              ]
        configured_prog <- requireProgramM cabalProgram
        r <- liftIO $ run
                      (Just $ testCurrentDir env </> dir)
                      (testEnvironment env)
                      (programPath configured_prog)
                      args Nothing
        recordLog r
