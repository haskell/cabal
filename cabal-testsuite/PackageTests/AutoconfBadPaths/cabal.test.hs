import Test.Cabal.Prelude
import Data.Foldable (traverse_)
main = cabalTest $ do
  -- Test the forbidden characters except NUL. Reference:
  -- https://www.gnu.org/software/autoconf/manual/autoconf.html#File-System-Conventions
  -- Most of these are magic on Windows, so don't bother testing there.
  --
  -- Note: we bundle the configure script so no need to autoreconf
  -- while building
  skipIf =<< isWindows
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
    -- 'cabal' from the prelude requires the command to succeed; we
    -- don't mind if it fails, so long as we get the warning. This is
    -- an inlined+specialised version of 'cabal' for v1-configure.
    check dir = withSourceCopyDir dir $
      defaultRecordMode RecordMarked $ do
        recordHeader ["cabal", "v1-configure"]
        env <- getTestEnv
        let args =
              [ "v1-configure"
              , "-vverbose +markoutput +nowrap"
              , "--builddir"
              , testDistDir env
              ]
        configured_prog <- requireProgramM cabalProgram
        r <- liftIO $ run (testVerbosity env)
                      (Just (testCurrentDir env))
                      (testEnvironment env)
                      (programPath configured_prog)
                      args Nothing
        recordLog r
