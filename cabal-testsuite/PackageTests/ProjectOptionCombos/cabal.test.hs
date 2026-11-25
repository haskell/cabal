import Test.Cabal.Prelude

-- Which project is used when --project-file, --project-dir and --ignore-project
-- are combined. Each project selects one package, named after the project, so
-- the package listed by @cabal sdist --list-only all@ shows which was used:
--
--  +-------------------+-------------------+
--  | Project           | Package           |
--  +-------------------+-------------------+
--  | cabal.project     | cabal-project     |
--  | other.project     | other-project     |
--  | sub/cabal.project | sub-cabal-project |
--  | sub/other.project | sub-other-project |
--  | no project        | no-project        |
--  +-------------------+-------------------+
--
-- The users guide says that --ignore-project "will be ignored if either of the
-- --project-dir or --project-file flags are also set".
--
-- Each case has expected output of its own, which opens with the options in
-- the order given, those that are ignored marked with a star.
--
-- A case marked BUG is one where what cabal does differs from what the users
-- guide says. Its expected output records what cabal does. Whether the users
-- guide describes the best behaviour is a separate question: it may be that
-- --ignore-project with either of the others should be an error.
main = do
  uses "none" [] "cabal-project"

  -- Each option alone.
  uses "file" [file] "other-project"
  uses "dir" [dir] "sub-cabal-project"
  uses "ignore" [ignore] "no-project"

  -- --project-file is relative to --project-dir, in either order.
  uses "dir-file" [dir, file] "sub-other-project"
  uses "file-dir" [file, dir] "sub-other-project"

  -- --ignore-project is ignored when it comes after either of the others.
  uses "file-ignore" [file, ignored ignore] "other-project"
  uses "dir-ignore" [dir, ignored ignore] "sub-cabal-project"
  uses "dir-file-ignore" [dir, file, ignored ignore] "sub-other-project"
  uses "dir-ignore-file" [dir, ignored ignore, file] "sub-other-project"

  cabalTest' "missing" . recordMode RecordMarked $ do
    given [missing]
    recordHeader ["An error, as the project file does not exist"]
    r <- fails $ sdist [missing]
    assertOutputContains "missing.project" r

  -- BUG: when --ignore-project comes before either of the others it takes
  -- effect, and they are the ones ignored.
  bug "ignore-file" [ignore, ignored file] "no-project" "the users guide says --ignore-project is the one ignored"
  bug "ignore-dir" [ignore, ignored dir] "no-project" "the users guide says --ignore-project is the one ignored"
  bug "ignore-dir-file" [ignore, ignored dir, ignored file] "no-project" "the users guide says --ignore-project is the one ignored"
  bug "ignore-missing" [ignore, ignored missing] "no-project" "the users guide says --ignore-project is the one ignored, which would make the missing project file an error"

-- An option as it is given to cabal and as it is shown in expected output.
data Option = Option {argument :: String, shown :: String}

file, dir, ignore, missing :: Option
file = Option "--project-file=other.project" "--project-file"
dir = Option "--project-dir=sub" "--project-dir"
ignore = Option "--ignore-project" "--ignore-project"
missing = Option "--project-file=missing.project" "--project-file"

-- Mark an option as ignored.
ignored :: Option -> Option
ignored option = option{shown = shown option ++ "*"}

-- A named case with expected output of its own. With these options, this
-- package is the one listed.
uses :: String -> [Option] -> String -> IO ()
uses mode options package = cabalTest' mode . recordMode RecordMarked $ do
  given options
  listed options package

-- A named case that records a bug. With these options this package is the one
-- listed, though the expected output says what should happen.
bug :: String -> [Option] -> String -> String -> IO ()
bug mode options package should = cabalTest' mode . recordMode RecordMarked $ do
  given options
  recordHeader ["BUG: " ++ should]
  listed options package

-- Introduce a case in the expected output by its options, in the order given.
given :: [Option] -> TestM ()
given [] = recordHeader ["No project options"]
given options = recordHeader (map shown options ++ legend)
  where
    legend = ["(* = ignored)" | any (('*' ==) . last . shown) options]

sdist :: [Option] -> TestM Result
sdist options = cabal' "v2-sdist" (map argument options ++ ["--list-only", "all"])

-- With these options, this package is the one listed.
listed :: [Option] -> String -> TestM ()
listed options package =
  assertOutputContains (package ++ ".cabal") =<< sdist options
