import Test.Cabal.Prelude

import System.FilePath (pathSeparator, (</>))

-- https://github.com/haskell/cabal/issues/12212
-- Pins the pairing of the two halves of '--read-interface' for an internal
-- sub-library whose interface path is recovered by the 'HaddockTarget'
-- fallback in 'haddockPackagePaths'.
--
-- Under '--haddock-for-hackage' the HTML half must stay the
-- '--html-location' template that this mode always sets, even though the
-- interface half is rewritten to the 'ForHackage' naming: the template is a
-- hackage URL, not a path into the local build tree, so it is unaffected by
-- which naming the interface file happens to use. Pairing the recovered
-- interface with a rewritten local HTML directory instead would point
-- dependency links at the build tree.
main = cabalTest $ recordMode DoNotRecord $ do
  res <- cabal' "haddock" ["--haddock-for-hackage", "-v3"]

  -- The interface half: recovered by the fallback, so it carries the
  -- 'ForHackage' naming rather than the registered 'ForDevelopment' one.
  -- This is a real filesystem path (unlike the HTML half below), so it
  -- must be compared using native path separators.
  assertOutputContains
    (pathSeparator : "doc" </> "html" </> "iface-html-path-0.1.0.0-docs" </> "sub" </> "sub.haddock")
    res

  -- The HTML half: the hackage template, paired with that same interface.
  assertOutputContains
    "--read-interface=/package/iface-html-path-0.1.0.0/docs,,visible,"
    res
