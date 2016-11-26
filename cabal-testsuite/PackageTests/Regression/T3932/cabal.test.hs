import Test.Cabal.Prelude
main = cabalTest $
    -- This repository contains a Cabal-1.18.0.0 option, which would
    -- normally would satisfy the repository, except for new-build's
    -- extra constraint that setup Cabal must be 1.20.  If we don't
    -- have a choice like this available, the unsatisfied constraint
    -- won't be reported.
    withRepo "repo" $ do
        fails (cabal' "new-build" []) >>=
            assertOutputContains "(issue #3932) requires >=1.20"
