{-# LANGUAGE DeriveGeneric #-}
module Distribution.Parsec.Warning (
    PWarning (..),
    PWarnType (..),
    showPWarning,
    ) where

import Distribution.Compat.Prelude
import Distribution.Parsec.Position
import Prelude ()
import System.FilePath              (normalise)

-- | Type of parser warning. We do classify warnings.
--
-- Different application may decide not to show some, or have fatal behaviour on others
data PWarnType
    = PWTOther                 -- ^ Unclassified warning
    | PWTUTF                   -- ^ Invalid UTF encoding
    | PWTBoolCase              -- ^ @true@ or @false@, not @True@ or @False@
    | PWTVersionTag            -- ^ there are version with tags
    | PWTNewSyntax             -- ^ New syntax used, but no @cabal-version: >= 1.2@ specified
    | PWTOldSyntax             -- ^ Old syntax used, and @cabal-version >= 1.2@ specified
    | PWTDeprecatedField
    | PWTInvalidSubsection
    | PWTUnknownField
    | PWTUnknownSection
    | PWTTrailingFields
    | PWTExtraMainIs           -- ^ extra main-is field
    | PWTExtraTestModule       -- ^ extra test-module field
    | PWTExtraBenchmarkModule  -- ^ extra benchmark-module field
    | PWTLexNBSP
    | PWTLexBOM
    | PWTLexTab
    | PWTQuirkyCabalFile       -- ^ legacy cabal file that we know how to patch
    | PWTDoubleDash            -- ^ Double dash token, most likely it's a mistake - it's not a comment
    | PWTMultipleSingularField -- ^ e.g. name or version should be specified only once.
    | PWTBuildTypeDefault      -- ^ Workaround for derive-package having build-type: Default. See <https://github.com/haskell/cabal/issues/5020>.

    | PWTVersionOperator       -- ^ Version operators used (without cabal-version: 1.8)
    | PWTVersionWildcard       -- ^ Version wildcard used (without cabal-version: 1.6)

    | PWTSpecVersion           -- ^ Warnings about cabal-version format.

    | PWTEmptyFilePath         -- ^ Empty filepath, i.e. literally ""

    | PWTExperimental          -- ^ Experimental feature
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary PWarnType
instance NFData PWarnType where rnf = genericRnf

-- | Parser warning.
data PWarning = PWarning !PWarnType !Position String
    deriving (Show, Generic)

instance Binary PWarning
instance NFData PWarning where rnf = genericRnf

showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning _ pos msg) =
    normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg
