-- | Module containing small types
module Distribution.Parsec.Types.Common (
    -- * Diagnostics
    PError (..),
    showPError,
    PWarning (..),
    PWarnType (..),
    showPWarning,
    -- * Field parser
    FieldParser,
    -- * Position
    Position (..),
    incPos,
    retPos,
    showPos,
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import System.FilePath (normalise)

import qualified Text.Parsec as Parsec

data PError = PError Position String
    deriving (Show)

-- | We do classify warnings.
--
-- Different application may decide not to show some, or have fatal behaviour on others
data PWarnType
    = PWTOther                 -- ^ Unclassified warning
    | PWTUTF
    | PWTBoolCase              -- ^ @true@ or @false@, not @True@ or @False@
    | PWTGluedOperators        -- ^ @&&!@
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
    deriving (Eq, Ord, Show, Enum, Bounded)


data PWarning = PWarning !PWarnType !Position String
    deriving (Show)

showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning _ pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg

showPError :: FilePath -> PError -> String
showPError fpath (PError pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg

-------------------------------------------------------------------------------
-- Field parser
-------------------------------------------------------------------------------

type FieldParser = Parsec.Parsec String [PWarning]

-------------------------------------------------------------------------------
-- Position
-------------------------------------------------------------------------------

data Position = Position
    {-# UNPACK #-}  !Int           -- row
    {-# UNPACK #-}  !Int           -- column
  deriving (Eq, Show)

incPos :: Int -> Position -> Position
incPos n (Position row col) = Position row (col + n)

retPos :: Position -> Position
retPos (Position row _col) = Position (row + 1) 1

showPos :: Position -> String
showPos (Position row col) = show row ++ ":" ++ show col
