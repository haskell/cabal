-- | Utilities to work with @.cabal@ like file structure.
module Distribution.Fields
  ( -- * Types
    Field (..)
  , Name (..)
  , FieldLine (..)
  , SectionArg (..)
  , FieldName

    -- * Grammar and parsing

  --
  -- See "Distribution.Fields.Parser" for grammar.
  , readFields
  , readFields'

    -- ** ParseResult
  , ParseResult
  , runParseResult
  , parseWarning
  , parseWarnings
  , parseFailure
  , parseFatalFailure

    -- ** Warnings
  , PWarnType (..)
  , PWarning (..)
  , PWarningWithSource (..)
  , PSource (..)
  , showPWarning
  , showPWarningWithSource

    -- ** Errors
  , PError (..)
  , PErrorWithSource (..)
  , showPError
  , showPErrorWithSource

    -- * Pretty printing
  , CommentPosition (..)
  , PrettyField (..)
  , showFields
  , showFields'
  , showFieldsWithTrivia

    -- ** Transformation from Field
  , genericFromParsecFields
  , fromParsecFields
  ) where

import Distribution.Fields.Field
import Distribution.Fields.ParseResult
import Distribution.Fields.Parser
import Distribution.Fields.Pretty
import Distribution.Parsec.Error
import Distribution.Parsec.Warning
