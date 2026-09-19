{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | Utilities to work with @.cabal@ like file structure.
module Distribution.Fields
  ( -- * Types
    Field
  , FieldConcrete
  , Name (..)
  , FieldLine (..)
  , SectionArg (..)
  , FieldName

    -- * Patterns
#if __GLASGOW_HASKELL__ >= 914
  , data Field
  , data Section
  , data FieldConcrete
  , data SectionConcrete
#else
  , pattern Field
  , pattern Section
  , pattern FieldConcrete
  , pattern SectionConcrete
#endif

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
