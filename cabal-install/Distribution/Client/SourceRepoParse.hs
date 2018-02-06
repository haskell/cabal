module Distribution.Client.SourceRepoParse where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.FieldGrammar.FieldDescrs        (fieldDescrsToList)
import Distribution.PackageDescription.FieldGrammar (sourceRepoFieldGrammar)
import Distribution.Parsec.Class                    (explicitEitherParsec)
import Distribution.ParseUtils                      (FieldDescr (..), syntaxError)
import Distribution.Types.SourceRepo                (SourceRepo, RepoKind (..))

sourceRepoFieldDescrs :: [FieldDescr SourceRepo]
sourceRepoFieldDescrs =
    map toDescr . fieldDescrsToList $ sourceRepoFieldGrammar (RepoKindUnknown "unused")
  where
    toDescr (name, pretty, parse) = FieldDescr
        { fieldName = name
        , fieldGet  = pretty
        , fieldSet  = \lineNo str x ->
              either (syntaxError lineNo) return
              $ explicitEitherParsec (parse x) str
        }
