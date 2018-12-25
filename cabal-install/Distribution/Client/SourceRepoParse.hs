module Distribution.Client.SourceRepoParse where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Deprecated.ParseUtils           (FieldDescr (..), syntaxError)
import Distribution.FieldGrammar.FieldDescrs        (fieldDescrsToList)
import Distribution.PackageDescription.FieldGrammar (sourceRepoFieldGrammar)
import Distribution.Parsec                          (explicitEitherParsec)
import Distribution.Simple.Utils                    (fromUTF8BS)
import Distribution.Types.SourceRepo                (RepoKind (..), SourceRepo)

sourceRepoFieldDescrs :: [FieldDescr SourceRepo]
sourceRepoFieldDescrs =
    map toDescr . fieldDescrsToList $ sourceRepoFieldGrammar (RepoKindUnknown "unused")
  where
    toDescr (name, pretty, parse) = FieldDescr
        { fieldName = fromUTF8BS name
        , fieldGet  = pretty
        , fieldSet  = \lineNo str x ->
              either (syntaxError lineNo) return
              $ explicitEitherParsec (parse x) str
        }
