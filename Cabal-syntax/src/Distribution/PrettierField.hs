module Distribution.PrettierField where

import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Annotation
import Distribution.Types.Namespace

import Distribution.Fields

import Distribution.Pretty
import Distribution.Fields.Pretty
import Distribution.Parsec.Position

import qualified Text.PrettyPrint as PP

-- TODO: clarify the relationship and usages of each class

-- |
-- Similar to Pretty but outputs a Field
-- This is important to implement reordering the field.
-- The existing Prett{y,ier} classe doesn't allow us to do so.
class Prettier a => PrettierField a where
  prettierField :: FieldName -> TriviaTree -> a -> [PrettyField (Maybe Position)]

instance PrettierField a => PrettierField (Identity a) where
  prettierField name t x =
    let tChildren = unmark (SomeNamespace x) t
    in
        case prettier t (runIdentity x) of
          doc | PP.isEmpty doc -> []
          doc -> [ PrettyField Nothing name doc ]


