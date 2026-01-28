{-# LANGUAGE DefaultSignatures #-}
module Distribution.ExactPrettyField where

import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Annotation

import Distribution.Fields

import Distribution.Pretty
import Distribution.Fields.Pretty
import Distribution.Parsec.Position

import qualified Text.PrettyPrint as PP

-- TODO: clarify the relationship and usages of each class

-- |
-- Similar to Pretty but outputs a Field
-- This is important to implement reordering the field.
-- The existing Prett{y,ier} classes don't allow us to do so.
class (ExactPretty a) => ExactPrettyField a where
  exactPrettyField :: FieldName -> TriviaTree -> a -> [PrettyField (Maybe Position)]

instance (Namespace a, ExactPrettyField a) => ExactPrettyField (Identity a) where
  exactPrettyField name t x =
    let tChildren = unmarkTriviaTree x t
    in  mconcat
        $ flip map (exactPretty t (runIdentity x))
        $ \(DocAnn doc _) ->
          if PP.isEmpty doc then []
          else [PrettyField Nothing name doc]
