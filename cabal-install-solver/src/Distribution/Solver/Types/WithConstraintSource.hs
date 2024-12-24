{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Solver.Types.WithConstraintSource
  ( WithConstraintSource (..)
  , showWithConstraintSource
  , withUnknownConstraint
  ) where

import Distribution.Solver.Compat.Prelude

import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..), showConstraintSource)
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint

-- | A package bundled with a `ConstraintSource`.
data WithConstraintSource pkg =
  WithConstraintSource
    { constraintInner :: pkg
      -- ^ The package.
    , constraintSource :: ConstraintSource
      -- ^ The constraint source for the package.
    }
  deriving (Show, Functor, Eq, Ord, Traversable, Foldable, Generic)

instance Binary pkg => Binary (WithConstraintSource pkg)
instance Structured pkg => Structured (WithConstraintSource pkg)

withUnknownConstraint :: pkg -> WithConstraintSource pkg
withUnknownConstraint constraintInner =
  WithConstraintSource
    { constraintInner
    , constraintSource = ConstraintSourceUnknown
    }

showWithConstraintSource :: (pkg -> String) -> WithConstraintSource pkg -> String
showWithConstraintSource
  showPackage
  (WithConstraintSource { constraintInner, constraintSource }) =
    showPackage constraintInner ++ " (" ++ showConstraintSource constraintSource ++ ")"

instance Pretty pkg => Pretty (WithConstraintSource pkg) where
  pretty (WithConstraintSource { constraintInner, constraintSource = ConstraintSourceUnknown })
    = pretty constraintInner
  pretty (WithConstraintSource { constraintInner, constraintSource })
    = pretty constraintInner
      <+> parens (text "from" <+> pretty constraintSource)
