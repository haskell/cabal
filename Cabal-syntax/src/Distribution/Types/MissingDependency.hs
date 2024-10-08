module Distribution.Types.MissingDependency
  ( MissingDependency (..)
  ) where

import Distribution.Compat.Prelude
import Distribution.Pretty
import Distribution.Types.Dependency
  ( Dependency
  , simplifyDependency
  )
import Distribution.Types.LibraryName
  ( prettyLibraryNames
  )
import Distribution.Types.MissingDependencyReason
  ( MissingDependencyReason (..)
  )

import qualified Text.PrettyPrint as PP

-- | A missing dependency and information on why it's missing.
data MissingDependency = MissingDependency Dependency MissingDependencyReason
  deriving (Show)

instance Pretty MissingDependency where
  pretty (MissingDependency dependency reason) =
    let prettyReason =
          case reason of
            MissingLibrary libraries ->
              PP.text "missing" <+> prettyLibraryNames PP.empty libraries
            MissingPackage -> PP.text "missing"
            MissingComponent name -> PP.text "missing component" <+> pretty name
            WrongVersion versions ->
              PP.text "installed:" <+> commaSpaceSep versions
     in pretty (simplifyDependency dependency) <+> PP.parens prettyReason
