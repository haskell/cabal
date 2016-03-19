module Distribution.Client.Dependency.Modular.ConflictSet (
    ConflictSet
  , showCS
  ) where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S

import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Var

type ConflictSet qpn = Set (Var qpn)

showCS :: ConflictSet QPN -> String
showCS = intercalate ", " . map showVar . S.toList
