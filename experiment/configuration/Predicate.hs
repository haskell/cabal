module Predicate (

  Predicate,
  eval,
  applySystemParams,
  applyFlagAssignment,

  ) where

import qualified BasePredicate
import BasePredicate (BasePredicate, SystemParams)

import qualified PredicateExpr
import PredicateExpr (PredicateExpr)

import PartialValuation
         ( PartialValuation )

import Distribution.PackageDescription
         ( FlagName, FlagAssignment )

type Predicate = PredicateExpr BasePredicate

eval :: SystemParams
     -> FlagAssignment
     -> PartialValuation Predicate
eval sysParams flagAssignment =
  PredicateExpr.eval' (BasePredicate.eval sysParams flagAssignment)

applySystemParams :: SystemParams
                  -> Predicate -> PredicateExpr FlagName
applySystemParams sysParams =
  PredicateExpr.simplify' (BasePredicate.applySystemParams sysParams)

applyFlagAssignment :: FlagAssignment
                    -> Predicate -> Predicate
applyFlagAssignment flagAssignment =
  PredicateExpr.simplify (BasePredicate.applyFlagAssignment flagAssignment)
