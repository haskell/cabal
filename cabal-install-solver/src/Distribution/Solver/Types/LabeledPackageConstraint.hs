{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    , weedLabeledPackageConstraints
    ) where

import Distribution.Compat.Prelude
import Prelude ()
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Types.VersionRange
import qualified Data.Map.Strict as Map
import Data.List (groupBy)

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc

unscopePackageConstraint :: PackageConstraint -> ConstraintScope
unscopePackageConstraint (PackageConstraint scope _) = scope

-- | Weed out potential package version conflicts for each package by picking
-- version equality constraints with the lowest import depth and discarding the
-- rest.  Constraints such as installed, source, flags and stanzas are untouched
-- by weeding.
--
-- Flags that may have applied to weeded versions of a package may be orphaned.
weedLabeledPackageConstraints :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedLabeledPackageConstraints =
    (\(xs, ys) ->
        let toKeyValues :: [(String, LabeledPackageConstraint)] -> (String, [LabeledPackageConstraint])
            toKeyValues kvs = (case kvs of (s, _) : _ -> (s,); [] -> ("",)) $ snd <$> kvs

            xsGrouped :: [(String, [LabeledPackageConstraint])]
            xsGrouped = toKeyValues <$> groupBy (\x y -> EQ == (comparing fst) x y)
                [ (show . unscopePackageConstraint $ unlabelPackageConstraint x, x)
                | x <- xs
                ]

            xsWeeded = (weedLabeled . sortLabeled) <$> Map.fromList xsGrouped
        in concat (Map.elems xsWeeded) ++ ys
    )
    . partition isVersionEqualityConstraint

isVersionEqualityConstraint :: LabeledPackageConstraint -> Bool
isVersionEqualityConstraint (LabeledPackageConstraint constraint source)
    | ConstraintSourceProjectConfig{} <- source
    , PackageConstraint _ (PackagePropertyVersion versionRange) <- constraint
    , ThisVersionF _ <- projectVersionRange versionRange = True
    | otherwise = False

sortLabeled :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
sortLabeled = sortBy (comparing (\(LabeledPackageConstraint _ src) -> case src of
    ConstraintSourceProjectConfig pci -> importDepth pci
    _ -> maxBound))

weedLabeled :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedLabeled xsSorted = case xsSorted of
    [] -> []
    (LabeledPackageConstraint _ srcX) : _ -> case srcX of
        ConstraintSourceProjectConfig ProjectConfigImport{importDepth = dX} ->
            filter
                (\(LabeledPackageConstraint _ srcY) -> case srcY of
                    ConstraintSourceProjectConfig ProjectConfigImport{importDepth = dY} ->
                        dX == dY
                    _ -> False)
                xsSorted
        _ -> xsSorted