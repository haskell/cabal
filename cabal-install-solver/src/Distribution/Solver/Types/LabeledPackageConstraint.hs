{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , unlabelPackageConstraint
    , shallowConstraintsWin
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
-- any user targets to win if they exist. Otherwise pick version equality
-- constraints with the lowest import depth to win. Discard the rest of the
-- version equality and installed constraints.  Constraints for flags and
-- stanzas are untouched by this weeding.
-- 
-- Flags that may have applied to weeded versions of a package may be orphaned.
shallowConstraintsWin :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
shallowConstraintsWin =
    (\(xs, (us, ys)) ->
        let toKeyValues :: [(String, LabeledPackageConstraint)] -> (String, [LabeledPackageConstraint])
            toKeyValues kvs = (case kvs of (s, _) : _ -> (s,); [] -> ("",)) $ snd <$> kvs

            xsGrouped :: [(String, [LabeledPackageConstraint])]
            xsGrouped = toKeyValues <$> groupBy (\x y -> EQ == (comparing fst) x y)
                [ (show . unscopePackageConstraint $ unlabelPackageConstraint x, x)
                | x <- xs
                ]

            xsWeeded = (weedByDepth . weedByUser us . sortLabeled) <$> Map.fromList xsGrouped
        in concat (Map.elems xsWeeded) ++ us ++ ys
    )
    . fmap (partition isUserVersionEquality)
    . partition (\c -> isProjectVersionInstalled c || isProjectVersionEquality c)

isUserVersionEquality :: LabeledPackageConstraint -> Bool
isUserVersionEquality (LabeledPackageConstraint constraint source)
    | ConstraintSourceUserTarget{} <- source
    , PackageConstraint _ (PackagePropertyVersion versionRange) <- constraint
    , ThisVersionF _ <- projectVersionRange versionRange = True
    | otherwise = False

isProjectVersionEquality :: LabeledPackageConstraint -> Bool
isProjectVersionEquality (LabeledPackageConstraint constraint source)
    | ConstraintSourceProjectConfig{} <- source
    , PackageConstraint _ (PackagePropertyVersion versionRange) <- constraint
    , ThisVersionF _ <- projectVersionRange versionRange = True
    | otherwise = False

isProjectVersionInstalled :: LabeledPackageConstraint -> Bool
isProjectVersionInstalled (LabeledPackageConstraint constraint source)
    | ConstraintSourceProjectConfig{} <- source
    , PackageConstraint _ PackagePropertyInstalled <- constraint = True
    | otherwise = False

sortLabeled :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
sortLabeled = sortBy (comparing (\(LabeledPackageConstraint _ src) -> case src of
    ConstraintSourceProjectConfig pci -> importDepth pci
    _ -> maxBound))

-- | Weed out any conflicts by picking user constraints over project
-- constraints.
weedByUser :: [LabeledPackageConstraint] -> [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedByUser us xs = case us of
    [] -> xs
    (toName -> uName) : us' -> weedByUser us' $ filter (\x -> uName /= toName x) xs
    where
        toName = scopeToPackageName . unscopePackageConstraint . unlabelPackageConstraint

-- | Weed out any conflicts by picking project constraints with the lowest
-- import depth, assuming the input is sorted by import depth.
weedByDepth :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedByDepth xs = case xs of
    [] -> []
    (LabeledPackageConstraint _ srcX) : _ -> case srcX of
        ConstraintSourceProjectConfig ProjectConfigImport{importDepth = dX} ->
            filter
                (\(LabeledPackageConstraint _ srcY) -> case srcY of
                    ConstraintSourceProjectConfig ProjectConfigImport{importDepth = dY} ->
                        dX == dY
                    _ -> False)
                xs
        _ -> xs