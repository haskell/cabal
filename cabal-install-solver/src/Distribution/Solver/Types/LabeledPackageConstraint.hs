{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint(..)
    , VersionWin (..)
    , unlabelPackageConstraint
    , versionWin
    , showVersionWin
    , showGroupedConstraints
    , showLabeledConstraint
    , showLabeledConstraints
    ) where

import Distribution.Compat.Prelude
import Prelude ()
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Pretty ( Pretty(pretty) )
import Distribution.Parsec ( Parsec(parsec) )

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP
import Distribution.Types.VersionRange
import qualified Data.Map.Strict as Map
import qualified Data.List as L (elemIndex, groupBy)

data VersionWin = ShallowWins | LastWins deriving (Eq, Generic)

instance Binary VersionWin
instance Structured VersionWin

instance Show VersionWin where
    show = showVersionWin

versionWin :: VersionWin -> [LabeledPackageConstraint] -> [LabeledPackageConstraint]
versionWin ShallowWins = shallowConstraintsWin
versionWin LastWins = laterConstraintsWin

showVersionWin :: VersionWin -> String
showVersionWin ShallowWins = "shallow wins"
showVersionWin LastWins = "last wins"

instance Pretty VersionWin where
  pretty ShallowWins  = PP.text "shallowest"
  pretty LastWins = PP.text "latest"

instance Parsec VersionWin where
  parsec = P.choice
    [ P.string "latest"  >> return LastWins
    , P.string "shallowest" >> return ShallowWins
    ]

-- | 'PackageConstraint' labeled with its source.
data LabeledPackageConstraint
   = LabeledPackageConstraint PackageConstraint ConstraintSource
   deriving Eq

unlabelPackageConstraint :: LabeledPackageConstraint -> PackageConstraint
unlabelPackageConstraint (LabeledPackageConstraint pc _) = pc

unscopePackageConstraint :: PackageConstraint -> ConstraintScope
unscopePackageConstraint (PackageConstraint scope _) = scope

showLabeledConstraint :: LabeledPackageConstraint -> String
showLabeledConstraint (LabeledPackageConstraint pc src) =
    showPackageConstraint pc ++ " (" ++ showConstraintSource src ++ ")"

showLabeledConstraints :: [LabeledPackageConstraint] -> String
showLabeledConstraints = concatMap (("\n  " ++) . showLabeledConstraint)

showGroupedConstraints :: [LabeledPackageConstraint] -> String
showGroupedConstraints xs = concat
    [ concatMap (("\n  " ++) . showLabeledConstraint) vs
    | (_, vs) <- groupConstraints xs
    ]

groupConstraints :: [LabeledPackageConstraint] -> [(String, [LabeledPackageConstraint])]
groupConstraints xs =
    let toKeyValues :: [(String, LabeledPackageConstraint)] -> (String, [LabeledPackageConstraint])
        toKeyValues kvs = (case kvs of (s, _) : _ -> (s,); [] -> ("",)) $ snd <$> kvs
    in
        toKeyValues <$> L.groupBy (\x y -> EQ == (comparing fst) x y)
            [ (show . unscopePackageConstraint $ unlabelPackageConstraint x, x)
            | x <- xs
            ]

-- | Later constraints win over earlier constraints. Preserves the original
-- order of the input constraints in the output constraints.
laterConstraintsWin :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
laterConstraintsWin [] = []
laterConstraintsWin lpcs@[_] = lpcs
laterConstraintsWin lpcsIn =
    let keepers =
            (\(xs, (us, ys)) -> weedByOrder (weedByUser us xs) ++ us ++ ys)
            . fmap (partition isUserVersionEquality)
            $ partition (\c -> isProjectVersionInstalled c || isProjectVersionEquality c) lpcsIn

    in snd <$> sortBy (comparing fst) [(fromMaybe (negate 1) (L.elemIndex k lpcsIn), k) | k <- keepers]

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
        let xsGrouped = groupConstraints xs
            xsWeeded = (weedByDepth . weedByUser us . sortByImportDepth) <$> Map.fromList xsGrouped
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

-- | Sort by import depth, ascending.
sortByImportDepth :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
sortByImportDepth = sortBy (comparing (\(LabeledPackageConstraint _ src) -> case src of
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

-- | Weed out any conflicts by picking the last project constraints, assuming
-- the input list is in definition order.
weedByOrder :: [LabeledPackageConstraint] -> [LabeledPackageConstraint]
weedByOrder [] = []
weedByOrder xs@[_] = xs
weedByOrder (reverse -> xs) = reverse $ go (nub $ toName <$> xs) xs where
    toName = scopeToPackageName . unscopePackageConstraint . unlabelPackageConstraint

    go [] ys = ys
    go (n : ns) ys =
        let sameNames = filter ((== n) . toName) ys
            winner = take 1 sameNames
        in
            go ns (winner ++ filter ((/= n) . toName) ys)