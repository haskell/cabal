{-# LANGUAGE LambdaCase #-}

module Distribution.Simple.BuildWay where

data BuildWay = StaticWay | DynWay | ProfWay | ProfDynWay
  deriving (Eq, Ord, Show, Read, Enum)

-- | Returns the object/interface extension prefix for the given build way (e.g. "dyn_" for 'DynWay')
buildWayPrefix :: BuildWay -> String
buildWayPrefix = \case
  StaticWay -> ""
  ProfWay -> "p_"
  DynWay -> "dyn_"
  ProfDynWay -> "p_dyn_"
