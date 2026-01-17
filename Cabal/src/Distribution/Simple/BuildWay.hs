{-# LANGUAGE LambdaCase #-}

module Distribution.Simple.BuildWay
  ( BuildWay (..)
  , buildWayObjectExtension
  , buildWayInterfaceExtension
  ) where

data BuildWay = StaticWay | DynWay | ProfWay | ProfDynWay
  deriving (Eq, Ord, Show, Read, Enum)

-- | Returns the object extension for the given build way (e.g. "dyn_o" for 'DynWay' on ELF)
buildWayObjectExtension :: String -> BuildWay -> String
buildWayObjectExtension ext = \case
  StaticWay -> ext
  ProfWay -> "p_" ++ ext
  DynWay -> "dyn_" ++ ext
  ProfDynWay -> "p_dyn_" ++ ext

buildWayInterfaceExtension :: BuildWay -> String
buildWayInterfaceExtension = buildWayObjectExtension "hi"
