module Distribution.Client.Dependency.Modular.Flag where

import Data.Map as M
import Prelude hiding (pi)

import Distribution.PackageDescription hiding (Flag) -- from Cabal

import Distribution.Client.Dependency.Modular.Package

-- | Flag name. Consists of a package instance and the flag identifier itself.
data FN qpn = FN (PI qpn) Flag
  deriving (Eq, Ord, Show)

-- | Extract the package name from a flag name.
getPN :: FN qpn -> qpn
getPN (FN (PI qpn _) _) = qpn

instance Functor FN where
  fmap f (FN x y) = FN (fmap f x) y

-- | Flag identifier. Just a string.
type Flag = FlagName

unFlag :: Flag -> String
unFlag (FlagName fn) = fn

-- | Flag default. Just a bool.
type FDefault = Bool

-- | Flag defaults.
type FlagDefaults = Map Flag FDefault

-- | Qualified flag name.
type QFN = FN QPN

showQFNBool :: QFN -> Bool -> String
showQFNBool qfn@(FN pi _f) b = showPI pi ++ ":" ++ showFBool qfn b

showFBool :: FN qpn -> Bool -> String
showFBool (FN _ f) True  = "+" ++ unFlag f
showFBool (FN _ f) False = "-" ++ unFlag f

showQFN :: QFN -> String
showQFN (FN pi f) = showPI pi ++ ":" ++ unFlag f
