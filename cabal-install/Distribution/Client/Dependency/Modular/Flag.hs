module Distribution.Client.Dependency.Modular.Flag where

import Data.Map as M
import Prelude hiding (pi)

import Distribution.PackageDescription hiding (Flag) -- from Cabal

import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Types (OptionalStanza(..))

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

-- | Flag info. Default value, and whether the flag is manual.
-- Manual flags can only be set explicitly.
data FInfo = FInfo { fdefault :: Bool, fmanual :: Bool }
  deriving (Eq, Ord, Show)

-- | Flag defaults.
type FlagInfo = Map Flag FInfo

-- | Qualified flag name.
type QFN = FN QPN

-- | Stanza name. Paired with a package name, much like a flag.
data SN qpn = SN (PI qpn) OptionalStanza
  deriving (Eq, Ord, Show)

instance Functor SN where
  fmap f (SN x y) = SN (fmap f x) y

-- | Qualified stanza name.
type QSN = SN QPN

unStanza :: OptionalStanza -> String
unStanza TestStanzas  = "test"
unStanza BenchStanzas = "bench"

showQFNBool :: QFN -> Bool -> String
showQFNBool qfn@(FN pi _f) b = showPI pi ++ ":" ++ showFBool qfn b

showQSNBool :: QSN -> Bool -> String
showQSNBool qsn@(SN pi _f) b = showPI pi ++ ":" ++ showSBool qsn b

showFBool :: FN qpn -> Bool -> String
showFBool (FN _ f) True  = "+" ++ unFlag f
showFBool (FN _ f) False = "-" ++ unFlag f

showSBool :: SN qpn -> Bool -> String
showSBool (SN _ s) True  = "*" ++ unStanza s
showSBool (SN _ s) False = "!" ++ unStanza s

showQFN :: QFN -> String
showQFN (FN pi f) = showPI pi ++ ":" ++ unFlag f

showQSN :: QSN -> String
showQSN (SN pi f) = showPI pi ++ ":" ++ unStanza f
