{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Flag
    ( FInfo(..)
    , Flag
    , FlagInfo
    , FN(..)
    , QFN
    , QSN
    , SN(..)
    , WeakOrTrivial(..)
    , mkFlag
    , showFBool
    , showQFN
    , showQFNBool
    , showQSN
    , showQSNBool
    ) where

import Data.Map as M
import Prelude hiding (pi)

import Distribution.PackageDescription hiding (Flag) -- from Cabal

import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.OptionalStanza

-- | Flag name. Consists of a package instance and the flag identifier itself.
data FN qpn = FN (PI qpn) Flag
  deriving (Eq, Ord, Show, Functor)

-- | Flag identifier. Just a string.
type Flag = FlagName

unFlag :: Flag -> String
unFlag (FlagName fn) = fn

mkFlag :: String -> Flag
mkFlag fn = FlagName fn

-- | Flag info. Default value, whether the flag is manual, and
-- whether the flag is weak. Manual flags can only be set explicitly.
-- Weak flags are typically deferred by the solver.
data FInfo = FInfo { fdefault :: Bool, fmanual :: Bool, fweak :: WeakOrTrivial }
  deriving (Eq, Ord, Show)

-- | Flag defaults.
type FlagInfo = Map Flag FInfo

-- | Qualified flag name.
type QFN = FN QPN

-- | Stanza name. Paired with a package name, much like a flag.
data SN qpn = SN (PI qpn) OptionalStanza
  deriving (Eq, Ord, Show, Functor)

-- | Qualified stanza name.
type QSN = SN QPN

-- | A property of flag and stanza choices that determines whether the
-- choice should be deferred in the solving process.
--
-- A choice is called weak if we do want to defer it. This is the
-- case for flags that should be implied by what's currently installed on
-- the system, as opposed to flags that are used to explicitly enable or
-- disable some functionality.
--
-- A choice is called trivial if it clearly does not matter. The
-- special case of triviality we actually consider is if there are no new
-- dependencies introduced by the choice.
newtype WeakOrTrivial = WeakOrTrivial { unWeakOrTrivial :: Bool }
  deriving (Eq, Ord, Show)

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
