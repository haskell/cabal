module Distribution.Solver.Modular.Flag
    ( FInfo(..)
    , Flag
    , FlagInfo
    , FN(..)
    , QFN
    , QSN
    , Stanza
    , SN(..)
    , WeakOrTrivial(..)
    , FlagValue(..)
    , mkFlag
    , showQFN
    , showQFNBool
    , showFlagValue
    , showQSN
    , showQSNBool
    , showSBool
    ) where

import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (pi)

import qualified Distribution.PackageDescription as P -- from Cabal

import Distribution.Solver.Types.Flag
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath

-- | Flag name. Consists of a package instance and the flag identifier itself.
data FN qpn = FN qpn Flag
  deriving (Eq, Ord, Show, Functor)

-- | Flag identifier. Just a string.
type Flag = P.FlagName

-- | Stanza identifier.
type Stanza = OptionalStanza

unFlag :: Flag -> Text
unFlag = P.unFlagName

mkFlag :: Text -> Flag
mkFlag = P.mkFlagNameText

-- | Flag info. Default value, whether the flag is manual, and
-- whether the flag is weak. Manual flags can only be set explicitly.
-- Weak flags are typically deferred by the solver.
data FInfo = FInfo { fdefault :: Bool, fmanual :: FlagType, fweak :: WeakOrTrivial }
  deriving (Eq, Show)

-- | Flag defaults.
type FlagInfo = Map Flag FInfo

-- | Qualified flag name.
type QFN = FN QPN

-- | Stanza name. Paired with a package name, much like a flag.
data SN qpn = SN qpn Stanza
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

-- | Value shown for a flag in a solver log message. The message can refer to
-- only the true choice, only the false choice, or both choices.
data FlagValue = FlagTrue | FlagFalse | FlagBoth
  deriving (Eq, Show)

showQFNBool :: QFN -> Bool -> String
showQFNBool qfn@(FN qpn _f) b = showQPN qpn ++ ":" ++ showFBool qfn b

showQSNBool :: QSN -> Bool -> String
showQSNBool (SN qpn s) b = showQPN qpn ++ ":" ++ showSBool s b

showFBool :: FN qpn -> Bool -> String
showFBool (FN _ f) v = Text.unpack (P.showFlagValue (f, v))

-- | String representation of a flag-value pair.
showFlagValue :: P.FlagName -> FlagValue -> Text
showFlagValue f FlagTrue  = Text.cons '+' (unFlag f)
showFlagValue f FlagFalse = Text.cons '-' (unFlag f)
showFlagValue f FlagBoth  = Text.pack "+/-" <> unFlag f

showSBool :: Stanza -> Bool -> String
showSBool s True  = "*" ++ showStanza s
showSBool s False = "!" ++ showStanza s

showQFN :: QFN -> String
showQFN (FN qpn f) = showQPN qpn ++ ":" ++ Text.unpack (unFlag f)

showQSN :: QSN -> String
showQSN (SN qpn s) = showQPN qpn ++ ":" ++ showStanza s
