{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

-- Verbosity for Cabal functions.

-- |
-- Module      :  Distribution.Verbosity
-- Copyright   :  Ian Lynagh 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A 'Verbosity' type with associated utilities.
--
-- There are 4 standard verbosity levels from 'silent', 'normal',
-- 'verbose' up to 'deafening'. This is used for deciding what logging
-- messages to print.
--
-- Verbosity also is equipped with some internal settings which can be
-- used to control at a fine granularity the verbosity of specific
-- settings (e.g., so that you can trace only particular things you
-- are interested in.)  It's important to note that the instances
-- for 'Verbosity' assume that this does not exist.
module Distribution.Verbosity
  ( -- * Verbosity
    Verbosity
  , silent
  , normal
  , verbose
  , deafening
  , moreVerbose
  , lessVerbose
  , isVerboseQuiet
  , intToVerbosity
  , flagToVerbosity
  , showForCabal
  , showForGHC
  , verboseNoFlags
  , verboseHasFlags
  , modifyVerbosity

    -- * Call stacks
  , verboseCallSite
  , verboseCallStack
  , isVerboseCallSite
  , isVerboseCallStack

    -- * Output markets
  , verboseMarkOutput
  , isVerboseMarkOutput
  , verboseUnmarkOutput

    -- * Line wrapping
  , verboseNoWrap
  , isVerboseNoWrap

    -- * Time stamps
  , verboseTimestamp
  , isVerboseTimestamp
  , verboseNoTimestamp

    -- * Stderr
  , verboseStderr
  , isVerboseStderr
  , verboseNoStderr

    -- * No warnings
  , verboseNoWarn
  , isVerboseNoWarn
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ReadE

import Data.List (elemIndex)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (isAsciiAlpha)
import Distribution.Verbosity.Internal

import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

data Verbosity = Verbosity
  { vLevel :: VerbosityLevel
  , vFlags :: Set VerbosityFlag
  , vQuiet :: Bool
  }
  deriving (Generic, Show, Read, Typeable)

mkVerbosity :: VerbosityLevel -> Verbosity
mkVerbosity l = Verbosity{vLevel = l, vFlags = Set.empty, vQuiet = False}

instance Eq Verbosity where
  x == y = vLevel x == vLevel y

instance Ord Verbosity where
  compare x y = compare (vLevel x) (vLevel y)

instance Enum Verbosity where
  toEnum = mkVerbosity . toEnum
  fromEnum = fromEnum . vLevel

instance Bounded Verbosity where
  minBound = mkVerbosity minBound
  maxBound = mkVerbosity maxBound

instance Binary Verbosity
instance Structured Verbosity

-- | In 'silent' mode, we should not print /anything/ unless an error occurs.
silent :: Verbosity
silent = mkVerbosity Silent

-- | Print stuff we want to see by default.
normal :: Verbosity
normal = mkVerbosity Normal

-- | Be more verbose about what's going on.
verbose :: Verbosity
verbose = mkVerbosity Verbose

-- | Not only are we verbose ourselves (perhaps even noisier than when
-- being 'verbose'), but we tell everything we run to be verbose too.
deafening :: Verbosity
deafening = mkVerbosity Deafening

-- | Increase verbosity level, but stay 'silent' if we are.
moreVerbose :: Verbosity -> Verbosity
moreVerbose v =
  case vLevel v of
    Silent -> v -- silent should stay silent
    Normal -> v{vLevel = Verbose}
    Verbose -> v{vLevel = Deafening}
    Deafening -> v

-- | Decrease verbosity level, but stay 'deafening' if we are.
lessVerbose :: Verbosity -> Verbosity
lessVerbose v =
  verboseQuiet $
    case vLevel v of
      Deafening -> v -- deafening stays deafening
      Verbose -> v{vLevel = Normal}
      Normal -> v{vLevel = Silent}
      Silent -> v

-- | Combinator for transforming verbosity level while retaining the
-- original hidden state.
--
-- For instance, the following property holds
--
-- prop> isVerboseNoWrap (modifyVerbosity (max verbose) v) == isVerboseNoWrap v
--
-- __Note__: you can use @modifyVerbosity (const v1) v0@ to overwrite
-- @v1@'s flags with @v0@'s flags.
--
-- @since 2.0.1.0
modifyVerbosity :: (Verbosity -> Verbosity) -> Verbosity -> Verbosity
modifyVerbosity f v = v{vLevel = vLevel (f v)}

-- | Numeric verbosity level @0..3@: @0@ is 'silent', @3@ is 'deafening'.
intToVerbosity :: Int -> Maybe Verbosity
intToVerbosity 0 = Just (mkVerbosity Silent)
intToVerbosity 1 = Just (mkVerbosity Normal)
intToVerbosity 2 = Just (mkVerbosity Verbose)
intToVerbosity 3 = Just (mkVerbosity Deafening)
intToVerbosity _ = Nothing

-- | Parser verbosity
--
-- >>> explicitEitherParsec parsecVerbosity "normal"
-- Right (Verbosity {vLevel = Normal, vFlags = fromList [], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap  "
-- Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap +markoutput"
-- Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal +nowrap +markoutput"
-- Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap+markoutput"
-- Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "deafening+nowrap+stdout+stderr+callsite+callstack"
-- Right (Verbosity {vLevel = Deafening, vFlags = fromList [VCallStack,VCallSite,VNoWrap,VStderr], vQuiet = False})
--
-- /Note:/ this parser will eat trailing spaces.
instance Parsec Verbosity where
  parsec = parsecVerbosity

instance Pretty Verbosity where
  pretty = PP.text . showForCabal

parsecVerbosity :: CabalParsing m => m Verbosity
parsecVerbosity = parseIntVerbosity <|> parseStringVerbosity
  where
    parseIntVerbosity = do
      i <- P.integral
      case intToVerbosity i of
        Just v -> return v
        Nothing -> P.unexpected $ "Bad integral verbosity: " ++ show i ++ ". Valid values are 0..3"

    parseStringVerbosity = do
      level <- parseVerbosityLevel
      _ <- P.spaces
      flags <- many (parseFlag <* P.spaces)
      return $ foldl' (flip ($)) (mkVerbosity level) flags

    parseVerbosityLevel = do
      token <- P.munch1 isAsciiAlpha
      case token of
        "silent" -> return Silent
        "normal" -> return Normal
        "verbose" -> return Verbose
        "debug" -> return Deafening
        "deafening" -> return Deafening
        _ -> P.unexpected $ "Bad verbosity level: " ++ token
    parseFlag = do
      _ <- P.char '+'
      token <- P.munch1 isAsciiAlpha
      case token of
        "callsite" -> return verboseCallSite
        "callstack" -> return verboseCallStack
        "nowrap" -> return verboseNoWrap
        "markoutput" -> return verboseMarkOutput
        "timestamp" -> return verboseTimestamp
        "stderr" -> return verboseStderr
        "stdout" -> return verboseNoStderr
        "nowarn" -> return verboseNoWarn
        _ -> P.unexpected $ "Bad verbosity flag: " ++ token

flagToVerbosity :: ReadE Verbosity
flagToVerbosity = parsecToReadE id parsecVerbosity

showForCabal :: Verbosity -> String
showForCabal v
  | Set.null (vFlags v) =
      maybe (error "unknown verbosity") show $
        elemIndex v [silent, normal, verbose, deafening]
  | otherwise =
      unwords $
        showLevel (vLevel v)
          : concatMap showFlag (Set.toList (vFlags v))
  where
    showLevel Silent = "silent"
    showLevel Normal = "normal"
    showLevel Verbose = "verbose"
    showLevel Deafening = "debug"

    showFlag VCallSite = ["+callsite"]
    showFlag VCallStack = ["+callstack"]
    showFlag VNoWrap = ["+nowrap"]
    showFlag VMarkOutput = ["+markoutput"]
    showFlag VTimestamp = ["+timestamp"]
    showFlag VStderr = ["+stderr"]
    showFlag VNoWarn = ["+nowarn"]

showForGHC :: Verbosity -> String
showForGHC v =
  maybe (error "unknown verbosity") show $
    elemIndex v [silent, normal, __, verbose, deafening]
  where
    __ = silent -- this will be always ignored by elemIndex

-- | Turn on verbose call-site printing when we log.
verboseCallSite :: Verbosity -> Verbosity
verboseCallSite = verboseFlag VCallSite

-- | Turn on verbose call-stack printing when we log.
verboseCallStack :: Verbosity -> Verbosity
verboseCallStack = verboseFlag VCallStack

-- | Turn on @-----BEGIN CABAL OUTPUT-----@ markers for output
-- from Cabal (as opposed to GHC, or system dependent).
verboseMarkOutput :: Verbosity -> Verbosity
verboseMarkOutput = verboseFlag VMarkOutput

-- | Turn off marking; useful for suppressing nondeterministic output.
verboseUnmarkOutput :: Verbosity -> Verbosity
verboseUnmarkOutput = verboseNoFlag VMarkOutput

-- | Disable line-wrapping for log messages.
verboseNoWrap :: Verbosity -> Verbosity
verboseNoWrap = verboseFlag VNoWrap

-- | Mark the verbosity as quiet.
verboseQuiet :: Verbosity -> Verbosity
verboseQuiet v = v{vQuiet = True}

-- | Turn on timestamps for log messages.
verboseTimestamp :: Verbosity -> Verbosity
verboseTimestamp = verboseFlag VTimestamp

-- | Turn off timestamps for log messages.
verboseNoTimestamp :: Verbosity -> Verbosity
verboseNoTimestamp = verboseNoFlag VTimestamp

-- | Switch logging to 'stderr'.
--
-- @since 3.4.0.0
verboseStderr :: Verbosity -> Verbosity
verboseStderr = verboseFlag VStderr

-- | Switch logging to 'stdout'.
--
-- @since 3.4.0.0
verboseNoStderr :: Verbosity -> Verbosity
verboseNoStderr = verboseNoFlag VStderr

-- | Turn off warnings for log messages.
verboseNoWarn :: Verbosity -> Verbosity
verboseNoWarn = verboseFlag VNoWarn

-- | Helper function for flag enabling functions.
verboseFlag :: VerbosityFlag -> (Verbosity -> Verbosity)
verboseFlag flag v = v{vFlags = Set.insert flag (vFlags v)}

-- | Helper function for flag disabling functions.
verboseNoFlag :: VerbosityFlag -> (Verbosity -> Verbosity)
verboseNoFlag flag v = v{vFlags = Set.delete flag (vFlags v)}

-- | Turn off all flags.
verboseNoFlags :: Verbosity -> Verbosity
verboseNoFlags v = v{vFlags = Set.empty}

verboseHasFlags :: Verbosity -> Bool
verboseHasFlags = not . Set.null . vFlags

-- | Test if we should output call sites when we log.
isVerboseCallSite :: Verbosity -> Bool
isVerboseCallSite = isVerboseFlag VCallSite

-- | Test if we should output call stacks when we log.
isVerboseCallStack :: Verbosity -> Bool
isVerboseCallStack = isVerboseFlag VCallStack

-- | Test if we should output markets.
isVerboseMarkOutput :: Verbosity -> Bool
isVerboseMarkOutput = isVerboseFlag VMarkOutput

-- | Test if line-wrapping is disabled for log messages.
isVerboseNoWrap :: Verbosity -> Bool
isVerboseNoWrap = isVerboseFlag VNoWrap

-- | Test if we had called 'lessVerbose' on the verbosity.
isVerboseQuiet :: Verbosity -> Bool
isVerboseQuiet = vQuiet

-- | Test if we should output timestamps when we log.
isVerboseTimestamp :: Verbosity -> Bool
isVerboseTimestamp = isVerboseFlag VTimestamp

-- | Test if we should output to 'stderr' when we log.
--
-- @since 3.4.0.0
isVerboseStderr :: Verbosity -> Bool
isVerboseStderr = isVerboseFlag VStderr

-- | Test if we should output warnings when we log.
isVerboseNoWarn :: Verbosity -> Bool
isVerboseNoWarn = isVerboseFlag VNoWarn

-- | Helper function for flag testing functions.
isVerboseFlag :: VerbosityFlag -> Verbosity -> Bool
isVerboseFlag flag = (Set.member flag) . vFlags

-- $setup
-- >>> import Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum)
-- >>> instance Arbitrary VerbosityLevel where arbitrary = arbitraryBoundedEnum
-- >>> instance Arbitrary Verbosity where arbitrary = fmap mkVerbosity arbitrary
