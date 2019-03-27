{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
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

-- Verbosity for Cabal functions.

module Distribution.Verbosity (
  -- * Verbosity
  Verbosity,
  silent, normal, verbose, deafening,
  moreVerbose, lessVerbose, isVerboseQuiet,
  intToVerbosity, flagToVerbosity,
  showForCabal, showForGHC,
  verboseNoFlags, verboseHasFlags,
  modifyVerbosity,

  -- * Call stacks
  verboseCallSite, verboseCallStack,
  isVerboseCallSite, isVerboseCallStack,

  -- * Output markets
  verboseMarkOutput, isVerboseMarkOutput,
  verboseUnmarkOutput,

  -- * line-wrapping
  verboseNoWrap, isVerboseNoWrap,

  -- * timestamps
  verboseTimestamp, isVerboseTimestamp,
  verboseNoTimestamp,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.ReadE

import Data.List (elemIndex)
import Data.Set (Set)
import Distribution.Parsec

import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as P

data Verbosity = Verbosity {
    vLevel :: VerbosityLevel,
    vFlags :: Set VerbosityFlag,
    vQuiet :: Bool
  } deriving (Generic, Show, Read)

mkVerbosity :: VerbosityLevel -> Verbosity
mkVerbosity l = Verbosity { vLevel = l, vFlags = Set.empty, vQuiet = False }

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

data VerbosityLevel = Silent | Normal | Verbose | Deafening
    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance Binary VerbosityLevel

-- We shouldn't print /anything/ unless an error occurs in silent mode
silent :: Verbosity
silent = mkVerbosity Silent

-- Print stuff we want to see by default
normal :: Verbosity
normal = mkVerbosity Normal

-- Be more verbose about what's going on
verbose :: Verbosity
verbose = mkVerbosity Verbose

-- Not only are we verbose ourselves (perhaps even noisier than when
-- being "verbose"), but we tell everything we run to be verbose too
deafening :: Verbosity
deafening = mkVerbosity Deafening

moreVerbose :: Verbosity -> Verbosity
moreVerbose v =
    case vLevel v of
        Silent    -> v -- silent should stay silent
        Normal    -> v { vLevel = Verbose }
        Verbose   -> v { vLevel = Deafening }
        Deafening -> v

lessVerbose :: Verbosity -> Verbosity
lessVerbose v =
    verboseQuiet $
    case vLevel v of
        Deafening -> v -- deafening stays deafening
        Verbose   -> v { vLevel = Normal }
        Normal    -> v { vLevel = Silent }
        Silent    -> v

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
modifyVerbosity f v = v { vLevel = vLevel (f v) }

intToVerbosity :: Int -> Maybe Verbosity
intToVerbosity 0 = Just (mkVerbosity Silent)
intToVerbosity 1 = Just (mkVerbosity Normal)
intToVerbosity 2 = Just (mkVerbosity Verbose)
intToVerbosity 3 = Just (mkVerbosity Deafening)
intToVerbosity _ = Nothing

-- | Parser verbosity
--
-- >>> explicitEitherParsec parsecVerbosity "normal"
-- Right (Right (Verbosity {vLevel = Normal, vFlags = fromList [], vQuiet = False}))
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap  "
-- Right (Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap], vQuiet = False}))
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap +markoutput"
-- Right (Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False}))
--
-- >>> explicitEitherParsec parsecVerbosity "normal +nowrap +markoutput"
-- Right (Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False}))
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap+markoutput"
-- Right (Right (Verbosity {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False}))
--
-- /Note:/ this parser will eat trailing spaces.
--
parsecVerbosity :: CabalParsing m => m (Either Int Verbosity)
parsecVerbosity = parseIntVerbosity <|> parseStringVerbosity
  where
    parseIntVerbosity = fmap Left P.integral
    parseStringVerbosity = fmap Right $ do
        level <- parseVerbosityLevel
        _ <- P.spaces
        extras <- many (parseExtra <* P.spaces)
        return (foldr (.) id extras (mkVerbosity level))
    parseVerbosityLevel = P.choice
        [ P.string "silent" >> return Silent
        , P.string "normal" >> return Normal
        , P.string "verbose" >> return Verbose
        , P.string "debug"  >> return Deafening
        , P.string "deafening" >> return Deafening
        ]
    parseExtra = P.char '+' >> P.choice
        [ P.string "callsite"  >> return verboseCallSite
        , P.string "callstack" >> return verboseCallStack
        , P.string "nowrap"    >> return verboseNoWrap
        , P.string "markoutput" >> return verboseMarkOutput
        , P.string "timestamp" >> return verboseTimestamp
        ]

flagToVerbosity :: ReadE Verbosity
flagToVerbosity = parsecToReadE id $ do
    e <- parsecVerbosity
    case e of
       Right v -> return v
       Left i -> case intToVerbosity i of
           Just v  -> return v
           Nothing -> fail $ "Bad verbosity: " ++ show i ++ ". Valid values are 0..3"

showForCabal, showForGHC :: Verbosity -> String

showForCabal v
    | Set.null (vFlags v)
    = maybe (error "unknown verbosity") show $
        elemIndex v [silent,normal,verbose,deafening]
    | otherwise
    = unwords $ (case vLevel v of
                    Silent -> "silent"
                    Normal -> "normal"
                    Verbose -> "verbose"
                    Deafening -> "debug")
              : concatMap showFlag (Set.toList (vFlags v))
  where
    showFlag VCallSite   = ["+callsite"]
    showFlag VCallStack  = ["+callstack"]
    showFlag VNoWrap     = ["+nowrap"]
    showFlag VMarkOutput = ["+markoutput"]
    showFlag VTimestamp  = ["+timestamp"]
showForGHC   v = maybe (error "unknown verbosity") show $
    elemIndex v [silent,normal,__,verbose,deafening]
        where __ = silent -- this will be always ignored by elemIndex

data VerbosityFlag
    = VCallStack
    | VCallSite
    | VNoWrap
    | VMarkOutput
    | VTimestamp
    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance Binary VerbosityFlag

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

-- | Mark the verbosity as quiet
verboseQuiet :: Verbosity -> Verbosity
verboseQuiet v = v { vQuiet = True }

-- | Turn on timestamps for log messages.
verboseTimestamp :: Verbosity -> Verbosity
verboseTimestamp = verboseFlag VTimestamp

-- | Turn off timestamps for log messages.
verboseNoTimestamp :: Verbosity -> Verbosity
verboseNoTimestamp = verboseNoFlag VTimestamp

-- | Helper function for flag enabling functions
verboseFlag :: VerbosityFlag -> (Verbosity -> Verbosity)
verboseFlag flag v = v { vFlags = Set.insert flag (vFlags v) }

-- | Helper function for flag disabling functions
verboseNoFlag :: VerbosityFlag -> (Verbosity -> Verbosity)
verboseNoFlag flag v = v { vFlags = Set.delete flag (vFlags v) }

-- | Turn off all flags
verboseNoFlags :: Verbosity -> Verbosity
verboseNoFlags v = v { vFlags = Set.empty }

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

-- | Test if we had called 'lessVerbose' on the verbosity
isVerboseQuiet :: Verbosity -> Bool
isVerboseQuiet = vQuiet

-- | Test if if we should output timestamps when we log.
isVerboseTimestamp :: Verbosity -> Bool
isVerboseTimestamp = isVerboseFlag VTimestamp

-- | Helper function for flag testing functions.
isVerboseFlag :: VerbosityFlag -> Verbosity -> Bool
isVerboseFlag flag = (Set.member flag) . vFlags

-- $setup
-- >>> import Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum)
-- >>> instance Arbitrary VerbosityLevel where arbitrary = arbitraryBoundedEnum
-- >>> instance Arbitrary Verbosity where arbitrary = fmap mkVerbosity arbitrary
