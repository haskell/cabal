{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

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
  ( -- * Rich verbosity
    Verbosity (..)
  , VerbosityHandles (..)
  , defaultVerbosityHandles
  , VerbosityLevel (..)
  , verbosityLevel
  , verbosityChosenOutputHandle
  , verbosityErrorHandle
  , modifyVerbosityFlags
  , mkVerbosity
  , setVerbosityHandles

    -- * Verbosity flags
  , VerbosityFlags (vLevel)
  , mkVerbosityFlags
  , makeVerbose
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

    -- * Call stacks
  , verboseCallSite
  , verboseCallStack
  , isVerboseCallSite
  , isVerboseCallStack

    -- * Output markers
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
import Distribution.Utils.Structured
import System.IO (Handle, stderr, stdout)
import qualified Text.PrettyPrint as PP
import qualified Type.Reflection as Typeable

-- | Rich verbosity, used for the Cabal library interface.
data Verbosity = Verbosity
  { verbosityFlags :: VerbosityFlags
  , verbosityHandles :: VerbosityHandles
  }
  deriving (Generic)

-- | Handles to use for logging (e.g. log to stdout, or log to a file).
data VerbosityHandles = VerbosityHandles
  { vStdoutHandle :: Handle
  , vStderrHandle :: Handle
  }

defaultVerbosityHandles :: VerbosityHandles
defaultVerbosityHandles =
  VerbosityHandles
    { vStdoutHandle = stdout
    , vStderrHandle = stderr
    }

-- | Verbosity information which can be passed by the CLI.
data VerbosityFlags = VerbosityFlags
  { vLevel :: VerbosityLevel
  , vFlags :: Set VerbosityFlag
  , vQuiet :: Bool
  }
  deriving (Generic, Show, Read, Eq)

verbosityLevel :: Verbosity -> VerbosityLevel
verbosityLevel = vLevel . verbosityFlags

-- | The handle used for normal output.
--
-- With the @+stderr@ verbosity flag, this is the error handle.
verbosityChosenOutputHandle :: Verbosity -> Handle
verbosityChosenOutputHandle verb =
  if isVerboseStderr (verbosityFlags verb)
    then vStderrHandle $ verbosityHandles verb
    else vStdoutHandle $ verbosityHandles verb

-- | The verbosity handle used for error output.
verbosityErrorHandle :: Verbosity -> Handle
verbosityErrorHandle = vStderrHandle . verbosityHandles

setVerbosityHandles :: Maybe Handle -> Verbosity -> Verbosity
setVerbosityHandles Nothing v = v
setVerbosityHandles (Just h) v =
  v{verbosityHandles = VerbosityHandles{vStdoutHandle = h, vStderrHandle = h}}

mkVerbosity :: VerbosityHandles -> VerbosityFlags -> Verbosity
mkVerbosity handles flags =
  Verbosity
    { verbosityFlags = flags
    , verbosityHandles = handles
    }

modifyVerbosityFlags :: (VerbosityFlags -> VerbosityFlags) -> Verbosity -> Verbosity
modifyVerbosityFlags f v@(Verbosity{verbosityFlags = flags}) =
  v{verbosityFlags = f flags}

mkVerbosityFlags :: VerbosityLevel -> VerbosityFlags
mkVerbosityFlags l = VerbosityFlags{vLevel = l, vFlags = Set.empty, vQuiet = False}

instance Binary VerbosityFlags
instance NFData VerbosityFlags
instance Structured VerbosityFlags

-- Hand-written instances, because there are no NFData/Structured instances
-- for Handle.
instance NFData VerbosityHandles where
  rnf (VerbosityHandles o e) = o `seq` e `seq` ()
instance Structured VerbosityHandles where
  structure _ =
    Structure
      tr
      0
      (show tr)
      [
        ( "VerbosityHandles"
        ,
          [ nominalStructure $ Proxy @Handle
          , nominalStructure $ Proxy @Handle
          ]
        )
      ]
    where
      tr = Typeable.SomeTypeRep $ Typeable.typeRep @VerbosityHandles

instance NFData Verbosity
instance Structured Verbosity

-- | In 'silent' mode, we should not print /anything/ unless an error occurs.
silent :: VerbosityFlags
silent = mkVerbosityFlags Silent

-- | Print stuff we want to see by default.
normal :: VerbosityFlags
normal = mkVerbosityFlags Normal

-- | Be more verbose about what's going on.
verbose :: VerbosityFlags
verbose = mkVerbosityFlags Verbose

-- | Not only are we verbose ourselves (perhaps even noisier than when
-- being 'verbose'), but we tell everything we run to be verbose too.
deafening :: VerbosityFlags
deafening = mkVerbosityFlags Deafening

-- | Increase verbosity level, but stay 'silent' if we are.
moreVerbose :: VerbosityFlags -> VerbosityFlags
moreVerbose v =
  case vLevel v of
    Silent -> v -- silent should stay silent
    Normal -> v{vLevel = Verbose}
    Verbose -> v{vLevel = Deafening}
    Deafening -> v

-- | Make sure the verbosity level is at least 'verbose',
-- but stay 'silent' if we are.
makeVerbose :: VerbosityFlags -> VerbosityFlags
makeVerbose v =
  case vLevel v of
    Silent -> v -- silent should stay silent
    Normal -> v{vLevel = Verbose}
    Verbose -> v
    Deafening -> v

-- | Decrease verbosity level, but stay 'deafening' if we are.
lessVerbose :: VerbosityFlags -> VerbosityFlags
lessVerbose v =
  verboseQuiet $
    case vLevel v of
      Deafening -> v -- deafening stays deafening
      Verbose -> v{vLevel = Normal}
      Normal -> v{vLevel = Silent}
      Silent -> v

-- | Numeric verbosity level @0..3@: @0@ is 'silent', @3@ is 'deafening'.
intToVerbosity :: Int -> Maybe VerbosityFlags
intToVerbosity 0 = Just (mkVerbosityFlags Silent)
intToVerbosity 1 = Just (mkVerbosityFlags Normal)
intToVerbosity 2 = Just (mkVerbosityFlags Verbose)
intToVerbosity 3 = Just (mkVerbosityFlags Deafening)
intToVerbosity _ = Nothing

-- | Parser verbosity
--
-- >>> explicitEitherParsec parsecVerbosity "normal"
-- Right (VerbosityFlags {vLevel = Normal, vFlags = fromList [], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap  "
-- Right (VerbosityFlags {vLevel = Normal, vFlags = fromList [VNoWrap], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap +markoutput"
-- Right (VerbosityFlags {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal +nowrap +markoutput"
-- Right (VerbosityFlags {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "normal+nowrap+markoutput"
-- Right (VerbosityFlags {vLevel = Normal, vFlags = fromList [VNoWrap,VMarkOutput], vQuiet = False})
--
-- >>> explicitEitherParsec parsecVerbosity "deafening+nowrap+stdout+stderr+callsite+callstack"
-- Right (VerbosityFlags {vLevel = Deafening, vFlags = fromList [VCallStack,VCallSite,VNoWrap,VStderr], vQuiet = False})
--
-- /Note:/ this parser will eat trailing spaces.
instance Parsec VerbosityFlags where
  parsec = parsecVerbosity

instance Pretty VerbosityFlags where
  pretty = PP.text . showForCabal

parsecVerbosity :: CabalParsing m => m VerbosityFlags
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
      return $ foldl' (flip ($)) (mkVerbosityFlags level) flags

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

flagToVerbosity :: ReadE VerbosityFlags
flagToVerbosity = parsecToReadE id parsecVerbosity

showForCabal :: VerbosityFlags -> String
showForCabal (VerbosityFlags{vLevel = lvl, vFlags = flags})
  | Set.null flags =
      maybe (error "unknown verbosity") show $
        elemIndex lvl [Silent, Normal, Verbose, Deafening]
  | otherwise =
      unwords $
        showLevel lvl
          : concatMap showFlag (Set.toList flags)
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

showForGHC :: VerbosityFlags -> String
showForGHC v =
  maybe (error "unknown verbosity") show $
    elemIndex (vLevel v) [Silent, Normal, __, Verbose, Deafening]
  where
    __ = Silent -- this will be always ignored by elemIndex

-- | Turn on verbose call-site printing when we log.
verboseCallSite :: VerbosityFlags -> VerbosityFlags
verboseCallSite = verboseFlag VCallSite

-- | Turn on verbose call-stack printing when we log.
verboseCallStack :: VerbosityFlags -> VerbosityFlags
verboseCallStack = verboseFlag VCallStack

-- | Turn on @-----BEGIN CABAL OUTPUT-----@ markers for output
-- from Cabal (as opposed to GHC, or system dependent).
verboseMarkOutput :: VerbosityFlags -> VerbosityFlags
verboseMarkOutput = verboseFlag VMarkOutput

-- | Turn off marking; useful for suppressing nondeterministic output.
verboseUnmarkOutput :: VerbosityFlags -> VerbosityFlags
verboseUnmarkOutput = verboseNoFlag VMarkOutput

-- | Disable line-wrapping for log messages.
verboseNoWrap :: VerbosityFlags -> VerbosityFlags
verboseNoWrap = verboseFlag VNoWrap

-- | Mark the verbosity as quiet.
verboseQuiet :: VerbosityFlags -> VerbosityFlags
verboseQuiet v = v{vQuiet = True}

-- | Turn on timestamps for log messages.
verboseTimestamp :: VerbosityFlags -> VerbosityFlags
verboseTimestamp = verboseFlag VTimestamp

-- | Turn off timestamps for log messages.
verboseNoTimestamp :: VerbosityFlags -> VerbosityFlags
verboseNoTimestamp = verboseNoFlag VTimestamp

-- | Switch logging to 'stderr'.
--
-- @since 3.4.0.0
verboseStderr :: VerbosityFlags -> VerbosityFlags
verboseStderr = verboseFlag VStderr

-- | Switch logging to 'stdout'.
--
-- @since 3.4.0.0
verboseNoStderr :: VerbosityFlags -> VerbosityFlags
verboseNoStderr = verboseNoFlag VStderr

-- | Turn off warnings for log messages.
verboseNoWarn :: VerbosityFlags -> VerbosityFlags
verboseNoWarn = verboseFlag VNoWarn

-- | Helper function for flag enabling functions.
verboseFlag :: VerbosityFlag -> (VerbosityFlags -> VerbosityFlags)
verboseFlag flag v@(VerbosityFlags{vFlags = flags}) = v{vFlags = Set.insert flag flags}

-- | Helper function for flag disabling functions.
verboseNoFlag :: VerbosityFlag -> (VerbosityFlags -> VerbosityFlags)
verboseNoFlag flag v@(VerbosityFlags{vFlags = flags}) = v{vFlags = Set.delete flag flags}

-- | Turn off all flags.
verboseNoFlags :: VerbosityFlags -> VerbosityFlags
verboseNoFlags v = v{vFlags = Set.empty}

verboseHasFlags :: VerbosityFlags -> Bool
verboseHasFlags (VerbosityFlags{vFlags = flags}) = not $ Set.null flags

-- | Test if we should output call sites when we log.
isVerboseCallSite :: VerbosityFlags -> Bool
isVerboseCallSite = isVerboseFlag VCallSite

-- | Test if we should output call stacks when we log.
isVerboseCallStack :: VerbosityFlags -> Bool
isVerboseCallStack = isVerboseFlag VCallStack

-- | Test if we should output markers.
isVerboseMarkOutput :: VerbosityFlags -> Bool
isVerboseMarkOutput = isVerboseFlag VMarkOutput

-- | Test if line-wrapping is disabled for log messages.
isVerboseNoWrap :: VerbosityFlags -> Bool
isVerboseNoWrap = isVerboseFlag VNoWrap

-- | Test if we had called 'lessVerbose' on the verbosity.
isVerboseQuiet :: VerbosityFlags -> Bool
isVerboseQuiet = vQuiet

-- | Test if we should output timestamps when we log.
isVerboseTimestamp :: VerbosityFlags -> Bool
isVerboseTimestamp = isVerboseFlag VTimestamp

-- | Test if we should output to 'stderr' when we log.
--
-- @since 3.4.0.0
isVerboseStderr :: VerbosityFlags -> Bool
isVerboseStderr = isVerboseFlag VStderr

-- | Test if we should output warnings when we log.
isVerboseNoWarn :: VerbosityFlags -> Bool
isVerboseNoWarn = isVerboseFlag VNoWarn

-- | Helper function for flag testing functions.
isVerboseFlag :: VerbosityFlag -> VerbosityFlags -> Bool
isVerboseFlag flag v = flag `Set.member` vFlags v

-- $setup
-- >>> import Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum)
-- >>> instance Arbitrary VerbosityLevel where arbitrary = arbitraryBoundedEnum
-- >>> instance Arbitrary VerbosityFlags where arbitrary = fmap mkVerbosityFlags arbitrary
