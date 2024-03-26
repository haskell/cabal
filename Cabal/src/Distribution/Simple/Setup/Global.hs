{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Global
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the global command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Global
  ( GlobalFlags (..)
  , emptyGlobalFlags
  , defaultGlobalFlags
  , globalCommand
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Setup.Common
import Distribution.Utils.Path

-- ------------------------------------------------------------

-- * Global flags

-- ------------------------------------------------------------

-- In fact since individual flags types are monoids and these are just sets of
-- flags then they are also monoids pointwise. This turns out to be really
-- useful. The mempty is the set of empty flags and mappend allows us to
-- override specific flags. For example we can start with default flags and
-- override with the ones we get from a file or the command line, or both.

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags
  { globalVersion :: Flag Bool
  , globalNumericVersion :: Flag Bool
  , globalWorkingDir :: Flag (SymbolicPath CWD (Dir Pkg))
  }
  deriving (Generic, Typeable)

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags =
  GlobalFlags
    { globalVersion = Flag False
    , globalNumericVersion = Flag False
    , globalWorkingDir = NoFlag
    }

globalCommand :: [Command action] -> CommandUI GlobalFlags
globalCommand commands =
  CommandUI
    { commandName = ""
    , commandSynopsis = ""
    , commandUsage = \pname ->
        "This Setup program uses the Haskell Cabal Infrastructure.\n"
          ++ "See http://www.haskell.org/cabal/ for more information.\n"
          ++ "\n"
          ++ "Usage: "
          ++ pname
          ++ " [GLOBAL FLAGS] [COMMAND [FLAGS]]\n"
    , commandDescription = Just $ \pname ->
        let
          commands' = commands ++ [commandAddAction helpCommandUI undefined]
          cmdDescs = getNormalCommandDescriptions commands'
          maxlen = maximum $ [length name | (name, _) <- cmdDescs]
          align str = str ++ replicate (maxlen - length str) ' '
         in
          "Commands:\n"
            ++ unlines
              [ "  " ++ align name ++ "    " ++ descr
              | (name, descr) <- cmdDescs
              ]
            ++ "\n"
            ++ "For more information about a command use\n"
            ++ "  "
            ++ pname
            ++ " COMMAND --help\n\n"
            ++ "Typical steps for installing Cabal packages:\n"
            ++ concat
              [ "  " ++ pname ++ " " ++ x ++ "\n"
              | x <- ["configure", "build", "install"]
              ]
    , commandNotes = Nothing
    , commandDefaultFlags = defaultGlobalFlags
    , commandOptions = \_ ->
        [ option
            ['V']
            ["version"]
            "Print version information"
            globalVersion
            (\v flags -> flags{globalVersion = v})
            trueArg
        , option
            []
            ["numeric-version"]
            "Print just the version number"
            globalNumericVersion
            (\v flags -> flags{globalNumericVersion = v})
            trueArg
        , option
            ""
            ["working-dir"]
            "Set working directory"
            globalWorkingDir
            (\v flags -> flags{globalWorkingDir = v})
            (reqSymbolicPathArgFlag "DIR")
        ]
    }

emptyGlobalFlags :: GlobalFlags
emptyGlobalFlags = mempty

instance Monoid GlobalFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup GlobalFlags where
  (<>) = gmappend
