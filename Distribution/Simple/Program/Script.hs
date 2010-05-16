-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Script
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hc-pkg@ program.
-- Currently only GHC and LHC have hc-pkg programs.

module Distribution.Simple.Program.Script (

    invocationAsSystemScript,
    invocationAsShellScript,
    invocationAsBatchFile,
  ) where

import Distribution.Simple.Program.Run
         ( ProgramInvocation(..) )
import Distribution.System
         ( OS(..) )

import Data.Maybe
         ( maybeToList )

-- | Generate a system script, either POSIX shell script or Windows batch file
-- as appropriate for the given system.
--
invocationAsSystemScript :: OS -> ProgramInvocation -> String
invocationAsSystemScript Windows = invocationAsBatchFile
invocationAsSystemScript _       = invocationAsShellScript


-- | Generate a POSIX shell script that invokes a program.
--
invocationAsShellScript :: ProgramInvocation -> String
invocationAsShellScript
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = envExtra,
    progInvokeCwd   = mcwd,
    progInvokeInput = minput
  } = unlines $
          [ "#!/bin/sh" ]
       ++ [ "export " ++ var ++ "=" ++ quote val
          | (var,val) <- envExtra ]
       ++ [ "cd " ++ quote cwd | cwd <- maybeToList mcwd ]
       ++ [ (case minput of
              Nothing    -> ""
              Just input -> "echo " ++ quote input ++ " | ")
         ++ unwords (map quote $ path : args) ++ " \"$@\""]

  where
    quote :: String -> String
    quote s = "'" ++ escape s ++ "'"

    escape []        = []
    escape ('\'':cs) = "'\\''" ++ escape cs
    escape (c   :cs) = c        : escape cs


-- | Generate a Windows batch file that invokes a program.
--
invocationAsBatchFile :: ProgramInvocation -> String
invocationAsBatchFile
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = envExtra,
    progInvokeCwd   = mcwd,
    progInvokeInput = minput
  } = unlines $
          [ "@echo off" ]
       ++ [ "set " ++ var ++ "=" ++ escape val | (var,val) <- envExtra ]
       ++ [ "cd \"" ++ cwd ++ "\"" | cwd <- maybeToList mcwd ]
       ++ case minput of
            Nothing    ->
                [ path ++ concatMap (' ':) args ]

            Just input ->
                [ "(" ]
             ++ [ "echo " ++ escape line | line <- lines input ]
             ++ [ ") | "
               ++ "\"" ++ path ++ "\""
               ++ concatMap (\arg -> ' ':quote arg) args ]

  where
    quote :: String -> String
    quote s = "\"" ++ escapeQ s ++ "\""

    escapeQ []       = []
    escapeQ ('"':cs) = "\"\"\"" ++ escapeQ cs
    escapeQ (c  :cs) = c         : escapeQ cs

    escape []        = []
    escape ('|':cs) = "^|" ++ escape cs
    escape ('<':cs) = "^<" ++ escape cs
    escape ('>':cs) = "^>" ++ escape cs
    escape ('&':cs) = "^&" ++ escape cs
    escape ('(':cs) = "^(" ++ escape cs
    escape (')':cs) = "^)" ++ escape cs
    escape ('^':cs) = "^^" ++ escape cs
    escape (c  :cs) = c     : escape cs
