-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Simon Marlow 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generate cabal_macros.h - CPP macros for package version testing
--
-- When using CPP you get
--
-- > VERSION_<package>
-- > MIN_VERSION_<package>(A,B,C)
--
-- for each /package/ in @build-depends@, which is true if the version of
-- /package/ in use is @>= A.B.C@, using the normal ordering on version
-- numbers.
--
module Distribution.Simple.Build.Macros (
    generate,
    generatePackageVersionMacros,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Types
import Distribution.Text

-- ------------------------------------------------------------
-- * Generate cabal_macros.h
-- ------------------------------------------------------------

-- Invariant: HeaderLines always has a trailing newline
type HeaderLines = String

line :: String -> HeaderLines
line str = str ++ "\n"

ifndef :: String -> HeaderLines -> HeaderLines
ifndef macro body =
    line ("#ifndef " ++ macro) ++
    body ++
    line ("#endif /* " ++ macro ++ " */")

define :: String -> Maybe [String] -> String -> HeaderLines
define macro params val =
    line ("#define " ++ macro ++ f params ++ " " ++ val)
  where
    f Nothing = ""
    f (Just xs) = "(" ++ intercalate "," xs ++ ")"

defineStr :: String -> String -> HeaderLines
defineStr macro str = define macro Nothing (show str)

ifndefDefine :: String -> Maybe [String] -> String -> HeaderLines
ifndefDefine macro params str =
    ifndef macro (define macro params str)

ifndefDefineStr :: String -> String -> HeaderLines
ifndefDefineStr macro str =
    ifndef macro (defineStr macro str)

-- | The contents of the @cabal_macros.h@ for the given configured package.
--
generate :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> String
generate pkg_descr lbi clbi =
  "/* DO NOT EDIT: This file is automatically generated by Cabal */\n\n" ++
  generatePackageVersionMacros
    (package pkg_descr : map snd (componentPackageDeps clbi)) ++
  generateToolVersionMacros (configuredPrograms . withPrograms $ lbi) ++
  generateComponentIdMacro lbi clbi

-- | Helper function that generates just the @VERSION_pkg@ and @MIN_VERSION_pkg@
-- macros for a list of package ids (usually used with the specific deps of
-- a configured package).
--
generatePackageVersionMacros :: [PackageIdentifier] -> String
generatePackageVersionMacros pkgids = concat
  [ line ("/* package " ++ display pkgid ++ " */")
  ++ generateMacros "" pkgname version
  | pkgid@(PackageIdentifier name version) <- pkgids
  , let pkgname = map fixchar (display name)
  ]

-- | Helper function that generates just the @TOOL_VERSION_pkg@ and
-- @MIN_TOOL_VERSION_pkg@ macros for a list of configured programs.
--
generateToolVersionMacros :: [ConfiguredProgram] -> String
generateToolVersionMacros progs = concat
  [ line ("/* tool " ++ progid ++ " */")
  ++ generateMacros "TOOL_" progname version
  | prog <- progs
  , isJust . programVersion $ prog
  , let progid       = programId prog ++ "-" ++ display version
        progname     = map fixchar (programId prog)
        Just version = programVersion prog
  ]

-- | Common implementation of 'generatePackageVersionMacros' and
-- 'generateToolVersionMacros'.
--
generateMacros :: String -> String -> Version -> String
generateMacros macro_prefix name version =
  concat
  [ifndefDefineStr (macro_prefix ++ "VERSION_" ++ name) (display version)
  ,ifndefDefine ("MIN_" ++ macro_prefix ++ "VERSION_" ++ name)
                (Just ["major1","major2","minor"])
    $ concat [
       "(\\\n"
      ,"  (major1) <  ",major1," || \\\n"
      ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
      ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
    ]
  ,"\n"]
  where
    (major1:major2:minor:_) = map show (unVersion version ++ repeat 0)

-- | Generate the @CURRENT_COMPONENT_ID@ definition for the component ID
--   of the current package.
generateComponentIdMacro :: LocalBuildInfo -> ComponentLocalBuildInfo -> String
generateComponentIdMacro _lbi clbi =
  concat $
      [case clbi of
        LibComponentLocalBuildInfo{} ->
          ifndefDefineStr "CURRENT_PACKAGE_KEY" (componentCompatPackageKey clbi)
        _ -> ""
      ,ifndefDefineStr "CURRENT_COMPONENT_ID" (display (componentComponentId clbi))
      ]

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c
