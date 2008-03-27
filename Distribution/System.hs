module Distribution.System (
  -- * Operating System
  OS(..),
  buildOS,

  -- * Machine Architecture
  Arch(..),
  buildArch,
  ) where

import qualified System.Info (os, arch)
import qualified Data.Char as Char (toLower, isAlphaNum)

import Distribution.Text (Text(..), display)
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

-- | How strict to be when classifying strings into the 'OS' and 'Arch' enums.
--
-- The reason we have multiple ways to do the classification is because there
-- are two situations where we need to do it.
--
-- For parsing os and arch names in .cabal files we really want everyone to be
-- referring to the same or or arch by the same name. Variety is not a virtue
-- in this case. We don't mind about case though.
--
-- For the System.Info.os\/arch different Haskell implementations use different
-- names for the same or\/arch. Also they tend to distinguish versions of an
-- os\/arch which we just don't care about.
--
-- The 'Compat' classification allows us to recognise aliases that are already
-- in common use but it allows us to distinguish them from the canonical name
-- which enables us to warn about such deprecated aliases.
--
data ClassificationStrictness = Permissive | Compat | Strict

-- ------------------------------------------------------------
-- * Operating System
-- ------------------------------------------------------------

data OS = Linux | Windows | OSX
        | FreeBSD | OpenBSD | NetBSD
        | Solaris | AIX | HPUX | IRIX
        | OtherOS String
  deriving (Eq, Ord, Show, Read)

knownOSs :: [OS]
knownOSs = [Linux, Windows, OSX
           ,FreeBSD, OpenBSD, NetBSD
           ,Solaris, AIX, HPUX, IRIX]

osAliases :: ClassificationStrictness -> OS -> [String]
osAliases Permissive Windows = ["mingw32", "cygwin32"]
osAliases Compat     Windows = ["mingw32", "win32"]
osAliases _          OSX     = ["darwin"]
osAliases Permissive FreeBSD = ["kfreebsdgnu"]
osAliases Permissive Solaris = ["solaris2"]
osAliases _          _       = []

instance Text OS where
  disp (OtherOS name) = Disp.text name
  disp other          = Disp.text (lowercase (show other))

  parse = fmap (classifyOS Compat) ident

classifyOS :: ClassificationStrictness -> String -> OS
classifyOS strictness s =
  case lookup (lowercase s) osMap of
    Just os -> os
    Nothing -> OtherOS s
  where
    osMap = [ (name, os)
            | os <- knownOSs
            , name <- display os : osAliases strictness os ]

buildOS :: OS
buildOS = classifyOS Permissive System.Info.os

-- ------------------------------------------------------------
-- * Machine Architecture
-- ------------------------------------------------------------

data Arch = I386  | X86_64 | PPC | PPC64 | Sparc
          | Arm   | Mips   | SH
          | IA64  | S390 
          | Alpha | Hppa   | Rs6000
          | M68k  | Vax
          | OtherArch String
  deriving (Eq, Ord, Show, Read)

knownArches :: [Arch]
knownArches = [I386, X86_64, PPC, PPC64, Sparc
              ,Arm, Mips, SH
              ,IA64, S390 
              ,Alpha, Hppa, Rs6000
              ,M68k, Vax]

archAliases :: ClassificationStrictness -> Arch -> [String]
archAliases Strict _     = []
archAliases Compat _     = []
archAliases _      PPC   = ["powerpc"]
archAliases _      PPC64 = ["powerpc64"]
archAliases _      Sparc = ["sparc64", "sun4"]
archAliases _      Mips  = ["mipsel", "mipseb"]
archAliases _      Arm   = ["armeb", "armel"]
archAliases _      _     = []

instance Text Arch where
  disp (OtherArch name) = Disp.text name
  disp other            = Disp.text (lowercase (show other))

  parse = fmap (classifyArch Strict) ident

classifyArch :: ClassificationStrictness -> String -> Arch
classifyArch strictness s =
  case lookup (lowercase s) archMap of
    Just arch -> arch
    Nothing   -> OtherArch s
  where
    archMap = [ (name, arch)
              | arch <- knownArches
              , name <- display arch : archAliases strictness arch ]

buildArch :: Arch
buildArch = classifyArch Permissive System.Info.arch

ident :: Parse.ReadP r String
ident = Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')
  --TODO: probably should disallow starting with a number

lowercase :: String -> String
lowercase = map Char.toLower
