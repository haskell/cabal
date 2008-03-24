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

osAliases :: OS -> [String]
osAliases Windows     = ["mingw32", "cygwin32"]
osAliases OSX         = ["darwin"]
osAliases FreeBSD     = ["kfreebsdgnu"]
osAliases Solaris     = ["solaris2"]
osAliases _           = []

instance Text OS where
  disp (OtherOS name) = Disp.text name
  disp other          = Disp.text (lowercase (show other))

  parse = fmap classifyOS (Parse.munch1 Char.isAlphaNum)
  --TODO: probably should disallow starting with a number

classifyOS :: String -> OS
classifyOS s =
  case lookup (lowercase s) osMap of
    Just os -> os
    Nothing -> OtherOS s
  where
    osMap = [ (name, os)
            | os <- knownOSs
            , name <- display os : osAliases os ]

buildOS :: OS
buildOS = classifyOS System.Info.os

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

archAliases :: Arch -> [String]
archAliases PPC   = ["powerpc"]
archAliases PPC64 = ["powerpc64"]
archAliases Sparc = ["sparc64"]
archAliases Mips  = ["mipsel", "mipseb"]
archAliases Arm   = ["armeb", "armel"]
archAliases _     = []

instance Text Arch where
  disp (OtherArch name) = Disp.text name
  disp other            = Disp.text (lowercase (show other))

  parse = fmap classifyArch (Parse.munch1 Char.isAlphaNum)
  --TODO: probably should disallow starting with a number

classifyArch :: String -> Arch
classifyArch s =
  case lookup (lowercase s) archMap of
    Just arch -> arch
    Nothing   -> OtherArch s
  where
    archMap = [ (name, arch)
              | arch <- knownArches
              , name <- display arch : archAliases arch ]

buildArch :: Arch
buildArch = classifyArch System.Info.arch

lowercase :: String -> String
lowercase = map Char.toLower
