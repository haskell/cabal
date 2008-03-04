module Distribution.System (
  -- * Operating System
  OS(..),
  showOS,
  readOS,
  buildOS,

  -- * Machine Architecture
  Arch(..),
  showArch,
  readArch,
  buildArch,
  ) where

import qualified System.Info (os, arch)
import qualified Data.Char as Char (toLower)

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

showOS :: OS -> String
showOS (OtherOS name) = name
showOS other          = lowercase (show other)

readOS :: String -> OS
readOS s =
  case lookup (lowercase s) osMap of
    Just os -> os
    Nothing -> OtherOS (lowercase s)
  where
    osMap = [ (name, os)
            | os <- knownOSs
            , name <- showOS os : osAliases os ]

buildOS :: OS
buildOS = readOS System.Info.os

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

showArch :: Arch -> String
showArch (OtherArch name) = name
showArch other            = lowercase (show other)

readArch :: String -> Arch
readArch s =
  case lookup (lowercase s) archMap of
    Just arch -> arch
    Nothing   -> OtherArch (lowercase s)
  where
    archMap = [ (name, arch)
              | arch <- knownArches
              , name <- showArch arch : archAliases arch ]

buildArch :: Arch
buildArch = readArch System.Info.arch

lowercase :: String -> String
lowercase = map Char.toLower
