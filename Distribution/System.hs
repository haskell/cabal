module Distribution.System (
  -- * Operating System
  OS(..),
  Windows(..),
  showOS,
  readOS,
  os,
  ) where

import qualified System.Info (os)
import qualified Data.Char as Char (toLower)

-- ------------------------------------------------------------
-- * Operating System
-- ------------------------------------------------------------

data OS = Linux | Windows Windows | OSX
        | FreeBSD | OpenBSD | NetBSD
        | Solaris | AIX | HPUX | IRIX
        | Other String
  deriving (Eq, Ord, Show, Read)

--TODO: eliminate Windows data type
data Windows = MingW
  deriving (Eq, Ord, Show, Read)

knownOSs :: [OS]
knownOSs = [Linux, Windows MingW, OSX
           ,FreeBSD, OpenBSD, NetBSD
           ,Solaris, AIX, HPUX, IRIX]

osAliases :: OS -> [String]
osAliases (Windows _) = ["mingw32", "cygwin32"]
osAliases OSX         = ["darwin"]
osAliases FreeBSD     = ["kfreebsdgnu"]
osAliases Solaris     = ["solaris2"]
osAliases _           = []

showOS :: OS -> String
showOS (Other name) = lowercase name
showOS other        = lowercase (show other)

readOS :: String -> OS
readOS s = case lookup (lowercase s)
                [ (name, os')
                | os' <- knownOSs
                , name <- showOS os' : osAliases os' ] of
  Just os' -> os'
  Nothing  -> Other s

--TODO: rename to buildOS and rename os' above to just os
os :: OS
os = readOS System.Info.os

lowercase :: String -> String
lowercase = map Char.toLower
