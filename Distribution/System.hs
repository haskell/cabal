module Distribution.System where

import qualified System.Info

data OS = Linux | Windows Windows | OSX | Solaris | Other String
data Windows = MingW

os :: OS
os = case System.Info.os of
  "linux"    -> Linux
  "mingw32"  -> Windows MingW
  "darwin"   -> OSX
  "solaris2" -> Solaris
  other      -> Other other
