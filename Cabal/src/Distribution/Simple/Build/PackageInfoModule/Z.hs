{-# LANGUAGE DeriveGeneric #-}

module Distribution.Simple.Build.PackageInfoModule.Z (render, Z (..)) where

import Distribution.ZinzaPrelude

data Z = Z
  { zPackageName :: String
  , zVersionDigits :: String
  , zSynopsis :: String
  , zCopyright :: String
  , zHomepage :: String
  , zSupportsNoRebindableSyntax :: Bool
  }
  deriving (Generic)

render :: Z -> String
render z_root = execWriter $ do
  if (zSupportsNoRebindableSyntax z_root)
    then do
      tell "{-# LANGUAGE NoRebindableSyntax #-}\n"
      return ()
    else do
      return ()
  tell "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}\n"
  tell "{-# OPTIONS_GHC -w #-}\n"
  tell "module PackageInfo_"
  tell (zPackageName z_root)
  tell " (\n"
  tell "    name,\n"
  tell "    version,\n"
  tell "    synopsis,\n"
  tell "    copyright,\n"
  tell "    homepage,\n"
  tell "  ) where\n"
  tell "\n"
  tell "import Data.Version (Version(..))\n"
  tell "import Prelude\n"
  tell "\n"
  tell "name :: String\n"
  tell "name = "
  tell (show $ zPackageName z_root)
  tell "\n"
  tell "version :: Version\n"
  tell "version = Version "
  tell (zVersionDigits z_root)
  tell " []\n"
  tell "\n"
  tell "synopsis :: String\n"
  tell "synopsis = "
  tell (show $ zSynopsis z_root)
  tell "\n"
  tell "copyright :: String\n"
  tell "copyright = "
  tell (show $ zCopyright z_root)
  tell "\n"
  tell "homepage :: String\n"
  tell "homepage = "
  tell (show $ zHomepage z_root)
  tell "\n"
