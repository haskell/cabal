{- FOURMOLU_DISABLE -}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Simple.Build.PackageInfoModule.Z (render, Z(..)) where
import Distribution.ZinzaPrelude
data Z
    = Z {zPackageName :: PackageName,
         zVersionDigits :: String,
         zLicense :: String,
         zCopyright :: String,
         zMaintainer :: String,
         zAuthor :: String,
         zStability :: String,
         zHomepage :: String,
         zPkgUrl :: String,
         zBugReports :: String,
         zSynopsis :: String,
         zDescription :: String,
         zCategory :: String,
         zSupportsNoRebindableSyntax :: Bool,
         zManglePkgName :: (PackageName -> String),
         zShow :: (String -> String)}
    deriving Generic
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
  tell (zManglePkgName z_root (zPackageName z_root))
  tell "\n"
  tell "  ( name\n"
  tell "  , version\n"
  tell "  , license\n"
  tell "  , copyright\n"
  tell "  , maintainer\n"
  tell "  , author\n"
  tell "  , stability\n"
  tell "  , homepage\n"
  tell "  , pkgUrl\n"
  tell "  , bugReports\n"
  tell "  , synopsis\n"
  tell "  , description\n"
  tell "  , category\n"
  tell "  ) where\n"
  tell "\n"
  tell "import Data.Version (Version(..))\n"
  tell "import Prelude\n"
  tell "\n"
  tell "name :: String\n"
  tell "name = "
  tell (zShow z_root (zManglePkgName z_root (zPackageName z_root)))
  tell "\n"
  tell "\n"
  tell "version :: Version\n"
  tell "version = Version "
  tell (zVersionDigits z_root)
  tell " []\n"
  tell "\n"
  tell "license :: String\n"
  tell "license = "
  tell (zLicense z_root)
  tell "\n"
  tell "\n"
  tell "copyright :: String\n"
  tell "copyright = "
  tell (zCopyright z_root)
  tell "\n"
  tell "\n"
  tell "maintainer :: String\n"
  tell "maintainer = "
  tell (zMaintainer z_root)
  tell "\n"
  tell "\n"
  tell "author :: String\n"
  tell "author = "
  tell (zAuthor z_root)
  tell "\n"
  tell "\n"
  tell "stability :: String\n"
  tell "stability = "
  tell (zStability z_root)
  tell "\n"
  tell "\n"
  tell "homepage :: String\n"
  tell "homepage = "
  tell (zHomepage z_root)
  tell "\n"
  tell "\n"
  tell "pkgUrl :: String\n"
  tell "pkgUrl = "
  tell (zPkgUrl z_root)
  tell "\n"
  tell "\n"
  tell "bugReports :: String\n"
  tell "bugReports = "
  tell (zBugReports z_root)
  tell "\n"
  tell "\n"
  tell "synopsis :: String\n"
  tell "synopsis = "
  tell (zSynopsis z_root)
  tell "\n"
  tell "\n"
  tell "description :: String\n"
  tell "description = "
  tell (zDescription z_root)
  tell "\n"
  tell "\n"
  tell "category :: String\n"
  tell "category = "
  tell (zCategory z_root)
  tell "\n"
