{% if supportsNoRebindableSyntax %}
{-# LANGUAGE NoRebindableSyntax #-}
{% endif %}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_{{ manglePkgName packageName }}
  ( name
  , version
  , synopsis
  , copyright
  , license
  , homepage
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = {{ show (manglePkgName packageName) }}

version :: Version
version = Version {{ versionDigits }} []

synopsis :: String
synopsis = {{ synopsis }}

copyright :: String
copyright = {{ copyright }}

license :: String
license = {{ license }}

homepage :: String
homepage = {{ homepage }}

