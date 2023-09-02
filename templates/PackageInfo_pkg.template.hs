{% if supportsNoRebindableSyntax %}
{-# LANGUAGE NoRebindableSyntax #-}
{% endif %}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_{{ manglePkgName packageName }}
  ( name
  , version
  , license
  , copyright
  , maintainer
  , author
  , stability
  , homepage
  , pkgUrl
  , bugReports
  , synopsis
  , description
  , category
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = {{ show (manglePkgName packageName) }}

version :: Version
version = Version {{ versionDigits }} []

license :: String
license = {{ license }}

copyright :: String
copyright = {{ copyright }}

maintainer :: String
maintainer = {{ maintainer }}

author :: String
author = {{ author }}

stability :: String
stability = {{ stability }}

homepage :: String
homepage = {{ homepage }}

pkgUrl :: String
pkgUrl = {{ pkgUrl }}

bugReports :: String
bugReports = {{ bugReports }}

synopsis :: String
synopsis = {{ synopsis }}

description :: String
description = {{ description }}

category :: String
category = {{ category }}
