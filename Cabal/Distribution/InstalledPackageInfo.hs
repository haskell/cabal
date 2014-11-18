{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.InstalledPackageInfo
-- Copyright   :  (c) The University of Glasgow 2004
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This is the information about an /installed/ package that
-- is communicated to the @ghc-pkg@ program in order to register
-- a package.  @ghc-pkg@ now consumes this package format (as of version
-- 6.4). This is specific to GHC at the moment.
--
-- The @.cabal@ file format is for describing a package that is not yet
-- installed. It has a lot of flexibility, like conditionals and dependency
-- ranges. As such, that format is not at all suitable for describing a package
-- that has already been built and installed. By the time we get to that stage,
-- we have resolved all conditionals and resolved dependency version
-- constraints to exact versions of dependent packages. So, this module defines
-- the 'InstalledPackageInfo' data structure that contains all the info we keep
-- about an installed package. There is a parser and pretty printer. The
-- textual format is rather simpler than the @.cabal@ format: there are no
-- sections, for example.

-- This module is meant to be local-only to Distribution...

module Distribution.InstalledPackageInfo (
        InstalledPackageInfo_(..), InstalledPackageInfo,
        OriginalModule(..), ExposedModule(..),
        ParseResult(..), PError(..), PWarning,
        emptyInstalledPackageInfo,
        parseInstalledPackageInfo,
        showInstalledPackageInfo,
        showInstalledPackageInfoField,
        showSimpleInstalledPackageInfoField,
        fieldsInstalledPackageInfo,
  ) where

import Distribution.ParseUtils
         ( FieldDescr(..), ParseResult(..), PError(..), PWarning
         , simpleField, listField, parseLicenseQ
         , showFields, showSingleNamedField, showSimpleSingleNamedField
         , parseFieldsFlat
         , parseFilePathQ, parseTokenQ, parseModuleNameQ, parsePackageNameQ
         , showFilePath, showToken, boolField, parseOptVersion
         , parseFreeText, showFreeText, parseOptCommaList )
import Distribution.License     ( License(..) )
import Distribution.Package
         ( PackageName(..), PackageIdentifier(..)
         , PackageId, InstalledPackageId(..)
         , packageName, packageVersion, PackageKey(..) )
import qualified Distribution.Package as Package
import Distribution.ModuleName
         ( ModuleName )
import Distribution.Version
         ( Version(..) )
import Distribution.Text
         ( Text(disp, parse) )
import Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse

import Data.Binary (Binary)
import GHC.Generics (Generic)

-- -----------------------------------------------------------------------------
-- The InstalledPackageInfo type


data InstalledPackageInfo_ m
   = InstalledPackageInfo {
        -- these parts are exactly the same as PackageDescription
        installedPackageId :: InstalledPackageId,
        sourcePackageId    :: PackageId,
        packageKey         :: PackageKey,
        license           :: License,
        copyright         :: String,
        maintainer        :: String,
        author            :: String,
        stability         :: String,
        homepage          :: String,
        pkgUrl            :: String,
        synopsis          :: String,
        description       :: String,
        category          :: String,
        -- these parts are required by an installed package only:
        exposed           :: Bool,
        exposedModules    :: [ExposedModule],
        hiddenModules     :: [m],
        trusted           :: Bool,
        importDirs        :: [FilePath],
        libraryDirs       :: [FilePath],
        dataDir           :: FilePath,
        hsLibraries       :: [String],
        extraLibraries    :: [String],
        extraGHCiLibraries:: [String],    -- overrides extraLibraries for GHCi
        includeDirs       :: [FilePath],
        includes          :: [String],
        depends           :: [InstalledPackageId],
        ccOptions         :: [String],
        ldOptions         :: [String],
        frameworkDirs     :: [FilePath],
        frameworks        :: [String],
        haddockInterfaces :: [FilePath],
        haddockHTMLs      :: [FilePath]
    }
    deriving (Generic, Read, Show)

instance Binary m => Binary (InstalledPackageInfo_ m)

instance Package.Package          (InstalledPackageInfo_ str) where
   packageId = sourcePackageId

instance Package.PackageInstalled (InstalledPackageInfo_ str) where
   installedPackageId = installedPackageId
   installedDepends = depends

type InstalledPackageInfo = InstalledPackageInfo_ ModuleName

emptyInstalledPackageInfo :: InstalledPackageInfo_ m
emptyInstalledPackageInfo
   = InstalledPackageInfo {
        installedPackageId = InstalledPackageId "",
        sourcePackageId    = PackageIdentifier (PackageName "") noVersion,
        packageKey         = OldPackageKey (PackageIdentifier
                                               (PackageName "") noVersion),
        license           = UnspecifiedLicense,
        copyright         = "",
        maintainer        = "",
        author            = "",
        stability         = "",
        homepage          = "",
        pkgUrl            = "",
        synopsis          = "",
        description       = "",
        category          = "",
        exposed           = False,
        exposedModules    = [],
        hiddenModules     = [],
        trusted           = False,
        importDirs        = [],
        libraryDirs       = [],
        dataDir           = "",
        hsLibraries       = [],
        extraLibraries    = [],
        extraGHCiLibraries= [],
        includeDirs       = [],
        includes          = [],
        depends           = [],
        ccOptions         = [],
        ldOptions         = [],
        frameworkDirs     = [],
        frameworks        = [],
        haddockInterfaces = [],
        haddockHTMLs      = []
    }

noVersion :: Version
noVersion = Version [] []

-- -----------------------------------------------------------------------------
-- Exposed modules

data OriginalModule
   = OriginalModule {
       originalPackageId :: InstalledPackageId,
       originalModuleName :: ModuleName
     }
  deriving (Generic, Eq, Read, Show)

data ExposedModule
   = ExposedModule {
       exposedName      :: ModuleName,
       exposedReexport  :: Maybe OriginalModule,
       exposedSignature :: Maybe OriginalModule -- This field is unused for now.
     }
  deriving (Generic, Read, Show)

instance Text OriginalModule where
    disp (OriginalModule ipi m) =
        disp ipi <> Disp.char ':' <> disp m
    parse = do
        ipi <- parse
        _ <- Parse.char ':'
        m <- parse
        return (OriginalModule ipi m)

instance Text ExposedModule where
    disp (ExposedModule m reexport signature) =
        Disp.sep [ disp m
                 , case reexport of
                    Just m' -> Disp.sep [Disp.text "from", disp m']
                    Nothing -> Disp.empty
                 , case signature of
                    Just m' -> Disp.sep [Disp.text "is", disp m']
                    Nothing -> Disp.empty
                 ]
    parse = do
        m <- parseModuleNameQ
        Parse.skipSpaces
        reexport <- Parse.option Nothing $ do
            _ <- Parse.string "from"
            Parse.skipSpaces
            fmap Just parse
        Parse.skipSpaces
        signature <- Parse.option Nothing $ do
            _ <- Parse.string "is"
            Parse.skipSpaces
            fmap Just parse
        return (ExposedModule m reexport signature)


instance Binary OriginalModule

instance Binary ExposedModule

-- To maintain backwards-compatibility, we accept both comma/non-comma
-- separated variants of this field.  You SHOULD use the comma syntax if you
-- use any new functions, although actually it's unambiguous due to a quirk
-- of the fact that modules must start with capital letters.

showExposedModules :: [ExposedModule] -> Disp.Doc
showExposedModules xs
    | all isExposedModule xs = fsep (map disp xs)
    | otherwise = fsep (Disp.punctuate comma (map disp xs))
    where isExposedModule (ExposedModule _ Nothing Nothing) = True
          isExposedModule _ = False

parseExposedModules :: Parse.ReadP r [ExposedModule]
parseExposedModules = parseOptCommaList parse

-- -----------------------------------------------------------------------------
-- Parsing

parseInstalledPackageInfo :: String -> ParseResult InstalledPackageInfo
parseInstalledPackageInfo =
    parseFieldsFlat (fieldsInstalledPackageInfo ++ deprecatedFieldDescrs)
    emptyInstalledPackageInfo

-- -----------------------------------------------------------------------------
-- Pretty-printing

showInstalledPackageInfo :: InstalledPackageInfo -> String
showInstalledPackageInfo = showFields fieldsInstalledPackageInfo

showInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showInstalledPackageInfoField = showSingleNamedField fieldsInstalledPackageInfo

showSimpleInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showSimpleInstalledPackageInfoField = showSimpleSingleNamedField fieldsInstalledPackageInfo

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldsInstalledPackageInfo :: [FieldDescr InstalledPackageInfo]
fieldsInstalledPackageInfo = basicFieldDescrs ++ installedFieldDescrs

basicFieldDescrs :: [FieldDescr InstalledPackageInfo]
basicFieldDescrs =
 [ simpleField "name"
                           disp                   parsePackageNameQ
                           packageName            (\name pkg -> pkg{sourcePackageId=(sourcePackageId pkg){pkgName=name}})
 , simpleField "version"
                           disp                   parseOptVersion
                           packageVersion         (\ver pkg -> pkg{sourcePackageId=(sourcePackageId pkg){pkgVersion=ver}})
 , simpleField "id"
                           disp                   parse
                           installedPackageId     (\ipid pkg -> pkg{installedPackageId=ipid})
 , simpleField "key"
                           disp                   parse
                           packageKey             (\ipid pkg -> pkg{packageKey=ipid})
 , simpleField "license"
                           disp                   parseLicenseQ
                           license                (\l pkg -> pkg{license=l})
 , simpleField "copyright"
                           showFreeText           parseFreeText
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           showFreeText           parseFreeText
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
                           showFreeText           parseFreeText
                           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
                           showFreeText           parseFreeText
                           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
                           showFreeText           parseFreeText
                           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "synopsis"
                           showFreeText           parseFreeText
                           synopsis               (\val pkg -> pkg{synopsis=val})
 , simpleField "description"
                           showFreeText           parseFreeText
                           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
                           showFreeText           parseFreeText
                           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
                           showFreeText           parseFreeText
                           author                 (\val pkg -> pkg{author=val})
 ]

installedFieldDescrs :: [FieldDescr InstalledPackageInfo]
installedFieldDescrs = [
   boolField "exposed"
        exposed            (\val pkg -> pkg{exposed=val})
 , simpleField "exposed-modules"
        showExposedModules parseExposedModules
        exposedModules     (\xs    pkg -> pkg{exposedModules=xs})
 , listField   "hidden-modules"
        disp               parseModuleNameQ
        hiddenModules      (\xs    pkg -> pkg{hiddenModules=xs})
 , boolField   "trusted"
        trusted            (\val pkg -> pkg{trusted=val})
 , listField   "import-dirs"
        showFilePath       parseFilePathQ
        importDirs         (\xs pkg -> pkg{importDirs=xs})
 , listField   "library-dirs"
        showFilePath       parseFilePathQ
        libraryDirs        (\xs pkg -> pkg{libraryDirs=xs})
 , simpleField "data-dir"
        showFilePath       (parseFilePathQ Parse.<++ return "")
        dataDir            (\val pkg -> pkg{dataDir=val})
 , listField   "hs-libraries"
        showFilePath       parseTokenQ
        hsLibraries        (\xs pkg -> pkg{hsLibraries=xs})
 , listField   "extra-libraries"
        showToken          parseTokenQ
        extraLibraries     (\xs pkg -> pkg{extraLibraries=xs})
 , listField   "extra-ghci-libraries"
        showToken          parseTokenQ
        extraGHCiLibraries (\xs pkg -> pkg{extraGHCiLibraries=xs})
 , listField   "include-dirs"
        showFilePath       parseFilePathQ
        includeDirs        (\xs pkg -> pkg{includeDirs=xs})
 , listField   "includes"
        showFilePath       parseFilePathQ
        includes           (\xs pkg -> pkg{includes=xs})
 , listField   "depends"
        disp               parse
        depends            (\xs pkg -> pkg{depends=xs})
 , listField   "cc-options"
        showToken          parseTokenQ
        ccOptions          (\path  pkg -> pkg{ccOptions=path})
 , listField   "ld-options"
        showToken          parseTokenQ
        ldOptions          (\path  pkg -> pkg{ldOptions=path})
 , listField   "framework-dirs"
        showFilePath       parseFilePathQ
        frameworkDirs      (\xs pkg -> pkg{frameworkDirs=xs})
 , listField   "frameworks"
        showToken          parseTokenQ
        frameworks         (\xs pkg -> pkg{frameworks=xs})
 , listField   "haddock-interfaces"
        showFilePath       parseFilePathQ
        haddockInterfaces  (\xs pkg -> pkg{haddockInterfaces=xs})
 , listField   "haddock-html"
        showFilePath       parseFilePathQ
        haddockHTMLs       (\xs pkg -> pkg{haddockHTMLs=xs})
 ]

deprecatedFieldDescrs :: [FieldDescr InstalledPackageInfo]
deprecatedFieldDescrs = [
   listField   "hugs-options"
        showToken          parseTokenQ
        (const [])        (const id)
  ]
