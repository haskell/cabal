{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
        InstalledPackageInfo(..),
        installedPackageId,
        installedComponentId,
        installedOpenUnitId,
        sourceComponentName,
        requiredSignatures,
        ExposedModule(..),
        AbiDependency(..),
        ParseResult(..), PError(..), PWarning,
        emptyInstalledPackageInfo,
        parseInstalledPackageInfo,
        showInstalledPackageInfo,
        showFullInstalledPackageInfo,
        showInstalledPackageInfoField,
        showSimpleInstalledPackageInfoField,
        fieldsInstalledPackageInfo,
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Set                               (Set)
import Distribution.Backpack
import Distribution.CabalSpecVersion          (cabalSpecLatest)
import Distribution.Compat.Lens               (Lens', (&), (.~))
import Distribution.Compat.Newtype
import Distribution.FieldGrammar
import Distribution.License
import Distribution.ModuleName
import Distribution.Package                   hiding (installedPackageId, installedUnitId)
import Distribution.Parsec.Class
import Distribution.Parsec.Newtypes
import Distribution.ParseUtils
import Distribution.Pretty
import Distribution.Text
import Distribution.Types.ComponentName
import Distribution.Types.MungedPackageName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Generic             (toUTF8BS)
import Distribution.Version

import qualified Data.Char                       as Char
import qualified Data.Map                        as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.ReadP       as Parse
import qualified Distribution.Parsec.Common      as P
import qualified Distribution.Parsec.Parser      as P
import qualified Distribution.Parsec.ParseResult as P
import qualified Text.Parsec.Error               as Parsec
import qualified Text.Parsec.Pos                 as Parsec
import qualified Text.PrettyPrint                as Disp

import Distribution.Types.InstalledPackageInfo

import qualified Distribution.Types.InstalledPackageInfo.Lens as L
import qualified Distribution.Types.PackageId.Lens            as L

installedComponentId :: InstalledPackageInfo -> ComponentId
installedComponentId ipi =
    case unComponentId (installedComponentId_ ipi) of
        "" -> mkComponentId (unUnitId (installedUnitId ipi))
        _  -> installedComponentId_ ipi

-- | Get the indefinite unit identity representing this package.
-- This IS NOT guaranteed to give you a substitution; for
-- instantiated packages you will get @DefiniteUnitId (installedUnitId ipi)@.
-- For indefinite libraries, however, you will correctly get
-- an @OpenUnitId@ with the appropriate 'OpenModuleSubst'.
installedOpenUnitId :: InstalledPackageInfo -> OpenUnitId
installedOpenUnitId ipi
    = mkOpenUnitId (installedUnitId ipi) (installedComponentId ipi) (Map.fromList (instantiatedWith ipi))

-- | Returns the set of module names which need to be filled for
-- an indefinite package, or the empty set if the package is definite.
requiredSignatures :: InstalledPackageInfo -> Set ModuleName
requiredSignatures ipi = openModuleSubstFreeHoles (Map.fromList (instantiatedWith ipi))

{-# DEPRECATED installedPackageId "Use installedUnitId instead" #-}
-- | Backwards compatibility with Cabal pre-1.24.
--
-- This type synonym is slightly awful because in cabal-install
-- we define an 'InstalledPackageId' but it's a ComponentId,
-- not a UnitId!
installedPackageId :: InstalledPackageInfo -> UnitId
installedPackageId = installedUnitId



emptyInstalledPackageInfo :: InstalledPackageInfo
emptyInstalledPackageInfo
   = InstalledPackageInfo {
        sourcePackageId   = PackageIdentifier (mkPackageName "") nullVersion,
        sourceLibName     = Nothing,
        installedComponentId_ = mkComponentId "",
        installedUnitId   = mkUnitId "",
        instantiatedWith  = [],
        compatPackageKey  = "",
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
        abiHash           = mkAbiHash "",
        indefinite        = False,
        exposed           = False,
        exposedModules    = [],
        hiddenModules     = [],
        trusted           = False,
        importDirs        = [],
        libraryDirs       = [],
        libraryDynDirs    = [],
        dataDir           = "",
        hsLibraries       = [],
        extraLibraries    = [],
        extraGHCiLibraries= [],
        includeDirs       = [],
        includes          = [],
        depends           = [],
        abiDepends        = [],
        ccOptions         = [],
        ldOptions         = [],
        frameworkDirs     = [],
        frameworks        = [],
        haddockInterfaces = [],
        haddockHTMLs      = [],
        pkgRoot           = Nothing
    }


-- To maintain backwards-compatibility, we accept both comma/non-comma
-- separated variants of this field.  You SHOULD use the comma syntax if you
-- use any new functions, although actually it's unambiguous due to a quirk
-- of the fact that modules must start with capital letters.

showExposedModules :: [ExposedModule] -> Disp.Doc
showExposedModules xs
    | all isExposedModule xs = Disp.fsep (map disp xs)
    | otherwise = Disp.fsep (Disp.punctuate Disp.comma (map disp xs))
    where isExposedModule (ExposedModule _ Nothing) = True
          isExposedModule _ = False

parseExposedModules :: Parse.ReadP r [ExposedModule]
parseExposedModules = parseOptCommaList parse

dispMaybe :: Text a => Maybe a -> Disp.Doc
dispMaybe Nothing = Disp.empty
dispMaybe (Just x) = disp x

parseMaybe :: Text a => Parse.ReadP r (Maybe a)
parseMaybe = fmap Just parse Parse.<++ return Nothing


-- -----------------------------------------------------------------------------
-- Munging

sourceComponentName :: InstalledPackageInfo -> ComponentName
sourceComponentName ipi =
    case sourceLibName ipi of
        Nothing -> CLibName
        Just qn -> CSubLibName qn

-- | Returns @Just@ if the @name@ field of the IPI record would not contain
-- the package name verbatim.  This helps us avoid writing @package-name@
-- when it's redundant.
maybePackageName :: InstalledPackageInfo -> Maybe PackageName
maybePackageName ipi =
    case sourceLibName ipi of
        Nothing -> Nothing
        Just _ -> Just (packageName ipi)

-- | Setter for the @package-name@ field.  It should be acceptable for this
-- to be a no-op.
setMaybePackageName :: Maybe PackageName -> InstalledPackageInfo -> InstalledPackageInfo
setMaybePackageName Nothing ipi = ipi
setMaybePackageName (Just pn) ipi = ipi {
        sourcePackageId=(sourcePackageId ipi){pkgName=pn}
    }

setMungedPackageName :: MungedPackageName -> InstalledPackageInfo -> InstalledPackageInfo
setMungedPackageName mpn ipi =
    let (pn, mb_uqn) = decodeCompatPackageName mpn
    in ipi {
            sourcePackageId = (sourcePackageId ipi) {pkgName=pn},
            sourceLibName   = mb_uqn
        }

-- -----------------------------------------------------------------------------
-- Parsing

parseInstalledPackageInfo :: String -> ParseResult InstalledPackageInfo
parseInstalledPackageInfo s = case P.readFields (toUTF8BS s) of
    Left err -> ParseFailed (NoParse (show err) $ Parsec.sourceLine $ Parsec.errorPos err)
    Right fs -> case partitionFields fs of
        (fs', _) -> case P.runParseResult $ parseFieldGrammar cabalSpecLatest fs' ipiFieldGrammar of
            (ws, Right x)        -> ParseOk ws' x where
                ws' = map (PWarning . P.showPWarning "") ws
            (_,  Left (_, errs)) -> ParseFailed (NoParse errs' 0) where
                errs' = intercalate "; " $ map (\(P.PError _ msg) -> msg) errs

-- -----------------------------------------------------------------------------
-- FieldGrammar
--

-- | 'FieldGrammar' for 'InstalledPackageInfo'.
--
-- @since 2.2
ipiFieldGrammar
    :: (FieldGrammar g, Applicative (g InstalledPackageInfo), Applicative (g Basic))
    => g InstalledPackageInfo InstalledPackageInfo
ipiFieldGrammar = mkInstalledPackageInfo
    -- Deprecated fields
    <$> monoidalFieldAla    "hugs-options"         (alaList' FSep Token)         unitedList
        ^^^ deprecatedField' "hugs isn't supported anymore"
    -- Very basic fields: name, version, package-name and lib-name
    <*> blurFieldGrammar basic basicFieldGrammar
    -- Basic fields
    <*> optionalFieldDef    "id"                                                 L.installedUnitId (mkUnitId "")
    <*> optionalFieldDefAla "instantiated-with"    InstWith                      L.instantiatedWith []
    <*> optionalFieldDefAla "key"                  CompatPackageKey              L.compatPackageKey ""
    <*> optionalFieldDef    "license"                                            L.license UnspecifiedLicense
    <*> optionalFieldDefAla "copyright"            FreeText                      L.copyright ""
    <*> optionalFieldDefAla "maintainer"           FreeText                      L.maintainer ""
    <*> optionalFieldDefAla "author"               FreeText                      L.author ""
    <*> optionalFieldDefAla "stability"            FreeText                      L.stability ""
    <*> optionalFieldDefAla "homepage"             FreeText                      L.homepage ""
    <*> optionalFieldDefAla "package-url"          FreeText                      L.pkgUrl ""
    <*> optionalFieldDefAla "synopsis"             FreeText                      L.synopsis ""
    <*> optionalFieldDefAla "description"          FreeText                      L.description ""
    <*> optionalFieldDefAla "category"             FreeText                      L.category ""
    -- Installed fields
    <*> optionalFieldDef    "abi"                                                L.abiHash (mkAbiHash "")
    <*> booleanFieldDef     "indefinite"                                         L.indefinite False
    <*> booleanFieldDef     "exposed"                                            L.exposed False
    <*> monoidalFieldAla    "exposed-modules"      ExposedModules                L.exposedModules
    <*> monoidalFieldAla    "hidden-modules"       (alaList' FSep MQuoted)       L.hiddenModules
    <*> booleanFieldDef     "trusted"                                            L.trusted False
    <*> monoidalFieldAla    "import-dirs"          (alaList' FSep FilePathNT)    L.importDirs
    <*> monoidalFieldAla    "library-dirs"         (alaList' FSep FilePathNT)    L.libraryDirs
    <*> monoidalFieldAla    "dynamic-library-dirs" (alaList' FSep FilePathNT)    L.libraryDynDirs
    <*> optionalFieldDefAla "data-dir"             FilePathNT                    L.dataDir ""
    <*> monoidalFieldAla    "hs-libraries"         (alaList' FSep Token)         L.hsLibraries
    <*> monoidalFieldAla    "extra-libraries"      (alaList' FSep Token)         L.extraLibraries
    <*> monoidalFieldAla    "extra-ghci-libraries" (alaList' FSep Token)         L.extraGHCiLibraries
    <*> monoidalFieldAla    "include-dirs"         (alaList' FSep FilePathNT)    L.includeDirs
    <*> monoidalFieldAla    "includes"             (alaList' FSep FilePathNT)    L.includes
    <*> monoidalFieldAla    "depends"              (alaList FSep)                L.depends
    <*> monoidalFieldAla    "abi-depends"          (alaList FSep)                L.abiDepends
    <*> monoidalFieldAla    "cc-options"           (alaList' FSep Token)         L.ccOptions
    <*> monoidalFieldAla    "ld-options"           (alaList' FSep Token)         L.ldOptions
    <*> monoidalFieldAla    "framework-dirs"       (alaList' FSep FilePathNT)    L.frameworkDirs
    <*> monoidalFieldAla    "frameworks"           (alaList' FSep Token)         L.frameworks
    <*> monoidalFieldAla    "haddock-interfaces"   (alaList' FSep FilePathNT)    L.haddockInterfaces
    <*> monoidalFieldAla    "haddock-html"         (alaList' FSep FilePathNT)    L.haddockHTMLs
    <*> optionalFieldAla    "pkgroot"              FilePathNT                    L.pkgRoot
  where
    mkInstalledPackageInfo _ Basic {..} = InstalledPackageInfo
        -- _basicPkgName is not used
        -- setMaybePackageId says it can be no-op.
        (PackageIdentifier pn _basicVersion)
        (mb_uqn <|> _basicLibName)
        (mkComponentId "") -- installedComponentId_, not in use
      where
        (pn, mb_uqn) = decodeCompatPackageName _basicName

-- (forall b. [b]) ~ ()
unitedList :: Lens' a [b]
unitedList f s = s <$ f []

-- -----------------------------------------------------------------------------
-- Auxiliary types

newtype ExposedModules = ExposedModules { getExposedModules :: [ExposedModule] }

instance Newtype ExposedModules [ExposedModule] where
    pack   = ExposedModules
    unpack = getExposedModules

instance Parsec ExposedModules where
    parsec = ExposedModules <$> parsecOptCommaList parsec

instance Pretty ExposedModules where
    pretty = showExposedModules . getExposedModules


newtype CompatPackageKey = CompatPackageKey { getCompatPackageKey :: String }

instance Newtype CompatPackageKey String where
    pack = CompatPackageKey
    unpack = getCompatPackageKey

instance Pretty CompatPackageKey where
    pretty = Disp.text . getCompatPackageKey

instance Parsec CompatPackageKey where
    parsec = CompatPackageKey <$> P.munch1 uid_char where
        uid_char c = Char.isAlphaNum c || c `elem` ("-_.=[],:<>+" :: String)


newtype InstWith = InstWith { getInstWith :: [(ModuleName,OpenModule)] }

instance Newtype InstWith [(ModuleName, OpenModule)] where
    pack = InstWith
    unpack = getInstWith

instance Pretty InstWith where
    pretty = dispOpenModuleSubst . Map.fromList . getInstWith

instance Parsec InstWith where
    parsec = InstWith . Map.toList <$> parsecOpenModuleSubst


data Basic = Basic
    { _basicName    :: MungedPackageName
    , _basicVersion :: Version
    , _basicPkgName :: Maybe PackageName
    , _basicLibName :: Maybe UnqualComponentName
    }

basic :: Lens' InstalledPackageInfo Basic
basic f ipi = g <$> f b
  where
    b = Basic
        (mungedPackageName ipi)
        (packageVersion ipi)
        (maybePackageName ipi)
        (sourceLibName ipi)

    g (Basic n v pn ln) = ipi
        & setMungedPackageName n
        & L.sourcePackageId . L.pkgVersion .~ v
        & setMaybePackageName pn
        & L.sourceLibName .~ ln

basicName :: Lens' Basic MungedPackageName
basicName f b = (\x -> b { _basicName = x }) <$> f (_basicName b)
{-# INLINE basicName #-}

basicVersion :: Lens' Basic Version
basicVersion f b = (\x -> b { _basicVersion = x }) <$> f (_basicVersion b)
{-# INLINE basicVersion #-}

basicPkgName :: Lens' Basic (Maybe PackageName)
basicPkgName f b = (\x -> b { _basicPkgName = x }) <$> f (_basicPkgName b)
{-# INLINE basicPkgName #-}

basicLibName :: Lens' Basic (Maybe UnqualComponentName)
basicLibName f b = (\x -> b { _basicLibName = x }) <$> f (_basicLibName b)
{-# INLINE basicLibName #-}

basicFieldGrammar
    :: (FieldGrammar g, Applicative (g Basic))
    => g Basic Basic
basicFieldGrammar = Basic
    <$> optionalFieldDefAla "name"          MQuoted  basicName (mungedPackageName emptyInstalledPackageInfo)
    <*> optionalFieldDefAla "version"       MQuoted  basicVersion nullVersion
    <*> optionalField       "package-name"           basicPkgName
    <*> optionalField       "lib-name"               basicLibName

-- -----------------------------------------------------------------------------
-- Pretty-printing

-- | Pretty print 'InstalledPackageInfo'.
--
-- @pkgRoot@ isn't printed, as ghc-pkg prints it manually (as GHC-8.4).
showInstalledPackageInfo :: InstalledPackageInfo -> String
showInstalledPackageInfo ipi =
    showFullInstalledPackageInfo ipi { pkgRoot = Nothing }

-- | The variant of 'showInstalledPackageInfo' which outputs @pkgroot@ field too.
showFullInstalledPackageInfo :: InstalledPackageInfo -> String
showFullInstalledPackageInfo = showFields fieldsInstalledPackageInfo

showInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showInstalledPackageInfoField = showSingleNamedField fieldsInstalledPackageInfo

showSimpleInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showSimpleInstalledPackageInfoField = showSimpleSingleNamedField fieldsInstalledPackageInfo

dispCompatPackageKey :: String -> Disp.Doc
dispCompatPackageKey = Disp.text

parseCompatPackageKey :: Parse.ReadP r String
parseCompatPackageKey = Parse.munch1 uid_char
    where uid_char c = Char.isAlphaNum c || c `elem` ("-_.=[],:<>+" :: String)

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldsInstalledPackageInfo :: [FieldDescr InstalledPackageInfo]
fieldsInstalledPackageInfo = basicFieldDescrs ++ installedFieldDescrs

basicFieldDescrs :: [FieldDescr InstalledPackageInfo]
basicFieldDescrs =
 [ simpleField "name"
                           disp                   (parseMaybeQuoted parse)
                           mungedPackageName      setMungedPackageName
 , simpleField "version"
                           disp                   parseOptVersion
                           packageVersion         (\ver pkg -> pkg{sourcePackageId=(sourcePackageId pkg){pkgVersion=ver}})
 , simpleField "id"
                           disp                   parse
                           installedUnitId        (\pk pkg -> pkg{installedUnitId=pk})
 , simpleField "instantiated-with"
        (dispOpenModuleSubst . Map.fromList)    (fmap Map.toList parseOpenModuleSubst)
        instantiatedWith   (\iw    pkg -> pkg{instantiatedWith=iw})
 , simpleField "package-name"
                           dispMaybe              parseMaybe
                           maybePackageName       setMaybePackageName
 , simpleField "lib-name"
                           dispMaybe              parseMaybe
                           sourceLibName          (\n pkg -> pkg{sourceLibName=n})
 , simpleField "key"
                           dispCompatPackageKey   parseCompatPackageKey
                           compatPackageKey       (\pk pkg -> pkg{compatPackageKey=pk})
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
 , boolField "indefinite"
        indefinite         (\val pkg -> pkg{indefinite=val})
 , simpleField "exposed-modules"
        showExposedModules parseExposedModules
        exposedModules     (\xs    pkg -> pkg{exposedModules=xs})
 , listField   "hidden-modules"
        disp               parseModuleNameQ
        hiddenModules      (\xs    pkg -> pkg{hiddenModules=xs})
 , simpleField "abi"
        disp               parse
        abiHash            (\abi    pkg -> pkg{abiHash=abi})
 , boolField   "trusted"
        trusted            (\val pkg -> pkg{trusted=val})
 , listField   "import-dirs"
        showFilePath       parseFilePathQ
        importDirs         (\xs pkg -> pkg{importDirs=xs})
 , listField   "library-dirs"
        showFilePath       parseFilePathQ
        libraryDirs        (\xs pkg -> pkg{libraryDirs=xs})
 , listField   "dynamic-library-dirs"
        showFilePath       parseFilePathQ
        libraryDynDirs     (\xs pkg -> pkg{libraryDynDirs=xs})
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
 , listField   "abi-depends"
        disp               parse
        abiDepends         (\xs pkg -> pkg{abiDepends=xs})
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
 , simpleField "pkgroot"
        (maybe mempty showFilePath)  (fmap Just parseFilePathQ)
        pkgRoot                      (\xs pkg -> pkg{pkgRoot=xs})
 ]
