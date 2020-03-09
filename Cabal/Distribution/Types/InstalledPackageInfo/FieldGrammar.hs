{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Distribution.Types.InstalledPackageInfo.FieldGrammar (
    ipiFieldGrammar,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens               (Lens', (&), (.~))
import Distribution.Compat.Newtype
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Described
import Distribution.FieldGrammar.FieldDescrs
import Distribution.License
import Distribution.ModuleName
import Distribution.Package
import Distribution.Parsec
import Distribution.Parsec.Newtypes
import Distribution.Pretty
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.MungedPackageName
import Distribution.Types.UnqualComponentName
import Distribution.Version

import qualified Data.Char                       as Char
import qualified Data.Map                        as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX               as SPDX
import qualified Text.PrettyPrint                as Disp

import Distribution.Types.InstalledPackageInfo

import qualified Distribution.Types.InstalledPackageInfo.Lens as L
import qualified Distribution.Types.PackageId.Lens            as L

-- Note: GHC goes nuts and inlines everything,
-- One can see e.g. in -ddump-simpl-stats:
--
-- 34886 KnownBranch
--  8197 wild1_ixF0
--
-- https://ghc.haskell.org/trac/ghc/ticket/13253 might be the cause.
--
-- The workaround is to prevent GHC optimising the code:
infixl 4 <+>
(<+>) :: Applicative f => f (a -> b) -> f a -> f b
f <+> x = f <*> x
{-# NOINLINE (<+>) #-}

ipiFieldGrammar
    :: (FieldGrammar g, Applicative (g InstalledPackageInfo), Applicative (g Basic))
    => g InstalledPackageInfo InstalledPackageInfo
ipiFieldGrammar = mkInstalledPackageInfo
    -- Deprecated fields
    <$> monoidalFieldAla    "hugs-options"         (alaList' FSep Token)         unitedList
        --- https://github.com/haskell/cabal/commit/40f3601e17024f07e0da8e64d3dd390177ce908b
        ^^^ deprecatedSince CabalSpecV1_22 "hugs isn't supported anymore"
    -- Very basic fields: name, version, package-name, lib-name and visibility
    <+> blurFieldGrammar basic basicFieldGrammar
    -- Basic fields
    <+> optionalFieldDef    "id"                                                 L.installedUnitId (mkUnitId "")
    <+> optionalFieldDefAla "instantiated-with"    InstWith                      L.instantiatedWith []
    <+> optionalFieldDefAla "key"                  CompatPackageKey              L.compatPackageKey ""
    <+> optionalFieldDefAla "license"              SpecLicenseLenient            L.license (Left SPDX.NONE)
    <+> freeTextFieldDefST    "copyright"                                          L.copyright
    <+> freeTextFieldDefST    "maintainer"                                         L.maintainer
    <+> freeTextFieldDefST    "author"                                             L.author
    <+> freeTextFieldDefST    "stability"                                          L.stability
    <+> freeTextFieldDefST    "homepage"                                           L.homepage
    <+> freeTextFieldDefST    "package-url"                                        L.pkgUrl
    <+> freeTextFieldDefST    "synopsis"                                           L.synopsis
    <+> freeTextFieldDefST    "description"                                        L.description
    <+> freeTextFieldDefST    "category"                                           L.category
    -- Installed fields
    <+> optionalFieldDef    "abi"                                                L.abiHash (mkAbiHash "")
    <+> booleanFieldDef     "indefinite"                                         L.indefinite False
    <+> booleanFieldDef     "exposed"                                            L.exposed False
    <+> monoidalFieldAla    "exposed-modules"      ExposedModules                L.exposedModules
    <+> monoidalFieldAla    "hidden-modules"       (alaList' FSep MQuoted)       L.hiddenModules
    <+> booleanFieldDef     "trusted"                                            L.trusted False
    <+> monoidalFieldAla    "import-dirs"          (alaList' FSep FilePathNT)    L.importDirs
    <+> monoidalFieldAla    "library-dirs"         (alaList' FSep FilePathNT)    L.libraryDirs
    <+> monoidalFieldAla    "dynamic-library-dirs" (alaList' FSep FilePathNT)    L.libraryDynDirs
    <+> optionalFieldDefAla "data-dir"             FilePathNT                    L.dataDir ""
    <+> monoidalFieldAla    "hs-libraries"         (alaList' FSep Token)         L.hsLibraries
    <+> monoidalFieldAla    "extra-libraries"      (alaList' FSep Token)         L.extraLibraries
    <+> monoidalFieldAla    "extra-ghci-libraries" (alaList' FSep Token)         L.extraGHCiLibraries
    <+> monoidalFieldAla    "include-dirs"         (alaList' FSep FilePathNT)    L.includeDirs
    <+> monoidalFieldAla    "includes"             (alaList' FSep FilePathNT)    L.includes
    <+> monoidalFieldAla    "depends"              (alaList FSep)                L.depends
    <+> monoidalFieldAla    "abi-depends"          (alaList FSep)                L.abiDepends
    <+> monoidalFieldAla    "cc-options"           (alaList' FSep Token)         L.ccOptions
    <+> monoidalFieldAla    "cxx-options"          (alaList' FSep Token)         L.cxxOptions
    <+> monoidalFieldAla    "ld-options"           (alaList' FSep Token)         L.ldOptions
    <+> monoidalFieldAla    "framework-dirs"       (alaList' FSep FilePathNT)    L.frameworkDirs
    <+> monoidalFieldAla    "frameworks"           (alaList' FSep Token)         L.frameworks
    <+> monoidalFieldAla    "haddock-interfaces"   (alaList' FSep FilePathNT)    L.haddockInterfaces
    <+> monoidalFieldAla    "haddock-html"         (alaList' FSep FilePathNT)    L.haddockHTMLs
    <+> optionalFieldAla    "pkgroot"              FilePathNT                    L.pkgRoot
  where
    mkInstalledPackageInfo _ Basic {..} = InstalledPackageInfo
        -- _basicPkgName is not used
        -- setMaybePackageId says it can be no-op.
        (PackageIdentifier pn _basicVersion)
        (combineLibraryName ln _basicLibName)
        (mkComponentId "") -- installedComponentId_, not in use
        _basicLibVisibility
      where
        MungedPackageName pn ln = _basicName
{-# SPECIALIZE ipiFieldGrammar :: FieldDescrs InstalledPackageInfo InstalledPackageInfo #-}
{-# SPECIALIZE ipiFieldGrammar :: ParsecFieldGrammar InstalledPackageInfo InstalledPackageInfo #-}
{-# SPECIALIZE ipiFieldGrammar :: PrettyFieldGrammar InstalledPackageInfo InstalledPackageInfo #-}

-- (forall b. [b]) ~ ()
unitedList :: Lens' a [b]
unitedList f s = s <$ f []

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- | Combine 'LibraryName'. in parsing we prefer value coming
-- from munged @name@ field over the @lib-name@.
--
-- /Should/ be irrelevant.
combineLibraryName :: LibraryName -> LibraryName -> LibraryName
combineLibraryName l@(LSubLibName _) _ = l
combineLibraryName _ l                 = l

-- To maintain backwards-compatibility, we accept both comma/non-comma
-- separated variants of this field.  You SHOULD use the comma syntax if you
-- use any new functions, although actually it's unambiguous due to a quirk
-- of the fact that modules must start with capital letters.

showExposedModules :: [ExposedModule] -> Disp.Doc
showExposedModules xs
    | all isExposedModule xs = Disp.fsep (map pretty xs)
    | otherwise = Disp.fsep (Disp.punctuate Disp.comma (map pretty xs))
    where isExposedModule (ExposedModule _ Nothing) = True
          isExposedModule _ = False

-- | Setter for the @package-name@ field.  It should be acceptable for this
-- to be a no-op.
setMaybePackageName :: Maybe PackageName -> InstalledPackageInfo -> InstalledPackageInfo
setMaybePackageName Nothing   ipi = ipi
setMaybePackageName (Just pn) ipi = ipi
    { sourcePackageId = (sourcePackageId ipi) {pkgName=pn}
    }

setMungedPackageName :: MungedPackageName -> InstalledPackageInfo -> InstalledPackageInfo
setMungedPackageName (MungedPackageName pn ln) ipi = ipi
    { sourcePackageId = (sourcePackageId ipi) {pkgName=pn}
    , sourceLibName   = ln
    }

--- | Returns @Just@ if the @name@ field of the IPI record would not contain
--- the package name verbatim.  This helps us avoid writing @package-name@
--- when it's redundant.
maybePackageName :: InstalledPackageInfo -> Maybe PackageName
maybePackageName ipi = case sourceLibName ipi of
    LMainLibName  -> Nothing
    LSubLibName _ -> Just (packageName ipi)

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

newtype ExposedModules = ExposedModules { getExposedModules :: [ExposedModule] }

instance Newtype [ExposedModule] ExposedModules

instance Parsec ExposedModules where
    parsec = ExposedModules <$> parsecOptCommaList parsec

instance Pretty ExposedModules where
    pretty = showExposedModules . getExposedModules

instance Described ExposedModules where
    describe _ = REMunch (REOpt reComma) (describe (Proxy :: Proxy ExposedModule))

newtype CompatPackageKey = CompatPackageKey { getCompatPackageKey :: String }

instance Newtype String CompatPackageKey

instance Pretty CompatPackageKey where
    pretty = Disp.text . getCompatPackageKey

instance Parsec CompatPackageKey where
    parsec = CompatPackageKey <$> P.munch1 uid_char where
        uid_char c = Char.isAlphaNum c || c `elem` ("-_.=[],:<>+" :: String)

instance Described CompatPackageKey where
    describe _ = RETodo

newtype InstWith = InstWith { getInstWith :: [(ModuleName,OpenModule)] }

instance Newtype  [(ModuleName, OpenModule)] InstWith

instance Pretty InstWith where
    pretty = dispOpenModuleSubst . Map.fromList . getInstWith

instance Parsec InstWith where
    parsec = InstWith . Map.toList <$> parsecOpenModuleSubst

instance Described InstWith where
    describe _ = RETodo

-- | SPDX License expression or legacy license. Lenient parser, accepts either.
newtype SpecLicenseLenient = SpecLicenseLenient { getSpecLicenseLenient :: Either SPDX.License License }

instance Newtype (Either SPDX.License License) SpecLicenseLenient

instance Parsec SpecLicenseLenient where
    parsec = fmap SpecLicenseLenient $ Left <$> P.try parsec <|> Right <$> parsec

instance Pretty SpecLicenseLenient where
    pretty = either pretty pretty . getSpecLicenseLenient

instance Described SpecLicenseLenient where
    describe _ = RETodo

-------------------------------------------------------------------------------
-- Basic fields
-------------------------------------------------------------------------------

-- | This type is used to mangle fields as
-- in serialised textual representation
-- to the actual 'InstalledPackageInfo' fields.
data Basic = Basic
    { _basicName          :: MungedPackageName
    , _basicVersion       :: Version
    , _basicPkgName       :: Maybe PackageName
    , _basicLibName       :: LibraryName
    , _basicLibVisibility :: LibraryVisibility
    }

basic :: Lens' InstalledPackageInfo Basic
basic f ipi = g <$> f b
  where
    b = Basic
        (mungedPackageName ipi)
        (packageVersion ipi)
        (maybePackageName ipi)
        (sourceLibName ipi)
        (libVisibility ipi)

    g (Basic n v pn ln lv) = ipi
        & setMungedPackageName n
        & L.sourcePackageId . L.pkgVersion .~ v
        & setMaybePackageName pn
        & L.sourceLibName .~ ln
        & L.libVisibility .~ lv

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
basicLibName f b = (\x -> b { _basicLibName = maybeToLibraryName x }) <$>
    f (libraryNameString (_basicLibName b))
{-# INLINE basicLibName #-}

basicLibVisibility :: Lens' Basic LibraryVisibility
basicLibVisibility f b = (\x -> b { _basicLibVisibility = x }) <$>
    f (_basicLibVisibility b)
{-# INLINE basicLibVisibility #-}

basicFieldGrammar
    :: (FieldGrammar g, Applicative (g Basic))
    => g Basic Basic
basicFieldGrammar = mkBasic
    <$> optionalFieldDefAla "name"          MQuoted  basicName (mungedPackageName emptyInstalledPackageInfo)
    <*> optionalFieldDefAla "version"       MQuoted  basicVersion nullVersion
    <*> optionalField       "package-name"           basicPkgName
    <*> optionalField       "lib-name"               basicLibName
    <+> optionalFieldDef    "visibility"             basicLibVisibility LibraryVisibilityPrivate
  where
    mkBasic n v pn ln lv = Basic n v pn ln' lv'
      where
        ln' = maybe LMainLibName LSubLibName ln
        -- Older GHCs (<8.8) always report installed libraries as private
        -- because their ghc-pkg builds with an older Cabal.
        -- So we always set LibraryVisibilityPublic for main (unnamed) libs.
        -- This can be removed once we stop supporting GHC<8.8, at the
        -- condition that we keep marking main libraries as public when
        -- registering them.
        lv' = if
                let MungedPackageName _ mln = n in
                -- We need to check both because on ghc<8.2 ln' will always
                -- be LMainLibName
                ln' == LMainLibName && mln == LMainLibName
              then LibraryVisibilityPublic
              else lv
