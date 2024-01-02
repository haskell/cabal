{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import Data.Map.Strict (Map)

import Data.Bifunctor                  (first)
import Data.Proxy                      (Proxy (..))
import Data.Void                       (Void)
import Distribution.CabalSpecVersion   (CabalSpecVersion, showCabalSpecVersion)
import Distribution.Compat.Newtype     (pack')
import Distribution.FieldGrammar.Class (FieldGrammar (..))
import Distribution.Fields.Field       (FieldName)
import Distribution.Pretty             (pretty)
import Distribution.Simple.Utils       (fromUTF8BS)
import GHC.Generics                    (Generic)
import System.Environment              (getArgs)
import System.Exit                     (exitFailure)

import Distribution.PackageDescription.FieldGrammar (buildInfoFieldGrammar, packageDescriptionFieldGrammar, testSuiteFieldGrammar)

import qualified Data.Map.Strict  as Map
import qualified Text.PrettyPrint as PP

import qualified Zinza as Z

import Distribution.Described
import Distribution.Utils.GrammarRegex

import Distribution.ModuleName         (ModuleName)
import Distribution.Types.Version      (Version)
import Distribution.Types.VersionRange (VersionRange)
import Language.Haskell.Extension

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [tmpl] -> do
            -- TODO: getArgs
            run <- Z.parseAndCompileTemplateIO tmpl
            contents <- run $ Z
                { zBuildInfoFields          = fromReference buildInfoFieldGrammar
                , zPackageDescriptionFields = fromReference packageDescriptionFieldGrammar
                , zTestSuiteFields          = fromReference $ testSuiteFieldGrammar // buildInfoFieldGrammar
                , zProductions              =
                    [ zproduction "interactive-extension" (reXs xGroupInteractive) "Language Extensions related to GHC interactive."
                    , zproduction "phase-extension" (reXs xGroupPhase) "Language Extensions related to a particular GHC phase."
                    , zproduction "syntax-extension" (reXs xGroupSyntax) "Syntax Language Extensions."
                    , zproduction "import-export-extension" (reXs xGroupImportExport) "Import and Export Language Extensions."
                    , zproduction "type-extension" (reXs xGroupTypes) "Language Extensions for Types."
                    , zproduction "record-extension" (reXs xGroupRecords) "Record Language Extensions."
                    , zproduction "deriving-extension" (reXs xGroupDeriving) "Language Extensions for deriving mechanisms."
                    , zproduction "pattern-extension" (reXs xGroupPatterns) "Patterns Language Extensions."
                    , zproduction "classes-instances-extension" (reXs xGroupClassesInstances) "Language Extensions for class and instance declarations."
                    , zproduction "literal-extension" (reXs xGroupLiterals) "Literals Language Extensions."
                    , zproduction "constraint-extension" (reXs xGroupConstraints) "Constraint Language Extensions."
                    , zproduction "type-signature-extension" (reXs xGroupTypeSignatures) "Type Signature Language Extensions."
                    , zproduction "binding-generalisation-extension" (reXs xGroupBindingsGeneralisation) "Language Extensions for bindings and generalisation "
                    , zproduction "template-haskell-extension" (reXs xGroupTemplates) "Template Haskell Language Extensions."
                    , zproduction "bang-strict-extension" (reXs xGroupBangStrict) "Bang pattern and Strict Haskell Language Extensions."
                    , zproduction "parallel-concurrent-extension" (reXs xGroupParallelConcurrent) "Parallel and Concurrent Language Extensions."
                    , zproduction "unboxed-primitive-extension" (reXs xGroupUnboxedPrimitive) "Unboxed types and Primitive operations Language Extensions."
                    , zproduction "foreign-extension" (reXs xGroupForeign) "Foreign function interface (FFI) Language Extensions."
                    , zproduction "safe-extension" (reXs xGroupSafe) "Safe Haskell Language Extensions."
                    , zproduction "miscellaneous-extension" (reXs xGroupMiscellaneous) "Miscellaneous Language Extensions."
                    , zproduction "bugs-extension" (reXs xGroupBugs) "Language Extensions related to GHC bugs and infelicities."
                    , zproduction "ungrouped-extension" (reXs xUngrouped) "Language Extensions not belonging to other extension groups, includes undocumented extensions."
                    , zproduction "enable-extension" reKnownExtension "GHC Language Extensions, some of these may be on by default."
                    , zproduction "disable-extension" reDisableExtension "Disable a GHC Language Extension."
                    , zproduction "hs-string"       reHsString
                        "String as in Haskell; it's recommended to avoid using Haskell-specific escapes."
                    , zproduction "unqual-name"     reUnqualComponent $ unwords
                        [ "Unqualified component names are used for package names, component names etc. but not flag names."
                        , "Unqualified component name consist of components separated by dash, each component is non-empty alphanumeric string, with at least one alphabetic character."
                        , "In other words, component may not look like a number."
                        ]

                    , zproduction "module-name"     (describe (Proxy :: Proxy ModuleName))
                        "Haskell module name as recognized by Cabal parser."
                    , zproduction "version"         (describe (Proxy :: Proxy Version))
                        "Version is to first approximation numbers separated by dots, where leading zero is not allowed and each version digit is consists at most of nine characters."
                    , zproduction "version-range"   (describe (Proxy :: Proxy VersionRange))
                        "Version range syntax is recursive. Also note the set syntax added in ``cabal-version: 3.0``, set cannot be empty."
                    ]
                , zSpaceList                = show $ regexDoc $
                    REMunch RESpaces1 (RENamed "element" RETodo)
                , zCommaList                = show $ regexDoc $
                    expandedCommaList (RENamed "element" RETodo)
                , zOptCommaList             = show $ regexDoc $
                    expandedOptCommaList (RENamed "element" RETodo)

                , zNull                     = null
                , zNotNull                  = not . null
                }

            putStrLn contents
        _ -> do
          putStrLn "Usage: generator <tmpl>"
          exitFailure

zproduction :: String -> GrammarRegex Void -> String -> ZProduction
zproduction name re desc = ZProduction
    { zprodName        = name
    , zprodSyntax      = show (regexDoc re')
    , zprodDescription = desc
    }
  where
    re' = case re of
        RENamed _ r -> r
        _           -> re

-- also in UnitTests.Distribution.Described
expandedCommaList :: GrammarRegex a -> GrammarRegex a
expandedCommaList = REUnion . expandedCommaList'

expandedCommaList' :: GrammarRegex a -> [GrammarRegex a]
expandedCommaList' r =
    [ REMunch reSpacedComma r
    , reComma <> RESpaces <> REMunch1 reSpacedComma r
    , REMunch1 reSpacedComma r <> RESpaces <> reComma
    ]

expandedOptCommaList :: GrammarRegex a -> GrammarRegex a
expandedOptCommaList r = REUnion $ reSpacedList r : expandedCommaList' r

-------------------------------------------------------------------------------
-- Template Inputs
-------------------------------------------------------------------------------

data Z = Z
    { zBuildInfoFields          :: [ZField]
    , zPackageDescriptionFields :: [ZField]
    , zTestSuiteFields          :: [ZField]
    , zProductions              :: [ZProduction]
    , zSpaceList                :: String
    , zCommaList                :: String
    , zOptCommaList             :: String
    , zNull                     :: String -> Bool
    , zNotNull                  :: String -> Bool
    }
  deriving (Generic)

data ZField = ZField
    { zfieldName            :: String
    , zfieldAvailableSince  :: String
    , zfieldDeprecatedSince :: (String, String)
    , zfieldRemovedIn       :: (String, String)
    , zfieldFormat          :: String
    , zfieldDefault         :: String
    , zfieldSyntax          :: String
    }
  deriving (Generic)

data ZProduction = ZProduction
    { zprodName        :: String
    , zprodSyntax      :: String
    , zprodDescription :: String
    }
  deriving (Generic)

instance Z.Zinza Z where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP

instance Z.Zinza ZField where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP

instance Z.Zinza ZProduction where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP

-------------------------------------------------------------------------------
-- From reference
-------------------------------------------------------------------------------

-- TODO: produce ZField
fromReference :: Reference a a -> [ZField]
fromReference (Reference m) =
    [ ZField
        { zfieldName            = fromUTF8BS n
        , zfieldAvailableSince  = maybe "" showCabalSpecVersion (fdAvailableSince desc)
        , zfieldDeprecatedSince = maybe ("", "") (first showCabalSpecVersion) (fdDeprecatedSince desc)
        , zfieldRemovedIn       = maybe ("", "") (first showCabalSpecVersion) (fdRemovedIn desc)
        , zfieldFormat          = fmt
        , zfieldDefault         = def
        , zfieldSyntax          = syntax
        }
    | (n, desc) <- Map.toList m
    , let (fmt, def, syntax) = fromFieldDesc' (fdDescription desc)
    ]

fromFieldDesc' :: FieldDesc' -> (String, String, String)
fromFieldDesc' (MonoidalFieldAla s)        = ("Monoidal field",  "",       show s)
fromFieldDesc' (BooleanFieldDesc def)      = ("Boolean field",   show def, show $ describeDoc ([] :: [Bool]))
fromFieldDesc' (OptionalFieldAla s)        = ("Optional field",  "",       show s)
fromFieldDesc' (OptionalFieldDefAla s def) = ("Optional field",  show def, show s)
fromFieldDesc' FreeTextField               = ("Free text field", "",      "")
fromFieldDesc' (UniqueField s)             = ("Required field",  "",       show s)

-------------------------------------------------------------------------------
-- Reference
-------------------------------------------------------------------------------

newtype Reference a b = Reference (Map FieldName FieldDesc)
  deriving (Functor)

referenceAvailableSince :: CabalSpecVersion -> Reference a b -> Reference a b
referenceAvailableSince v (Reference m) =
    Reference (fmap (fieldDescAvailableSince v) m)

referenceRemovedIn :: CabalSpecVersion -> String -> Reference a b -> Reference a b
referenceRemovedIn v desc (Reference m) =
    Reference (fmap (fieldDescRemovedIn v desc) m)

referenceDeprecatedSince :: CabalSpecVersion -> String -> Reference a b -> Reference a b
referenceDeprecatedSince v desc (Reference m) =
    Reference (fmap (fieldDescDeprecatedSince v desc) m)

(//) :: Reference a b -> Reference c d -> Reference a b
Reference ab // Reference cd = Reference $ Map.difference ab cd

fieldDescAvailableSince :: CabalSpecVersion -> FieldDesc -> FieldDesc
fieldDescAvailableSince v d = d { fdAvailableSince = Just v }

fieldDescRemovedIn :: CabalSpecVersion -> String -> FieldDesc -> FieldDesc
fieldDescRemovedIn v desc d = d { fdRemovedIn = Just (v, desc) }

fieldDescDeprecatedSince :: CabalSpecVersion -> String -> FieldDesc -> FieldDesc
fieldDescDeprecatedSince v desc d = d { fdDeprecatedSince = Just (v, desc) }

data FieldDesc = FieldDesc
    { fdAvailableSince  :: Maybe CabalSpecVersion
    , fdRemovedIn       :: Maybe (CabalSpecVersion, String)
    , fdDeprecatedSince :: Maybe (CabalSpecVersion, String)
    , fdDescription     :: FieldDesc'
    }
  deriving Show

reference :: FieldName -> FieldDesc' -> Reference a b
reference fn d = Reference $ Map.singleton fn $ FieldDesc Nothing Nothing Nothing d

data FieldDesc'
    = BooleanFieldDesc Bool
    | UniqueField  PP.Doc  -- ^ not used in BuildInfo
    | FreeTextField        -- ^ not user in BuildInfo
    | OptionalFieldAla PP.Doc
    | OptionalFieldDefAla PP.Doc PP.Doc
    | MonoidalFieldAla PP.Doc
  deriving Show

instance Applicative (Reference a) where
    pure _                      = Reference Map.empty
    Reference f <*> Reference x = Reference (Map.union f x)

instance FieldGrammar Described Reference where
    blurFieldGrammar _ (Reference xs) = Reference xs

    uniqueFieldAla fn pack _l =
        reference fn $ UniqueField (describeDoc pack)

    booleanFieldDef fn _l def =
        reference fn $ BooleanFieldDesc def

    optionalFieldAla fn pack _l =
        reference fn $ OptionalFieldAla (describeDoc pack)

    optionalFieldDefAla fn pack _l def =
        reference fn $ OptionalFieldDefAla
            (describeDoc pack)
            (pretty $ pack' pack def)

    freeTextField fn _l = reference fn FreeTextField

    freeTextFieldDef   fn _l = reference fn FreeTextField
    freeTextFieldDefST fn _l = reference fn FreeTextField

    monoidalFieldAla fn pack _l =
        reference fn (MonoidalFieldAla (describeDoc pack))

    prefixedFields _pfx _l = Reference Map.empty

    knownField _fn = Reference Map.empty -- TODO

    -- hidden fields are hidden from the reference.
    hiddenField _ = Reference Map.empty

    deprecatedSince = referenceDeprecatedSince
    removedIn       = referenceRemovedIn
    availableSince  v _ r = referenceAvailableSince v r
