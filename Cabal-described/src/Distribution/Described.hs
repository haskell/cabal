{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Described (
    Described (..),
    describeDoc,
    -- * Regular expressions
    GrammarRegex (..),
    reEps,
    reChar,
    reChars,
    reMunchCS,
    reMunch1CS,
    -- * Variables
    reVar0,
    reVar1,
    -- * Special expressions
    reDot,
    reComma,
    reSpacedComma,
    reHsString,
    reUnqualComponent,
    -- *
    describeFlagAssignmentNonEmpty,
    -- * Lists
    reSpacedList,
    reCommaList,
    reCommaNonEmpty,
    reOptCommaList,
    -- * Character Sets
    csChar,
    csAlpha,
    csAlphaNum,
    csUpper,
    csNotSpace,
    csNotSpaceOrComma,
    -- * tasty
    testDescribed,
    ) where

import Prelude
       ( Bool (..), Char, Either (..), Enum (..), Eq (..), Ord (..), Show (..), String
       , elem, fmap, foldr, id, map, maybe, otherwise, return, reverse, undefined
       , ($), (.), (<$>)
       )

import Data.Functor.Identity (Identity (..))
import Data.Maybe            (fromMaybe)
import Data.Proxy            (Proxy (..))
import Data.String           (IsString (..))
import Data.Typeable         (Typeable, typeOf)
import Data.Void             (Void, vacuous)
import Test.QuickCheck       (Arbitrary (..), Property, counterexample)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Distribution.Compat.Semigroup (Semigroup (..))
import Distribution.Parsec           (Parsec, eitherParsec)
import Distribution.Pretty           (Pretty, prettyShow)

import qualified Distribution.Utils.CharSet as CS
import qualified RERE                       as RE
import qualified RERE.CharSet               as RE
import qualified Text.PrettyPrint           as PP

import Distribution.Utils.GrammarRegex

-- Types
import Distribution.Compat.Newtype
import Distribution.Compiler                       (CompilerFlavor, CompilerId, knownCompilerFlavors)
import Distribution.PackageDescription.FieldGrammar (CompatLicenseFile, CompatDataDir)
import Distribution.FieldGrammar.Newtypes
import Distribution.ModuleName                     (ModuleName)
import Distribution.System                         (Arch, OS, knownArches, knownOSs)
import Distribution.Types.AbiDependency            (AbiDependency)
import Distribution.Types.AbiHash                  (AbiHash)
import Distribution.Types.BenchmarkType            (BenchmarkType)
import Distribution.Types.BuildType                (BuildType)
import Distribution.Types.Dependency               (Dependency)
import Distribution.Types.ExecutableScope          (ExecutableScope)
import Distribution.Types.ExeDependency            (ExeDependency)
import Distribution.Types.ExposedModule            (ExposedModule)
import Distribution.Types.Flag                     (FlagAssignment, FlagName)
import Distribution.Types.ForeignLib               (LibVersionInfo)
import Distribution.Types.ForeignLibOption         (ForeignLibOption)
import Distribution.Types.ForeignLibType           (ForeignLibType)
import Distribution.Types.IncludeRenaming          (IncludeRenaming)
import Distribution.Types.LegacyExeDependency      (LegacyExeDependency)
import Distribution.Types.LibraryVisibility        (LibraryVisibility)
import Distribution.Types.Mixin                    (Mixin)
import Distribution.Types.ModuleReexport           (ModuleReexport)
import Distribution.Types.ModuleRenaming           (ModuleRenaming)
import Distribution.Types.MungedPackageName        (MungedPackageName)
import Distribution.Types.PackageId                (PackageIdentifier)
import Distribution.Types.PackageName              (PackageName)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint)
import Distribution.Types.PkgconfigDependency      (PkgconfigDependency)
import Distribution.Types.SourceRepo               (RepoType)
import Distribution.Types.TestType                 (TestType)
import Distribution.Types.UnitId                   (UnitId)
import Distribution.Types.UnqualComponentName      (UnqualComponentName)
import Distribution.Utils.Path                     (SymbolicPath, RelativePath)
import Distribution.Verbosity                      (Verbosity)
import Distribution.Version                        (Version, VersionRange)
import Language.Haskell.Extension                  (Extension, Language, knownLanguages)

-- | Class describing the pretty/parsec format of a.
class (Pretty a, Parsec a) => Described a where
    -- | A pretty document of "regex" describing the field format
    describe :: proxy a -> GrammarRegex void

-- | Pretty-print description.
--
-- >>> describeDoc ([] :: [Bool])
-- \left\{ \mathop{\mathord{``}\mathtt{True}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{False}\mathord{"}} \right\}
--
describeDoc :: Described a => proxy a -> PP.Doc
describeDoc p = regexDoc (describe p)

instance Described Bool where
    describe _ = REUnion ["True", "False"]

instance Described a => Described (Identity a) where
    describe _ = describe ([] :: [a])

-------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

reSpacedList :: GrammarRegex a -> GrammarRegex a
reSpacedList = REMunch RESpaces1

reCommaList :: GrammarRegex a -> GrammarRegex a
reCommaList = RECommaList

reCommaNonEmpty :: GrammarRegex a -> GrammarRegex a
reCommaNonEmpty = RECommaNonEmpty

reOptCommaList :: GrammarRegex a -> GrammarRegex a
reOptCommaList = REOptCommaList

-------------------------------------------------------------------------------
-- Specific grammars
-------------------------------------------------------------------------------

reHsString :: GrammarRegex a
reHsString = RENamed "hs-string" impl  where
    impl = reChar '"' <> REMunch reEps (REUnion [strChar, escChar]) <> reChar '"'
    strChar = RECharSet $ CS.difference CS.universe (CS.fromList "\"\\")

    escChar = REUnion
        [ "\\&"
        , "\\\\"
        , REUnion ["\\n", RENamed "escapes" "\\n"] -- TODO
        , "\\" <> RECharSet "0123456789"
        , "\\o" <> RECharSet "01234567"
        , "\\x" <> RECharSet "0123456789abcdefABCDEF"
        , REUnion ["\\^@", RENamed "control" "\\^@"] -- TODO
        , REUnion ["\\NUL", RENamed "ascii" "\\NUL"] -- TODO
        ]

reUnqualComponent :: GrammarRegex a
reUnqualComponent = RENamed "unqual-name" $
    REMunch1 (reChar '-') component
  where
    component
        = REMunch reEps (RECharSet csAlphaNum)
        -- currently the parser accepts "csAlphaNum `difference` "0123456789"
        -- which is larger set than CS.alpha
        --
        -- Hackage rejects non ANSI names, so it's not so relevant.
        <> RECharSet CS.alpha
        <> REMunch reEps (RECharSet csAlphaNum)

reDot :: GrammarRegex a
reDot = reChar '.'

reComma :: GrammarRegex a
reComma = reChar ','

reSpacedComma :: GrammarRegex a
reSpacedComma = RESpaces <> reComma <> RESpaces

-------------------------------------------------------------------------------
-- Character sets
-------------------------------------------------------------------------------

csChar :: Char -> CS.CharSet
csChar = CS.singleton

csAlpha :: CS.CharSet
csAlpha = CS.alpha

csAlphaNum :: CS.CharSet
csAlphaNum = CS.alphanum

csUpper :: CS.CharSet
csUpper = CS.upper

csNotSpace :: CS.CharSet
csNotSpace = CS.difference CS.universe $ CS.singleton ' '

csNotSpaceOrComma :: CS.CharSet
csNotSpaceOrComma = CS.difference csNotSpace $ CS.singleton ','

-------------------------------------------------------------------------------
-- Special
-------------------------------------------------------------------------------

describeFlagAssignmentNonEmpty :: GrammarRegex void
describeFlagAssignmentNonEmpty = REMunch1 RESpaces1 $
    REUnion [fromString "+", fromString "-"] <> describe (Proxy :: Proxy FlagName)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

convert :: GrammarRegex Void -> RE.RE Void
convert = go id . vacuous where
    go :: Ord b => (a -> b) -> GrammarRegex a -> RE.RE b
    go f (REAppend rs)      = foldr (\r acc -> go f r <> acc) RE.Eps rs
    go f (REUnion rs)       = foldr (\r acc -> go f r RE.\/ acc) RE.Null rs
    go _ (RECharSet cs)     = RE.Ch (convertCS cs)
    go _ (REString str)     = RE.string_ str

    go f (REMunch sep r)    = RE.Eps RE.\/ r' <> RE.star_ (sep' <> r') where
        sep' = go f sep
        r'   = go f r
    go f (REMunch1 sep r)   = r' <> RE.star_ (sep' <> r') where
        sep' = go f sep
        r'   = go f r
    go f (REMunchR n sep r)
        | n <= 0    = RE.Eps
        | otherwise = RE.Eps RE.\/ r' <> go' (pred n)
      where
        sep' = go f sep
        r'   = go f r

        go' m | m <= 0    = RE.Eps
              | otherwise = RE.Eps RE.\/ sep' <> r' <> go' (pred m)

    go f (REOpt r)          = RE.Eps RE.\/ go f r

    go f (REVar a)          = RE.Var (f a)
    go f (RENamed _ r)      = go f r
    go f (RERec n r)        = RE.fix_ (fromString n)
        (go (maybe RE.B (RE.F . f)) r)

    go _ RESpaces           = RE.Eps RE.\/ RE.ch_ ' ' RE.\/ "  " RE.\/ "\n"
    go _ RESpaces1          = RE.ch_ ' ' RE.\/ "  " RE.\/ "\n"

    go f (RECommaList r)    = go f (expandedCommaList r)
    go f (RECommaNonEmpty r)= go f (expandedCommaNonEmpty r)
    go f (REOptCommaList r) = go f (expandedOptCommaList r)

    go _ RETodo             = RE.Null

expandedCommaList :: GrammarRegex a -> GrammarRegex a
expandedCommaList = REUnion . expandedCommaList'

expandedCommaNonEmpty :: GrammarRegex a -> GrammarRegex a
expandedCommaNonEmpty r = REUnion
    [ REMunch1 reSpacedComma r
    , reComma <> RESpaces <> REMunch1 reSpacedComma r
    , REMunch1 reSpacedComma r <> RESpaces <> reComma
    ]

expandedCommaList' :: GrammarRegex a -> [GrammarRegex a]
expandedCommaList' r =
    [ REMunch reSpacedComma r
    , reComma <> RESpaces <> REMunch1 reSpacedComma r
    , REMunch1 reSpacedComma r <> RESpaces <> reComma
    ]

expandedOptCommaList :: GrammarRegex a -> GrammarRegex a
expandedOptCommaList r = REUnion $ reSpacedList r : expandedCommaList' r

convertCS :: CS.CharSet -> RE.CharSet
convertCS = RE.fromIntervalList . CS.toIntervalList

-------------------------------------------------------------------------------
-- tasty
-------------------------------------------------------------------------------

testDescribed
    :: forall a. (Arbitrary a, Described a, Typeable a, Eq a, Show a)
    => Proxy a
    -> TestTree
testDescribed _ = testGroup name
    [ testProperty "parsec" propParsec
    , testProperty "pretty" propPretty
    , testProperty "roundtrip" propRoundtrip
    ]
  where
    name = show (typeOf (undefined :: a))

    propParsec :: Ex a -> Property
    propParsec (Example str) = counterexample (show res) $ case res of
        Right _ -> True
        Left _  -> False
      where
        res :: Either String a
        res = eitherParsec str

    rr :: RE.RE Void
    rr = convert $ describe (Proxy :: Proxy a)

    propPretty :: a -> Property
    propPretty x = counterexample str $ RE.matchR rr str
      where
        str = prettyShow x

    propRoundtrip :: a -> Property
    propRoundtrip x = counterexample (show (res, str)) $ case res of
        Right y -> x == y
        Left _  -> False
      where
        str = prettyShow x
        res = eitherParsec str

newtype Ex a = Example String
  deriving (Show)

instance Described a => Arbitrary (Ex a) where
    arbitrary
        = fmap Example
        $ fromMaybe (return "")
        $ RE.generate 10 5
        $ convert $ describe (Proxy :: Proxy a)

    shrink (Example s)
        | '\n' `elem` s = [ Example $ map (\c -> if c == '\n' then ' ' else c) s ]
        | otherwise     = []

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Described AbiDependency where
    describe _ =
        describe (Proxy :: Proxy UnitId) <>
        reChar '=' <>
        describe (Proxy :: Proxy AbiHash)

instance Described AbiHash where
    describe _ = reMunchCS csAlphaNum

instance Described Arch where
    describe _ = REUnion
        [ fromString (prettyShow arch)
        | arch <- knownArches
        ]

instance Described BenchmarkType where
    describe _ = "exitcode-stdio-1.0"

instance Described BuildType where
    describe _ = REUnion ["Simple","Configure","Custom","Hooks","Make","Default"]

instance Described CompilerFlavor where
    describe _ = REUnion
        [ fromString (prettyShow c)
        | c <- knownCompilerFlavors
        ]

instance Described CompilerId where
    describe _ =
        describe (Proxy :: Proxy CompilerFlavor)
        <> fromString "-"
        <> describe (Proxy :: Proxy Version)

instance Described Dependency where
    describe _ = REAppend
        [ RENamed "pkg-name" (describe (Proxy :: Proxy PackageName))
        , REOpt $
               reChar ':'
            <> REUnion
                [ reUnqualComponent
                , REAppend
                    [ reChar '{'
                    , RESpaces
                    -- no leading or trailing comma
                    , REMunch1 reSpacedComma reUnqualComponent
                    , RESpaces
                    , reChar '}'
                    ]
                ]

        , REOpt $ RESpaces <> vr
        ]
      where
        vr = RENamed "version-range" (describe (Proxy :: Proxy VersionRange))

instance Described ExecutableScope where
    describe _ = REUnion ["public","private"]

instance Described ExeDependency where
    describe _ = RETodo

instance Described ExposedModule where
    describe _ = RETodo

instance Described Extension where
    describe _ = RETodo

instance Described FlagAssignment where
    describe _ = REMunch RESpaces1 $
        REUnion [fromString "+", fromString "-"] <> describe (Proxy :: Proxy FlagName)

instance Described FlagName where
    describe _ = lead <> rest where
        lead = RECharSet $ csAlphaNum <> fromString "_"
        rest = reMunchCS $ csAlphaNum <> fromString "_-"

instance Described ForeignLibOption where
    describe _ = "standalone"

instance Described ForeignLibType where
    describe _ = REUnion ["native-shared","native-static"]

instance Described IncludeRenaming where
    describe _ = mr <> REOpt (RESpaces <> "requires" <> RESpaces1 <> mr)
      where
        mr = describe (Proxy :: Proxy ModuleRenaming)

instance Described Language where
    describe _ = REUnion $ (REString . show) <$> reverse knownLanguages

instance Described LegacyExeDependency where
    describe _ = RETodo

instance Described LibraryVisibility where
    describe _ = REUnion ["public","private"]

instance Described LibVersionInfo where
    describe _ = reDigits <> REOpt (reChar ':' <> reDigits <> REOpt (reChar ':' <> reDigits)) where
        reDigits = reChars ['0'..'9']

instance Described Mixin where
    describe _ =
        RENamed "package-name" (describe (Proxy :: Proxy PackageName)) <>
        REOpt (reChar ':' <> RENamed "library-name" (describe (Proxy :: Proxy UnqualComponentName))) <>
        REOpt (RESpaces1 <> describe (Proxy :: Proxy IncludeRenaming))

instance Described ModuleName where
    describe _ = REMunch1 (reChar '.') component where
        component = RECharSet csUpper <> REMunch reEps (REUnion [RECharSet csAlphaNum, RECharSet (fromString "_'")])

instance Described ModuleReexport where
    describe _ = RETodo

instance Described ModuleRenaming where
    describe _ = REUnion
        [ reEps
        , "hiding" <> RESpaces <> bp (REMunch reSpacedComma mn)
        , bp (REMunch reSpacedComma entry)
        ]
      where
        bp r = "(" <> RESpaces <> r <> RESpaces <> ")"
        mn = RENamed "module-name" $ describe (Proxy :: Proxy ModuleName)

        entry = mn <> REOpt (RESpaces1 <> "as" <> RESpaces1 <> mn)

instance Described MungedPackageName where
    describe _ = RETodo

instance Described OS where
    describe _ = REUnion
        [ fromString (prettyShow os)
        | os <- knownOSs
        ]

instance Described PackageIdentifier where
    describe _ = describe (Proxy :: Proxy PackageName) <> fromString "-" <> describe (Proxy :: Proxy Version)

instance Described PackageName where
    describe _ = reUnqualComponent

instance Described PackageVersionConstraint where
    describe _ = describe (Proxy :: Proxy PackageName) <> REUnion
        [ fromString "-" <> describe (Proxy :: Proxy Version)
        , RESpaces <> describe (Proxy :: Proxy VersionRange)
        ]

instance Described PkgconfigDependency where
    describe _ = RETodo

instance Described RepoType where
    describe _ = reMunch1CS $ csAlphaNum <> csChar '_' <> csChar '-'

instance Described TestType where
    describe _ = REUnion ["exitcode-stdio-1.0", "detailed-0.9"]

instance Described Verbosity where
    describe _ = REUnion
        [ REUnion ["0", "1", "2", "3"]
        , REUnion ["silent", "normal", "verbose", "debug", "deafening"]
          <> REMunch reEps (RESpaces <> "+" <>
            -- markoutput is left out on purpose
            REUnion ["callsite", "callstack", "nowrap", "timestamp", "stderr", "stdout" ])
        ]

instance Described Version where
    describe _ = REMunch1 reDot reDigits where
        reDigits = REUnion
            [ reChar '0'
            , reChars ['1'..'9'] <> REMunchR 8 reEps (reChars ['0'..'9'])
            ]

instance Described VersionRange where
    describe _ = RERec "version-range" $ REUnion
        [ "=="  <> RESpaces <> ver
        , ">"   <> RESpaces <> ver
        , "<"   <> RESpaces <> ver
        , "<="  <> RESpaces <> ver
        , ">="  <> RESpaces <> ver
        , "^>=" <> RESpaces <> ver

        -- ==0.1.*
        , "==" <> RESpaces <> wildVer

        , reVar0 <> RESpaces  <> "||" <> RESpaces <> reVar0
        , reVar0 <> RESpaces  <> "&&" <> RESpaces <> reVar0
        , "(" <> RESpaces <> reVar0  <> RESpaces <> ")"

        -- == { 0.1.2 }
        -- silly haddock: ^>= { 0.1.2, 3.4.5 }
        , "=="  <> RESpaces <> verSet
        , "^>=" <> RESpaces <> verSet
        ]
      where
        ver'    = describe (Proxy :: Proxy Version)
        ver     = RENamed "version" ver'
        wildVer = ver' <> ".*"
        verSet  = "{" <> RESpaces <> REMunch1 reSpacedComma ver <> RESpaces <> "}"

instance Described UnitId where
    describe _ = reMunch1CS $ csAlphaNum <> csChar '-' <> csChar '_' <> csChar '.' <> csChar '+'

instance Described UnqualComponentName where
    describe _ = reUnqualComponent

-------------------------------------------------------------------------------
-- Instances: Newtypes
-------------------------------------------------------------------------------

class Sep sep => DescribeSep sep where
    describeSep :: Proxy sep -> GrammarRegex a -> GrammarRegex a

instance DescribeSep CommaVCat   where describeSep _ = reCommaList
instance DescribeSep CommaFSep   where describeSep _ = reCommaList
instance DescribeSep VCat        where describeSep _ = reCommaList
instance DescribeSep FSep        where describeSep _ = reOptCommaList
instance DescribeSep NoCommaFSep where describeSep _ = reSpacedList

instance (Newtype a b, DescribeSep sep, Described b) => Described (List sep b a) where
    describe _ = describeSep (Proxy :: Proxy sep) (describe (Proxy :: Proxy b))

instance (Newtype a b, Ord a, DescribeSep sep, Described b) => Described (Set' sep b a) where
    describe _ = describeSep (Proxy :: Proxy sep) (describe (Proxy :: Proxy b))

instance Described Token where
    describe _ = REUnion [reHsString, reMunch1CS csNotSpaceOrComma]

instance Described Token' where
    describe _ = REUnion [reHsString, reMunch1CS csNotSpace]

instance Described a => Described (MQuoted a) where
    -- TODO: this is simplification
    describe _ = describe ([] :: [a])

instance Described SpecVersion where
    describe _ = "3.4" -- :)

instance Described SpecLicense where
    describe _ = RETodo

instance Described TestedWith where
    describe _ = RETodo


instance Described (SymbolicPath from to) where
    describe _ = describe ([] :: [Token])

instance Described (RelativePath from to) where
    describe _ = describe ([] :: [Token])

instance Described (SymbolicPathNT from to) where
    describe _ = describe ([] :: [Token])

instance Described (RelativePathNT from to) where
    describe _ = describe ([] :: [Token])

instance Described CompatLicenseFile where
    describe _ = describe ([] :: [Token])

instance Described CompatDataDir where
    describe _ = describe ([] :: [Token])

instance Described FilePathNT where
    describe _ = describe ([] :: [Token])
