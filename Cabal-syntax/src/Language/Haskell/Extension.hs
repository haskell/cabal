{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Language.Haskell.Extension
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- Haskell language dialects and extensions
module Language.Haskell.Extension
  ( Language (..)
  , knownLanguages
  , classifyLanguage
  , Extension (..)
  , KnownExtension (..)
  , deprecatedExtensions
  , classifyExtension
  , knownExtensions
  ) where

import Distribution.Compat.Prelude

import Data.Array (Array, Ix (inRange), accumArray, bounds, (!))

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- ------------------------------------------------------------

-- * Language

-- ------------------------------------------------------------

-- | This represents a Haskell language dialect.
--
-- Language 'Extension's are interpreted relative to one of these base
-- languages.
data Language
  = -- | The Haskell 98 language as defined by the Haskell 98 report.
    -- <http://haskell.org/onlinereport/>
    Haskell98
  | -- | The Haskell 2010 language as defined by the Haskell 2010 report.
    -- <http://www.haskell.org/onlinereport/haskell2010>
    Haskell2010
  | -- | The GHC2021 collection of language extensions.
    -- <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0380-ghc2021.rst>
    GHC2021
  | -- | The GHC2024 collection of language extensions.
    -- <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0613-ghc2024.rst>
    GHC2024
  | -- | An unknown language, identified by its name.
    UnknownLanguage String
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary Language
instance Structured Language

instance NFData Language where rnf = genericRnf

-- | List of known (supported) languages for GHC, oldest first.
knownLanguages :: [Language]
knownLanguages = [Haskell98, Haskell2010, GHC2021, GHC2024]

instance Pretty Language where
  pretty (UnknownLanguage other) = Disp.text other
  pretty other = Disp.text (show other)

instance Parsec Language where
  parsec = classifyLanguage <$> P.munch1 isAlphaNum

classifyLanguage :: String -> Language
classifyLanguage = \str -> case lookup str langTable of
  Just lang -> lang
  Nothing -> UnknownLanguage str
  where
    langTable =
      [ (show lang, lang)
      | lang <- knownLanguages
      ]

-- ------------------------------------------------------------

-- * Extension

-- ------------------------------------------------------------

-- Note: if you add a new 'KnownExtension':
--

-- * also add it to the Distribution.Simple.X.compilerExtensions lists

--   (where X is each compiler: GHC, UHC, HaskellSuite)
--

-- | This represents language extensions beyond a base 'Language' definition
-- (such as 'Haskell98') that are supported by some implementations, usually
-- in some special mode.
--
-- Where applicable, references are given to an implementation's
-- official documentation.
data Extension
  = -- | Enable a known extension
    EnableExtension KnownExtension
  | -- | Disable a known extension
    DisableExtension KnownExtension
  | -- | An unknown extension, identified by the name of its @LANGUAGE@
    -- pragma.
    UnknownExtension String
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary Extension
instance Structured Extension

instance NFData Extension where rnf = genericRnf

-- | Known Haskell language extensions, including deprecated and undocumented
-- ones.
--
-- Check <https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/exts/table.html “Overview of all language extensions” in GHC User’s Guide>
-- for more information.
data KnownExtension
  = -- | Allow overlapping class instances, provided there is a unique
    -- most specific instance for each use.
    OverlappingInstances
  | -- | Ignore structural rules guaranteeing the termination of class
    -- instance resolution.  Termination is guaranteed by a fixed-depth
    -- recursion stack, and compilation may fail if this depth is
    -- exceeded.
    UndecidableInstances
  | -- | Implies 'OverlappingInstances'.  Allow the implementation to
    -- choose an instance even when it is possible that further
    -- instantiation of types will lead to a more specific instance
    -- being applicable.
    IncoherentInstances
  | -- | /(deprecated)/ Deprecated in favour of 'RecursiveDo'.
    --
    -- Old description: Allow recursive bindings in @do@ blocks, using
    -- the @rec@ keyword. See also 'RecursiveDo'.
    DoRec
  | -- | Allow recursive bindings in @do@ blocks, using the @rec@
    -- keyword, or @mdo@, a variant of @do@.
    RecursiveDo
  | -- | Provide syntax for writing list comprehensions which iterate
    -- over several lists together, like the 'zipWith' family of
    -- functions.
    ParallelListComp
  | -- | Allow multiple parameters in a type class.
    MultiParamTypeClasses
  | -- | Enable the dreaded monomorphism restriction.
    MonomorphismRestriction
  | -- | Enable deep subsumption, relaxing the simple subsumption rules,
    -- implicitly inserting eta-expansions when matching up function types
    -- with different quantification structures.
    DeepSubsumption
  | -- | Allow a specification attached to a multi-parameter type class
    -- which indicates that some parameters are entirely determined by
    -- others. The implementation will check that this property holds
    -- for the declared instances, and will use this property to reduce
    -- ambiguity in instance resolution.
    FunctionalDependencies
  | -- | /(deprecated)/ A synonym for 'RankNTypes'.
    --
    -- Old description: Like 'RankNTypes' but does not allow a
    -- higher-rank type to itself appear on the left of a function
    -- arrow.
    Rank2Types
  | -- | Allow a universally-quantified type to occur on the left of a
    -- function arrow.
    RankNTypes
  | -- | /(deprecated)/ A synonym for 'RankNTypes'.
    --
    -- Old description: Allow data constructors to have polymorphic
    -- arguments.  Unlike 'RankNTypes', does not allow this for ordinary
    -- functions.
    PolymorphicComponents
  | -- | Allow existentially-quantified data constructors.
    ExistentialQuantification
  | -- | Cause a type variable in a signature, which has an explicit
    -- @forall@ quantifier, to scope over the definition of the
    -- accompanying value declaration.
    ScopedTypeVariables
  | -- | Deprecated, use 'ScopedTypeVariables' instead.
    PatternSignatures
  | -- | Enable implicit function parameters with dynamic scope.
    ImplicitParams
  | -- | Relax some restrictions on the form of the context of a type
    -- signature.
    FlexibleContexts
  | -- | Relax some restrictions on the form of the context of an
    -- instance declaration.
    FlexibleInstances
  | -- | Allow data type declarations with no constructors.
    EmptyDataDecls
  | -- | Run the C preprocessor on Haskell source code.
    CPP
  | -- | Allow an explicit kind signature giving the kind of types over
    -- which a type variable ranges.
    KindSignatures
  | -- | Enable a form of pattern which forces evaluation before an
    -- attempted match, and a form of strict @let@/@where@ binding.
    BangPatterns
  | -- | Allow type synonyms in instance heads.
    TypeSynonymInstances
  | -- | Enable Template Haskell, a system for compile-time
    -- metaprogramming.
    TemplateHaskell
  | -- | Enable the Foreign Function Interface.  In GHC, implements the
    -- standard Haskell 98 Foreign Function Interface Addendum, plus
    -- some GHC-specific extensions.
    ForeignFunctionInterface
  | -- | Enable arrow notation.
    Arrows
  | -- | /(deprecated)/ Enable generic type classes, with default instances defined in
    -- terms of the algebraic structure of a type.
    Generics
  | -- | Enable the implicit importing of the module "Prelude".  When
    -- disabled, when desugaring certain built-in syntax into ordinary
    -- identifiers, use whatever is in scope rather than the "Prelude"
    -- -- version.
    ImplicitPrelude
  | -- | Enable syntax for implicitly binding local names corresponding
    -- to the field names of a record.  Puns bind specific names, unlike
    -- 'RecordWildCards'.
    NamedFieldPuns
  | -- | Enable a form of guard which matches a pattern and binds
    -- variables.
    PatternGuards
  | -- | Allow a type declared with @newtype@ to use @deriving@ for any
    -- class with an instance for the underlying type.
    GeneralizedNewtypeDeriving
  | -- Synonym for GeneralizedNewtypeDeriving added in GHC 8.6.1.
    GeneralisedNewtypeDeriving
  | -- | Enable the \"Trex\" extensible records system.
    ExtensibleRecords
  | -- | Enable type synonyms which are transparent in some definitions
    -- and opaque elsewhere, as a way of implementing abstract
    -- datatypes.
    RestrictedTypeSynonyms
  | -- | Enable an alternate syntax for string literals,
    -- with string templating.
    HereDocuments
  | -- | Allow the character @#@ as a postfix modifier on identifiers.
    -- Also enables literal syntax for unboxed values.
    MagicHash
  | -- | Allow data types and type synonyms which are indexed by types,
    -- i.e. ad-hoc polymorphism for types.
    TypeFamilies
  | -- | Allow a standalone declaration which invokes the type class
    -- @deriving@ mechanism.
    StandaloneDeriving
  | -- | Allow certain Unicode characters to stand for certain ASCII
    -- character sequences, e.g. keywords and punctuation.
    UnicodeSyntax
  | -- | Allow the use of unboxed types as foreign types, e.g. in
    -- @foreign import@ and @foreign export@.
    UnliftedFFITypes
  | -- | Enable interruptible FFI.
    InterruptibleFFI
  | -- | Allow use of CAPI FFI calling convention (@foreign import capi@).
    CApiFFI
  | -- | Defer validity checking of types until after expanding type
    -- synonyms, relaxing the constraints on how synonyms may be used.
    LiberalTypeSynonyms
  | -- | Allow the name of a type constructor, type class, or type
    -- variable to be an infix operator.
    TypeOperators
  | -- | Enable syntax for implicitly binding local names corresponding
    -- to the field names of a record.  A wildcard binds all unmentioned
    -- names, unlike 'NamedFieldPuns'.
    RecordWildCards
  | -- | Deprecated, use 'NamedFieldPuns' instead.
    RecordPuns
  | -- | Allow a record field name to be disambiguated by the type of
    -- the record it's in.
    DisambiguateRecordFields
  | -- | Enable traditional record syntax (as supported by Haskell 98)
    TraditionalRecordSyntax
  | -- | Enable overloading of string literals using a type class, much
    -- like integer literals.
    OverloadedStrings
  | -- | Enable generalized algebraic data types, in which type
    -- variables may be instantiated on a per-constructor basis. Implies
    -- 'GADTSyntax'.
    GADTs
  | -- | Enable GADT syntax for declaring ordinary algebraic datatypes.
    GADTSyntax
  | -- | /(deprecated)/ Has no effect.
    --
    -- Old description: Make pattern bindings monomorphic.
    MonoPatBinds
  | -- | Relax the requirements on mutually-recursive polymorphic
    -- functions.
    RelaxedPolyRec
  | -- | Allow default instantiation of polymorphic types in more
    -- situations.
    ExtendedDefaultRules
  | -- | Allow @default@ declarations to explicitly name the class and
    -- be exported.
    NamedDefaults
  | -- | Enable unboxed tuples.
    UnboxedTuples
  | -- | Enable @deriving@ for classes 'Data.Typeable.Typeable' and
    -- 'Data.Generics.Data'.
    DeriveDataTypeable
  | -- | Enable @deriving@ for 'GHC.Generics.Generic' and 'GHC.Generics.Generic1'.
    DeriveGeneric
  | -- | Enable support for default signatures.
    DefaultSignatures
  | -- | Allow type signatures to be specified in instance declarations.
    InstanceSigs
  | -- | Allow a class method's type to place additional constraints on
    -- a class type variable.
    ConstrainedClassMethods
  | -- | Allow imports to be qualified by the package name the module is
    -- intended to be imported from, e.g.
    --
    -- > import "network" Network.Socket
    PackageImports
  | -- | /(deprecated)/ Allow a type variable to be instantiated at a
    -- polymorphic type.
    ImpredicativeTypes
  | -- | /(deprecated)/ Change the syntax for qualified infix operators.
    NewQualifiedOperators
  | -- | Relax the interpretation of left operator sections to allow
    -- unary postfix operators.
    PostfixOperators
  | -- | Enable quasi-quotation, a mechanism for defining new concrete
    -- syntax for expressions and patterns.
    QuasiQuotes
  | -- | Enable generalized list comprehensions, supporting operations
    -- such as sorting and grouping.
    TransformListComp
  | -- | Enable monad comprehensions, which generalise the list
    -- comprehension syntax to work for any monad.
    MonadComprehensions
  | -- | Enable view patterns, which match a value by applying a
    -- function and matching on the result.
    ViewPatterns
  | -- | Allow concrete XML syntax to be used in expressions and patterns,
    -- as per the Haskell Server Pages extension language:
    -- <http://www.haskell.org/haskellwiki/HSP>. The ideas behind it are
    -- discussed in the paper \"Haskell Server Pages through Dynamic Loading\"
    -- by Niklas Broberg, from Haskell Workshop '05.
    XmlSyntax
  | -- | Allow regular pattern matching over lists, as discussed in the
    -- paper \"Regular Expression Patterns\" by Niklas Broberg, Andreas Farre
    -- and Josef Svenningsson, from ICFP '04.
    RegularPatterns
  | -- | Enable the use of tuple sections, e.g. @(, True)@ desugars into
    -- @\x -> (x, True)@.
    TupleSections
  | -- | Allow GHC primops, written in C--, to be imported into a Haskell
    -- file.
    GHCForeignImportPrim
  | -- | Support for patterns of the form @n + k@, where @k@ is an
    -- integer literal.
    NPlusKPatterns
  | -- | Improve the layout rule when @if@ expressions are used in a @do@
    -- block.
    DoAndIfThenElse
  | -- | Enable support for multi-way @if@-expressions.
    MultiWayIf
  | -- | Enable support lambda-@case@ expressions.
    LambdaCase
  | -- | Makes much of the Haskell sugar be desugared into calls to the
    -- function with a particular name that is in scope.
    RebindableSyntax
  | -- | Make @forall@ a keyword in types, which can be used to give the
    -- generalisation explicitly.
    ExplicitForAll
  | -- | Allow contexts to be put on datatypes, e.g. the @Eq a@ in
    -- @data Eq a => Set a = NilSet | ConsSet a (Set a)@.
    DatatypeContexts
  | -- | Local (@let@ and @where@) bindings are monomorphic.
    MonoLocalBinds
  | -- | Enable @deriving@ for the 'Data.Functor.Functor' class.
    DeriveFunctor
  | -- | Enable @deriving@ for the 'Data.Traversable.Traversable' class.
    DeriveTraversable
  | -- | Enable @deriving@ for the 'Data.Foldable.Foldable' class.
    DeriveFoldable
  | -- | Enable non-decreasing indentation for @do@ blocks.
    NondecreasingIndentation
  | -- | Allow imports to be qualified with a safe keyword that requires
    -- the imported module be trusted as according to the Safe Haskell
    -- definition of trust.
    --
    -- > import safe Network.Socket
    SafeImports
  | -- | Compile a module in the Safe, Safe Haskell mode -- a restricted
    -- form of the Haskell language to ensure type safety.
    Safe
  | -- | Compile a module in the Trustworthy, Safe Haskell mode -- no
    -- restrictions apply but the module is marked as trusted as long as
    -- the package the module resides in is trusted.
    Trustworthy
  | -- | Compile a module in the Unsafe, Safe Haskell mode so that
    -- modules compiled using Safe, Safe Haskell mode can't import it.
    Unsafe
  | -- | Allow type class/implicit parameter/equality constraints to be
    -- used as types with the special kind constraint.  Also generalise
    -- the @(ctxt => ty)@ syntax so that any type of kind constraint can
    -- occur before the arrow.
    ConstraintKinds
  | -- | Enable kind polymorphism.
    PolyKinds
  | -- | Enable datatype promotion.
    DataKinds
  | -- | Enable @type data@ declarations, defining constructors at the type level.
    TypeData
  | -- | Enable parallel arrays syntax (@[:@, @:]@) for /Data Parallel Haskell/.
    ParallelArrays
  | -- | Enable explicit role annotations, like in (@type role Foo representational representational@).
    RoleAnnotations
  | -- | Enable overloading of list literals, arithmetic sequences and
    -- list patterns using the 'IsList' type class.
    OverloadedLists
  | -- | Enable case expressions that have no alternatives. Also applies to lambda-case expressions if they are enabled.
    EmptyCase
  | -- | /(deprecated)/ Deprecated in favour of 'DeriveDataTypeable'.
    --
    -- Old description: Triggers the generation of derived 'Typeable'
    -- instances for every datatype and type class declaration.
    AutoDeriveTypeable
  | -- | Desugars negative literals directly (without using negate).
    NegativeLiterals
  | -- | Allow the use of binary integer literal syntax (e.g. @0b11001001@ to denote @201@).
    BinaryLiterals
  | -- | Allow the use of floating literal syntax for all instances of 'Num', including 'Int' and 'Integer'.
    NumDecimals
  | -- | Enable support for type classes with no type parameter.
    NullaryTypeClasses
  | -- | Enable explicit namespaces in module import/export lists.
    ExplicitNamespaces
  | -- | Allow the user to write ambiguous types, and the type inference engine to infer them.
    AllowAmbiguousTypes
  | -- | Enable @foreign import javascript@.
    JavaScriptFFI
  | -- | Allow giving names to and abstracting over patterns.
    PatternSynonyms
  | -- | Allow anonymous placeholders (underscore) inside type signatures.  The
    -- type inference engine will generate a message describing the type inferred
    -- at the hole's location.
    PartialTypeSignatures
  | -- | Allow named placeholders written with a leading underscore inside type
    -- signatures.  Wildcards with the same name unify to the same type.
    NamedWildCards
  | -- | Enable @deriving@ for any class.
    DeriveAnyClass
  | -- | Enable @deriving@ for the 'Language.Haskell.TH.Syntax.Lift' class.
    DeriveLift
  | -- | Enable support for 'static pointers' (and the @static@
    -- keyword) to refer to globally stable names, even across
    -- different programs.
    StaticPointers
  | -- | Switches data type declarations to be strict by default (as if
    -- they had a bang using @BangPatterns@), and allow opt-in field
    -- laziness using @~@.
    StrictData
  | -- | Switches all pattern bindings to be strict by default (as if
    -- they had a bang using @BangPatterns@), ordinary patterns are
    -- recovered using @~@. Implies @StrictData@.
    Strict
  | -- | Allows @do@-notation for types that are @'Applicative'@ as well
    -- as @'Monad'@. When enabled, desugaring @do@ notation tries to use
    -- @(<*>)@ and @'fmap'@ and @'join'@ as far as possible.
    ApplicativeDo
  | -- | Allow records to use duplicated field labels for accessors.
    DuplicateRecordFields
  | -- | Enable explicit type applications with the syntax @id \@Int@.
    TypeApplications
  | -- | Dissolve the distinction between types and kinds, allowing the compiler
    -- to reason about kind equality and therefore enabling GADTs to be promoted
    -- to the type-level.
    TypeInType
  | -- | Allow recursive (and therefore undecidable) super-class relationships.
    UndecidableSuperClasses
  | -- | A temporary extension to help library authors check if their
    -- code will compile with the new planned desugaring of fail.
    MonadFailDesugaring
  | -- | A subset of @TemplateHaskell@ including only quoting.
    TemplateHaskellQuotes
  | -- | Allows use of the @#label@ syntax.
    OverloadedLabels
  | -- | Allow functional dependency annotations on type families to declare them
    -- as injective.
    TypeFamilyDependencies
  | -- | Allow multiple @deriving@ clauses, each optionally qualified with a
    -- /strategy/.
    DerivingStrategies
  | -- | Enable deriving instances via types of the same runtime representation.
    -- Implies 'DerivingStrategies'.
    DerivingVia
  | -- | Enable the use of unboxed sum syntax.
    UnboxedSums
  | -- | Allow use of hexadecimal literal notation for floating-point values.
    HexFloatLiterals
  | -- | Allow @do@ blocks etc. in argument position.
    BlockArguments
  | -- | Allow use of underscores in numeric literals.
    NumericUnderscores
  | -- | Allow @forall@ in constraints.
    QuantifiedConstraints
  | -- | Have @*@ refer to @Type@.
    StarIsType
  | -- | Liberalises deriving to provide instances for empty data types.
    EmptyDataDeriving
  | -- | Enable detection of complete user-supplied kind signatures.
    CUSKs
  | -- | Allows the syntax @import M qualified@.
    ImportQualifiedPost
  | -- | Allow the use of standalone kind signatures.
    StandaloneKindSignatures
  | -- | Enable unlifted newtypes.
    UnliftedNewtypes
  | -- | Use whitespace to determine whether the minus sign stands for negation or subtraction.
    LexicalNegation
  | -- | Enable qualified do-notation desugaring.
    QualifiedDo
  | -- | Enable linear types.
    LinearTypes
  | -- | Allow the use of visible forall in types of terms.
    RequiredTypeArguments
  | -- | Enable the generation of selector functions corresponding to record fields.
    FieldSelectors
  | -- | Enable the use of record dot-accessor and updater syntax
    OverloadedRecordDot
  | -- | Provides record @.@ syntax in record updates, e.g. @x {foo.bar = 1}@.
    OverloadedRecordUpdate
  | -- | Enable data types for which an unlifted or levity-polymorphic result kind is inferred.
    UnliftedDatatypes
  | -- | Enable syntax for primitive numeric literals, e.g. @3#Int8@
    ExtendedLiterals
  | -- | Undocumented parsing-related extensions introduced in GHC 7.0.
    AlternativeLayoutRule
  | -- | Undocumented parsing-related extensions introduced in GHC 7.0.
    AlternativeLayoutRuleTransitional
  | -- | Undocumented parsing-related extensions introduced in GHC 7.2.
    RelaxedLayout
  | -- | Allow the use of type abstraction syntax.
    TypeAbstractions
  | -- | Allow the use of built-in syntax for list, tuple and sum type constructors
    -- rather than being exclusive to data constructors.
    ListTuplePuns
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data)

instance Binary KnownExtension
instance Structured KnownExtension

instance NFData KnownExtension where rnf = genericRnf

-- | Extensions that have been deprecated, possibly paired with another
-- extension that replaces it.
deprecatedExtensions :: [(Extension, Maybe Extension)]
deprecatedExtensions =
  [ (EnableExtension RecordPuns, Just (EnableExtension NamedFieldPuns))
  , (EnableExtension PatternSignatures, Just (EnableExtension ScopedTypeVariables))
  ]

-- NOTE: when adding deprecated extensions that have new alternatives
-- we must be careful to make sure that the deprecation messages are
-- valid. We must not recommend aliases that cannot be used with older
-- compilers, perhaps by adding support in Cabal to translate the new
-- name to the old one for older compilers. Otherwise we are in danger
-- of the scenario in ticket #689.

instance Pretty Extension where
  pretty (UnknownExtension other) = Disp.text other
  pretty (EnableExtension ke) = Disp.text (show ke)
  pretty (DisableExtension ke) = Disp.text ("No" ++ show ke)

instance Parsec Extension where
  parsec = classifyExtension <$> P.munch1 isAlphaNum

instance Pretty KnownExtension where
  pretty ke = Disp.text (show ke)

classifyExtension :: String -> Extension
classifyExtension string =
  case classifyKnownExtension string of
    Just ext -> EnableExtension ext
    Nothing ->
      case string of
        'N' : 'o' : string' ->
          case classifyKnownExtension string' of
            Just ext -> DisableExtension ext
            Nothing -> UnknownExtension string
        _ -> UnknownExtension string

-- | 'read' for 'KnownExtension's is really really slow so for the Text
-- instance
-- what we do is make a simple table indexed off the first letter in the
-- extension name. The extension names actually cover the range @'A'-'Z'@
-- pretty densely and the biggest bucket is 7 so it's not too bad. We just do
-- a linear search within each bucket.
--
-- This gives an order of magnitude improvement in parsing speed, and it'll
-- also allow us to do case insensitive matches in future if we prefer.
classifyKnownExtension :: String -> Maybe KnownExtension
classifyKnownExtension "" = Nothing
classifyKnownExtension string@(c : _)
  | inRange (bounds knownExtensionTable) c =
      lookup string (knownExtensionTable ! c)
  | otherwise = Nothing

knownExtensionTable :: Array Char [(String, KnownExtension)]
knownExtensionTable =
  accumArray
    (flip (:))
    []
    ('A', 'Z')
    [ (hd, (str, extension)) -- assume KnownExtension's Show returns a non-empty string
    | (extension, str@(hd : _)) <- map (\e -> (e, show e)) [toEnum 0 ..]
    ]

knownExtensions :: [KnownExtension]
knownExtensions = [minBound .. maxBound]
