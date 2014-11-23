{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

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

module Language.Haskell.Extension (
        Language(..),
        knownLanguages,

        Extension(..),
        KnownExtension(..),
        knownExtensions,
        deprecatedExtensions
  ) where

import Distribution.Text (Text(..))
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import qualified Data.Char as Char (isAlphaNum)
import Data.Array (Array, accumArray, bounds, Ix(inRange), (!))
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- ------------------------------------------------------------
-- * Language
-- ------------------------------------------------------------

-- | This represents a Haskell language dialect.
--
-- Language 'Extension's are interpreted relative to one of these base
-- languages.
--
data Language =

  -- | The Haskell 98 language as defined by the Haskell 98 report.
  -- <http://haskell.org/onlinereport/>
     Haskell98

  -- | The Haskell 2010 language as defined by the Haskell 2010 report.
  -- <http://www.haskell.org/onlinereport/haskell2010>
  | Haskell2010

  -- | An unknown language, identified by its name.
  | UnknownLanguage String
  deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary Language

knownLanguages :: [Language]
knownLanguages = [Haskell98, Haskell2010]

instance Text Language where
  disp (UnknownLanguage other) = Disp.text other
  disp other                   = Disp.text (show other)

  parse = do
    lang <- Parse.munch1 Char.isAlphaNum
    return (classifyLanguage lang)

classifyLanguage :: String -> Language
classifyLanguage = \str -> case lookup str langTable of
    Just lang -> lang
    Nothing   -> UnknownLanguage str
  where
    langTable = [ (show lang, lang)
                | lang <- knownLanguages ]

-- ------------------------------------------------------------
-- * Extension
-- ------------------------------------------------------------

-- Note: if you add a new 'KnownExtension':
--
-- * also add it to the Distribution.Simple.X.languageExtensions lists
--   (where X is each compiler: GHC, JHC, LHC, UHC, HaskellSuite)
--
-- | This represents language extensions beyond a base 'Language' definition
-- (such as 'Haskell98') that are supported by some implementations, usually
-- in some special mode.
--
-- Where applicable, references are given to an implementation's
-- official documentation.

data Extension =
  -- | Enable a known extension
    EnableExtension KnownExtension

  -- | Disable a known extension
  | DisableExtension KnownExtension

  -- | An unknown extension, identified by the name of its @LANGUAGE@
  -- pragma.
  | UnknownExtension String

  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary Extension

data KnownExtension =

  -- | Allow overlapping class instances, provided there is a unique
  -- most specific instance for each use.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-overlap>
    OverlappingInstances

  -- | Ignore structural rules guaranteeing the termination of class
  -- instance resolution.  Termination is guaranteed by a fixed-depth
  -- recursion stack, and compilation may fail if this depth is
  -- exceeded.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#undecidable-instances>
  | UndecidableInstances

  -- | Implies 'OverlappingInstances'.  Allow the implementation to
  -- choose an instance even when it is possible that further
  -- instantiation of types will lead to a more specific instance
  -- being applicable.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-overlap>
  | IncoherentInstances

  -- | /(deprecated)/ Allows recursive bindings in @do@ blocks, using the @rec@
  -- keyword. See also 'RecursiveDo'.
  | DoRec

  -- | Allows recursive bindings using @mdo@, a variant of @do@.
  -- @DoRec@ provides a different, preferred syntax.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#recursive-do-notation>
  | RecursiveDo

  -- | Provide syntax for writing list comprehensions which iterate
  -- over several lists together, like the 'zipWith' family of
  -- functions.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#parallel-list-comprehensions>
  | ParallelListComp

  -- | Allow multiple parameters in a type class.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#multi-param-type-classes>
  | MultiParamTypeClasses

  -- | Enable the dreaded monomorphism restriction.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/monomorphism.html>
  | MonomorphismRestriction

  -- | Allow a specification attached to a multi-parameter type class
  -- which indicates that some parameters are entirely determined by
  -- others. The implementation will check that this property holds
  -- for the declared instances, and will use this property to reduce
  -- ambiguity in instance resolution.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#functional-dependencies>
  | FunctionalDependencies

  -- | Like 'RankNTypes' but does not allow a higher-rank type to
  -- itself appear on the left of a function arrow.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#universal-quantification>
  | Rank2Types

  -- | Allow a universally-quantified type to occur on the left of a
  -- function arrow.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#universal-quantification>
  | RankNTypes

  -- | Allow data constructors to have polymorphic arguments.  Unlike
  -- 'RankNTypes', does not allow this for ordinary functions.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#universal-quantification>
  | PolymorphicComponents

  -- | Allow existentially-quantified data constructors.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#existential-quantification>
  | ExistentialQuantification

  -- | Cause a type variable in a signature, which has an explicit
  -- @forall@ quantifier, to scope over the definition of the
  -- accompanying value declaration.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables>
  | ScopedTypeVariables

  -- | Deprecated, use 'ScopedTypeVariables' instead.
  | PatternSignatures

  -- | Enable implicit function parameters with dynamic scope.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#implicit-parameters>
  | ImplicitParams

  -- | Relax some restrictions on the form of the context of a type
  -- signature.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#flexible-contexts>
  | FlexibleContexts

  -- | Relax some restrictions on the form of the context of an
  -- instance declaration.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-rules>
  | FlexibleInstances

  -- | Allow data type declarations with no constructors.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#nullary-types>
  | EmptyDataDecls

  -- | Run the C preprocessor on Haskell source code.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/options-phases.html#c-pre-processor>
  | CPP

  -- | Allow an explicit kind signature giving the kind of types over
  -- which a type variable ranges.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#kinding>
  | KindSignatures

  -- | Enable a form of pattern which forces evaluation before an
  -- attempted match, and a form of strict @let@/@where@ binding.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/bang-patterns.html>
  | BangPatterns

  -- | Allow type synonyms in instance heads.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#flexible-instance-head>
  | TypeSynonymInstances

  -- | Enable Template Haskell, a system for compile-time
  -- metaprogramming.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/template-haskell.html>
  | TemplateHaskell

  -- | Enable the Foreign Function Interface.  In GHC, implements the
  -- standard Haskell 98 Foreign Function Interface Addendum, plus
  -- some GHC-specific extensions.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi.html>
  | ForeignFunctionInterface

  -- | Enable arrow notation.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/arrow-notation.html>
  | Arrows

  -- | /(deprecated)/ Enable generic type classes, with default instances defined in
  -- terms of the algebraic structure of a type.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-classes.html>
  | Generics

  -- | Enable the implicit importing of the module "Prelude".  When
  -- disabled, when desugaring certain built-in syntax into ordinary
  -- identifiers, use whatever is in scope rather than the "Prelude"
  -- -- version.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#rebindable-syntax>
  | ImplicitPrelude

  -- | Enable syntax for implicitly binding local names corresponding
  -- to the field names of a record.  Puns bind specific names, unlike
  -- 'RecordWildCards'.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#record-puns>
  | NamedFieldPuns

  -- | Enable a form of guard which matches a pattern and binds
  -- variables.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#pattern-guards>
  | PatternGuards

  -- | Allow a type declared with @newtype@ to use @deriving@ for any
  -- class with an instance for the underlying type.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#newtype-deriving>
  | GeneralizedNewtypeDeriving

  -- | Enable the \"Trex\" extensible records system.
  --
  -- * <http://cvs.haskell.org/Hugs/pages/users_guide/hugs-only.html#TREX>
  | ExtensibleRecords

  -- | Enable type synonyms which are transparent in some definitions
  -- and opaque elsewhere, as a way of implementing abstract
  -- datatypes.
  --
  -- * <http://cvs.haskell.org/Hugs/pages/users_guide/restricted-synonyms.html>
  | RestrictedTypeSynonyms

  -- | Enable an alternate syntax for string literals,
  -- with string templating.
  --
  -- * <http://cvs.haskell.org/Hugs/pages/users_guide/here-documents.html>
  | HereDocuments

  -- | Allow the character @#@ as a postfix modifier on identifiers.
  -- Also enables literal syntax for unboxed values.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#magic-hash>
  | MagicHash

  -- | Allow data types and type synonyms which are indexed by types,
  -- i.e. ad-hoc polymorphism for types.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-families.html>
  | TypeFamilies

  -- | Allow a standalone declaration which invokes the type class
  -- @deriving@ mechanism.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#stand-alone-deriving>
  | StandaloneDeriving

  -- | Allow certain Unicode characters to stand for certain ASCII
  -- character sequences, e.g. keywords and punctuation.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#unicode-syntax>
  | UnicodeSyntax

  -- | Allow the use of unboxed types as foreign types, e.g. in
  -- @foreign import@ and @foreign export@.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi.html#id681687>
  | UnliftedFFITypes

  -- | Enable interruptible FFI.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi.html#ffi-interruptible>
  | InterruptibleFFI

  -- | Allow use of CAPI FFI calling convention (@foreign import capi@).
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi.html#ffi-capi>
  | CApiFFI

  -- | Defer validity checking of types until after expanding type
  -- synonyms, relaxing the constraints on how synonyms may be used.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#type-synonyms>
  | LiberalTypeSynonyms

  -- | Allow the name of a type constructor, type class, or type
  -- variable to be an infix operator.
  | TypeOperators

  -- | Enable syntax for implicitly binding local names corresponding
  -- to the field names of a record.  A wildcard binds all unmentioned
  -- names, unlike 'NamedFieldPuns'.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#record-wildcards>
  | RecordWildCards

  -- | Deprecated, use 'NamedFieldPuns' instead.
  | RecordPuns

  -- | Allow a record field name to be disambiguated by the type of
  -- the record it's in.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#disambiguate-fields>
  | DisambiguateRecordFields

  -- | Enable traditional record syntax (as supported by Haskell 98)
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#traditional-record-syntax>
  | TraditionalRecordSyntax

  -- | Enable overloading of string literals using a type class, much
  -- like integer literals.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#overloaded-strings>
  | OverloadedStrings

  -- | Enable generalized algebraic data types, in which type
  -- variables may be instantiated on a per-constructor basis. Implies
  -- 'GADTSyntax'.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt>
  | GADTs

  -- | Enable GADT syntax for declaring ordinary algebraic datatypes.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt-style>
  | GADTSyntax

  -- | Make pattern bindings monomorphic.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/monomorphism.html#id630981>
  | MonoPatBinds

  -- | Relax the requirements on mutually-recursive polymorphic
  -- functions.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#typing-binds>
  | RelaxedPolyRec

  -- | Allow default instantiation of polymorphic types in more
  -- situations.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/interactive-evaluation.html#extended-default-rules>
  | ExtendedDefaultRules

  -- | Enable unboxed tuples.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/primitives.html#unboxed-tuples>
  | UnboxedTuples

  -- | Enable @deriving@ for classes 'Data.Typeable.Typeable' and
  -- 'Data.Generics.Data'.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#deriving-typeable>
  | DeriveDataTypeable

  -- | Enable @deriving@ for 'GHC.Generics.Generic' and 'GHC.Generics.Generic1'.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#deriving-typeable>
  | DeriveGeneric

  -- | Enable support for default signatures.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#class-default-signatures>
  | DefaultSignatures

  -- | Allow type signatures to be specified in instance declarations.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#instance-sigs>
  | InstanceSigs

  -- | Allow a class method's type to place additional constraints on
  -- a class type variable.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#class-method-types>
  | ConstrainedClassMethods

  -- | Allow imports to be qualified by the package name the module is
  -- intended to be imported from, e.g.
  --
  -- > import "network" Network.Socket
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#package-imports>
  | PackageImports

  -- | /(deprecated)/ Allow a type variable to be instantiated at a
  -- polymorphic type.
  --
  -- * <http://www.haskell.org/ghc/docs/6.12.3/html/users_guide/other-type-extensions.html#impredicative-polymorphism>
  | ImpredicativeTypes

  -- | /(deprecated)/ Change the syntax for qualified infix operators.
  --
  -- * <http://www.haskell.org/ghc/docs/6.12.3/html/users_guide/syntax-extns.html#new-qualified-operators>
  | NewQualifiedOperators

  -- | Relax the interpretation of left operator sections to allow
  -- unary postfix operators.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#postfix-operators>
  | PostfixOperators

  -- | Enable quasi-quotation, a mechanism for defining new concrete
  -- syntax for expressions and patterns.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/template-haskell.html#th-quasiquotation>
  | QuasiQuotes

  -- | Enable generalized list comprehensions, supporting operations
  -- such as sorting and grouping.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#generalised-list-comprehensions>
  | TransformListComp

  -- | Enable monad comprehensions, which generalise the list
  -- comprehension syntax to work for any monad.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#monad-comprehensions>
  | MonadComprehensions

  -- | Enable view patterns, which match a value by applying a
  -- function and matching on the result.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#view-patterns>
  | ViewPatterns

  -- | Allow concrete XML syntax to be used in expressions and patterns,
  -- as per the Haskell Server Pages extension language:
  -- <http://www.haskell.org/haskellwiki/HSP>. The ideas behind it are
  -- discussed in the paper \"Haskell Server Pages through Dynamic Loading\"
  -- by Niklas Broberg, from Haskell Workshop '05.
  | XmlSyntax

  -- | Allow regular pattern matching over lists, as discussed in the
  -- paper \"Regular Expression Patterns\" by Niklas Broberg, Andreas Farre
  -- and Josef Svenningsson, from ICFP '04.
  | RegularPatterns

  -- | Enables the use of tuple sections, e.g. @(, True)@ desugars into
  -- @\x -> (x, True)@.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#tuple-sections>
  | TupleSections

  -- | Allows GHC primops, written in C--, to be imported into a Haskell
  -- file.
  | GHCForeignImportPrim

  -- | Support for patterns of the form @n + k@, where @k@ is an
  -- integer literal.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#n-k-patterns>
  | NPlusKPatterns

  -- | Improve the layout rule when @if@ expressions are used in a @do@
  -- block.
  | DoAndIfThenElse

  -- | Enable support for multi-way @if@-expressions.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#multi-way-if>
  | MultiWayIf

  -- | Enable support lambda-@case@ expressions.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#lambda-case>
  | LambdaCase

  -- | Makes much of the Haskell sugar be desugared into calls to the
  -- function with a particular name that is in scope.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#rebindable-syntax>
  | RebindableSyntax

  -- | Make @forall@ a keyword in types, which can be used to give the
  -- generalisation explicitly.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#explicit-foralls>
  | ExplicitForAll

  -- | Allow contexts to be put on datatypes, e.g. the @Eq a@ in
  -- @data Eq a => Set a = NilSet | ConsSet a (Set a)@.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#datatype-contexts>
  | DatatypeContexts

  -- | Local (@let@ and @where@) bindings are monomorphic.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#mono-local-binds>
  | MonoLocalBinds

  -- | Enable @deriving@ for the 'Data.Functor.Functor' class.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#deriving-typeable>
  | DeriveFunctor

  -- | Enable @deriving@ for the 'Data.Traversable.Traversable' class.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#deriving-typeable>
  | DeriveTraversable

  -- | Enable @deriving@ for the 'Data.Foldable.Foldable' class.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#deriving-typeable>
  | DeriveFoldable

  -- | Enable non-decreasing indentation for @do@ blocks.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/bugs-and-infelicities.html#infelicities-syntax>
  | NondecreasingIndentation

  -- | Allow imports to be qualified with a safe keyword that requires
  -- the imported module be trusted as according to the Safe Haskell
  -- definition of trust.
  --
  -- > import safe Network.Socket
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html#safe-imports>
  | SafeImports

  -- | Compile a module in the Safe, Safe Haskell mode -- a restricted
  -- form of the Haskell language to ensure type safety.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html#safe-trust>
  | Safe

  -- | Compile a module in the Trustworthy, Safe Haskell mode -- no
  -- restrictions apply but the module is marked as trusted as long as
  -- the package the module resides in is trusted.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html#safe-trust>
  | Trustworthy

  -- | Compile a module in the Unsafe, Safe Haskell mode so that
  -- modules compiled using Safe, Safe Haskell mode can't import it.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html#safe-trust>
  | Unsafe

  -- | Allow type class/implicit parameter/equality constraints to be
  -- used as types with the special kind constraint.  Also generalise
  -- the @(ctxt => ty)@ syntax so that any type of kind constraint can
  -- occur before the arrow.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/constraint-kind.html>
  | ConstraintKinds

  -- | Enable kind polymorphism.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/kind-polymorphism.html>
  | PolyKinds

  -- | Enable datatype promotion.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/promotion.html>
  | DataKinds

  -- | Enable parallel arrays syntax (@[:@, @:]@) for /Data Parallel Haskell/.
  --
  -- * <http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell>
  | ParallelArrays

  -- | Enable explicit role annotations, like in (@data T a\@R@).
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/roles.html>
  | RoleAnnotations

  -- | Enable overloading of list literals, arithmetic sequences and
  -- list patterns using the 'IsList' type class.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#overloaded-lists>
  | OverloadedLists

  -- | Enables case expressions that have no alternatives. Also applies to lambda-case expressions if they are enabled.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#empty-case>
  | EmptyCase

  -- | Triggers the generation of derived 'Typeable' instances for every
  -- datatype and type class declaration.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#auto-derive-typeable>
  | AutoDeriveTypeable

  -- | Desugars negative literals directly (without using negate).
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#negative-literals>
  | NegativeLiterals

  -- | Allows the use of binary integer literal syntax (e.g. @0b11001001@ to denote @201@).
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#binary-literals>
  | BinaryLiterals

  -- | Allows the use of floating literal syntax for all instances of 'Num', including 'Int' and 'Integer'.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#num-decimals>
  | NumDecimals

  -- | Enables support for type classes with no type parameter.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#nullary-type-classes>
  | NullaryTypeClasses

  -- | Enable explicit namespaces in module import/export lists.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#explicit-namespaces>
  | ExplicitNamespaces

  -- | Allow the user to write ambiguous types, and the type inference engine to infer them.
  --
  -- * <http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#ambiguity>
  | AllowAmbiguousTypes

  -- | Enable @foreign import javascript@
  | JavaScriptFFI

  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data)

instance Binary KnownExtension

{-# DEPRECATED knownExtensions
   "KnownExtension is an instance of Enum and Bounded, use those instead." #-}
knownExtensions :: [KnownExtension]
knownExtensions = [minBound..maxBound]

-- | Extensions that have been deprecated, possibly paired with another
-- extension that replaces it.
--
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

instance Text Extension where
  disp (UnknownExtension other) = Disp.text other
  disp (EnableExtension ke)     = Disp.text (show ke)
  disp (DisableExtension ke)    = Disp.text ("No" ++ show ke)

  parse = do
    extension <- Parse.munch1 Char.isAlphaNum
    return (classifyExtension extension)

instance Text KnownExtension where
  disp ke = Disp.text (show ke)

  parse = do
    extension <- Parse.munch1 Char.isAlphaNum
    case classifyKnownExtension extension of
        Just ke ->
            return ke
        Nothing ->
            fail ("Can't parse " ++ show extension ++ " as KnownExtension")

classifyExtension :: String -> Extension
classifyExtension string
  = case classifyKnownExtension string of
    Just ext -> EnableExtension ext
    Nothing ->
        case string of
        'N':'o':string' ->
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
--
classifyKnownExtension :: String -> Maybe KnownExtension
classifyKnownExtension "" = Nothing
classifyKnownExtension string@(c : _)
  | inRange (bounds knownExtensionTable) c
  = lookup string (knownExtensionTable ! c)
  | otherwise = Nothing

knownExtensionTable :: Array Char [(String, KnownExtension)]
knownExtensionTable =
  accumArray (flip (:)) [] ('A', 'Z')
    [ (head str, (str, extension))
    | extension <- [toEnum 0 ..]
    , let str = show extension ]
