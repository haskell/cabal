-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Extension
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- Haskell language dialects and extensions

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

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
  deriving (Show, Read, Eq)

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
--   (where X is each compiler: GHC, JHC, Hugs, NHC)
--
-- | This represents language extensions beyond a base 'Language' definition
-- (such as 'Haskell98') that are supported by some implementations, usually
-- in some special mode.
--
-- Where applicable, references are given to an implementation's
-- official documentation, e.g. \"GHC &#xa7; 7.2.1\" for an extension
-- documented in section 7.2.1 of the GHC User's Guide.

data Extension =
  -- | Enable a known extension
    EnableExtension KnownExtension

  -- | Disable a known extension
  | DisableExtension KnownExtension

  -- | An unknown extension, identified by the name of its @LANGUAGE@
  -- pragma.
  | UnknownExtension String

  deriving (Show, Read, Eq)

data KnownExtension =

  -- | [GHC &#xa7; 7.6.3.4] Allow overlapping class instances,
  -- provided there is a unique most specific instance for each use.
    OverlappingInstances

  -- | [GHC &#xa7; 7.6.3.3] Ignore structural rules guaranteeing the
  -- termination of class instance resolution.  Termination is
  -- guaranteed by a fixed-depth recursion stack, and compilation
  -- may fail if this depth is exceeded.
  | UndecidableInstances

  -- | [GHC &#xa7; 7.6.3.4] Implies 'OverlappingInstances'.  Allow the
  -- implementation to choose an instance even when it is possible
  -- that further instantiation of types will lead to a more specific
  -- instance being applicable.
  | IncoherentInstances

  -- | [GHC &#xa7; 7.3.8] Allows recursive bindings in @do@ blocks,
  -- using the @rec@ keyword.
  | DoRec

  -- | [GHC &#xa7; 7.3.8.2] Deprecated in GHC.  Allows recursive bindings
  -- using @mdo@, a variant of @do@.  @DoRec@ provides a different,
  -- preferred syntax.
  | RecursiveDo

  -- | [GHC &#xa7; 7.3.9] Provide syntax for writing list
  -- comprehensions which iterate over several lists together, like
  -- the 'zipWith' family of functions.
  | ParallelListComp

  -- | [GHC &#xa7; 7.6.1.1] Allow multiple parameters in a type class.
  | MultiParamTypeClasses

  -- | [GHC &#xa7; 7.17] Enable the dreaded monomorphism restriction.
  | MonomorphismRestriction

  -- | [GHC &#xa7; 7.6.2] Allow a specification attached to a
  -- multi-parameter type class which indicates that some parameters
  -- are entirely determined by others. The implementation will check
  -- that this property holds for the declared instances, and will use
  -- this property to reduce ambiguity in instance resolution.
  | FunctionalDependencies

  -- | [GHC &#xa7; 7.8.5] Like 'RankNTypes' but does not allow a
  -- higher-rank type to itself appear on the left of a function
  -- arrow.
  | Rank2Types

  -- | [GHC &#xa7; 7.8.5] Allow a universally-quantified type to occur on
  -- the left of a function arrow.
  | RankNTypes

  -- | [GHC &#xa7; 7.8.5] Allow data constructors to have polymorphic
  -- arguments.  Unlike 'RankNTypes', does not allow this for ordinary
  -- functions.
  | PolymorphicComponents

  -- | [GHC &#xa7; 7.4.4] Allow existentially-quantified data constructors.
  | ExistentialQuantification

  -- | [GHC &#xa7; 7.8.7] Cause a type variable in a signature, which has an
  -- explicit @forall@ quantifier, to scope over the definition of the
  -- accompanying value declaration.
  | ScopedTypeVariables

  -- | Deprecated, use 'ScopedTypeVariables' instead.
  | PatternSignatures

  -- | [GHC &#xa7; 7.8.3] Enable implicit function parameters with dynamic
  -- scope.
  | ImplicitParams

  -- | [GHC &#xa7; 7.8.2] Relax some restrictions on the form of the context
  -- of a type signature.
  | FlexibleContexts

  -- | [GHC &#xa7; 7.6.3.2] Relax some restrictions on the form of the
  -- context of an instance declaration.
  | FlexibleInstances

  -- | [GHC &#xa7; 7.4.1] Allow data type declarations with no constructors.
  | EmptyDataDecls

  -- | [GHC &#xa7; 4.10.3] Run the C preprocessor on Haskell source code.
  | CPP

  -- | [GHC &#xa7; 7.8.4] Allow an explicit kind signature giving the kind of
  -- types over which a type variable ranges.
  | KindSignatures

  -- | [GHC &#xa7; 7.11] Enable a form of pattern which forces evaluation
  -- before an attempted match, and a form of strict @let@/@where@
  -- binding.
  | BangPatterns

  -- | [GHC &#xa7; 7.6.3.1] Allow type synonyms in instance heads.
  | TypeSynonymInstances

  -- | [GHC &#xa7; 7.9] Enable Template Haskell, a system for compile-time
  -- metaprogramming.
  | TemplateHaskell

  -- | [GHC &#xa7; 8] Enable the Foreign Function Interface.  In GHC,
  -- implements the standard Haskell 98 Foreign Function Interface
  -- Addendum, plus some GHC-specific extensions.
  | ForeignFunctionInterface

  -- | [GHC &#xa7; 7.10] Enable arrow notation.
  | Arrows

  -- | [GHC &#xa7; 7.16] Enable generic type classes, with default instances
  -- defined in terms of the algebraic structure of a type.
  | Generics

  -- | [GHC &#xa7; 7.3.11] Enable the implicit importing of the module
  -- @Prelude@.  When disabled, when desugaring certain built-in syntax
  -- into ordinary identifiers, use whatever is in scope rather than the
  -- @Prelude@ -- version.
  | ImplicitPrelude

  -- | [GHC &#xa7; 7.3.15] Enable syntax for implicitly binding local names
  -- corresponding to the field names of a record.  Puns bind specific
  -- names, unlike 'RecordWildCards'.
  | NamedFieldPuns

  -- | [GHC &#xa7; 7.3.5] Enable a form of guard which matches a pattern and
  -- binds variables.
  | PatternGuards

  -- | [GHC &#xa7; 7.5.4] Allow a type declared with @newtype@ to use
  -- @deriving@ for any class with an instance for the underlying type.
  | GeneralizedNewtypeDeriving

  -- | [Hugs &#xa7; 7.1] Enable the \"Trex\" extensible records system.
  | ExtensibleRecords

  -- | [Hugs &#xa7; 7.2] Enable type synonyms which are transparent in
  -- some definitions and opaque elsewhere, as a way of implementing 
  -- abstract datatypes.
  | RestrictedTypeSynonyms

  -- | [Hugs &#xa7; 7.3] Enable an alternate syntax for string literals,
  -- with string templating.
  | HereDocuments

  -- | [GHC &#xa7; 7.3.2] Allow the character @#@ as a postfix modifier on
  -- identifiers.  Also enables literal syntax for unboxed values.
  | MagicHash

  -- | [GHC &#xa7; 7.7] Allow data types and type synonyms which are
  -- indexed by types, i.e. ad-hoc polymorphism for types.
  | TypeFamilies

  -- | [GHC &#xa7; 7.5.2] Allow a standalone declaration which invokes the
  -- type class @deriving@ mechanism.
  | StandaloneDeriving

  -- | [GHC &#xa7; 7.3.1] Allow certain Unicode characters to stand for
  -- certain ASCII character sequences, e.g. keywords and punctuation.
  | UnicodeSyntax

  -- | [GHC &#xa7; 8.1.1] Allow the use of unboxed types as foreign types,
  -- e.g. in @foreign import@ and @foreign export@.
  | UnliftedFFITypes

  -- | [GHC &#xa7; 7.4.3] Defer validity checking of types until after
  -- expanding type synonyms, relaxing the constraints on how synonyms
  -- may be used.
  | LiberalTypeSynonyms

  -- | [GHC &#xa7; 7.4.2] Allow the name of a type constructor, type class,
  -- or type variable to be an infix operator.
  | TypeOperators

--PArr -- not ready yet, and will probably be renamed to ParallelArrays

  -- | [GHC &#xa7; 7.3.16] Enable syntax for implicitly binding local names
  -- corresponding to the field names of a record.  A wildcard binds
  -- all unmentioned names, unlike 'NamedFieldPuns'.
  | RecordWildCards

  -- | Deprecated, use 'NamedFieldPuns' instead.
  | RecordPuns

  -- | [GHC &#xa7; 7.3.14] Allow a record field name to be disambiguated
  -- by the type of the record it's in.
  | DisambiguateRecordFields

  -- | [GHC &#xa7; 7.6.4] Enable overloading of string literals using a
  -- type class, much like integer literals.
  | OverloadedStrings

  -- | [GHC &#xa7; 7.4.6] Enable generalized algebraic data types, in
  -- which type variables may be instantiated on a per-constructor
  -- basis. Implies GADTSyntax.
  | GADTs

  -- | Enable GADT syntax for declaring ordinary algebraic datatypes.
  | GADTSyntax

  -- | [GHC &#xa7; 7.17.2] Make pattern bindings monomorphic.
  | MonoPatBinds

  -- | [GHC &#xa7; 7.8.8] Relax the requirements on mutually-recursive
  -- polymorphic functions.
  | RelaxedPolyRec

  -- | [GHC &#xa7; 2.4.5] Allow default instantiation of polymorphic
  -- types in more situations.
  | ExtendedDefaultRules

  -- | [GHC &#xa7; 7.2.2] Enable unboxed tuples.
  | UnboxedTuples

  -- | [GHC &#xa7; 7.5.3] Enable @deriving@ for classes
  -- @Data.Typeable.Typeable@ and @Data.Generics.Data@.
  | DeriveDataTypeable

  -- | [GHC &#xa7; 7.6.1.3] Allow a class method's type to place
  -- additional constraints on a class type variable.
  | ConstrainedClassMethods

  -- | [GHC &#xa7; 7.3.18] Allow imports to be qualified by the package
  -- name the module is intended to be imported from, e.g.
  --
  -- > import "network" Network.Socket
  | PackageImports

  -- | [GHC &#xa7; 7.8.6] Deprecated in GHC 6.12 and will be removed in
  -- GHC 7.  Allow a type variable to be instantiated at a
  -- polymorphic type.
  | ImpredicativeTypes

  -- | [GHC &#xa7; 7.3.3] Change the syntax for qualified infix
  -- operators.
  | NewQualifiedOperators

  -- | [GHC &#xa7; 7.3.12] Relax the interpretation of left operator
  -- sections to allow unary postfix operators.
  | PostfixOperators

  -- | [GHC &#xa7; 7.9.5] Enable quasi-quotation, a mechanism for defining
  -- new concrete syntax for expressions and patterns.
  | QuasiQuotes

  -- | [GHC &#xa7; 7.3.10] Enable generalized list comprehensions,
  -- supporting operations such as sorting and grouping.
  | TransformListComp

  -- | [GHC &#xa7; 7.3.6] Enable view patterns, which match a value by
  -- applying a function and matching on the result.
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
  | TupleSections

  -- | Allows GHC primops, written in C--, to be imported into a Haskell
  -- file.
  | GHCForeignImportPrim

  -- | Support for patterns of the form @n + k@, where @k@ is an
  -- integer literal.
  | NPlusKPatterns

  -- | Improve the layout rule when @if@ expressions are used in a @do@
  -- block.
  | DoAndIfThenElse

  -- | Makes much of the Haskell sugar be desugared into calls to the
  -- function with a particular name that is in scope.
  | RebindableSyntax

  -- | Make @forall@ a keyword in types, which can be used to give the
  -- generalisation explicitly.
  | ExplicitForAll

  -- | Allow contexts to be put on datatypes, e.g. the @Eq a@ in
  -- @data Eq a => Set a = NilSet | ConsSet a (Set a)@.
  | DatatypeContexts

  -- | Local (@let@ and @where@) bindings are monomorphic.
  | MonoLocalBinds

  -- | Enable @deriving@ for the @Data.Functor.Functor@ class.
  | DeriveFunctor

  -- | Enable @deriving@ for the @Data.Traversable.Traversable@ class.
  | DeriveTraversable

  -- | Enable @deriving@ for the @Data.Foldable.Foldable@ class.
  | DeriveFoldable

  -- | Enable non-decreasing indentation for 'do' blocks.
  | NondecreasingIndentation

  -- | [GHC &#xa7; 7.20.3] Allow imports to be qualified with a safe
  -- keyword that requires the imported module be trusted as according
  -- to the Safe Haskell definition of trust.
  --
  -- > import safe Network.Socket
  | SafeImports

  -- | [GHC &#xa7; 7.20] Compile a module in the Safe, Safe Haskell
  -- mode -- a restricted form of the Haskell language to ensure
  -- type safety.
  | Safe

  -- | [GHC &#xa7; 7.20] Compile a module in the Trustworthy, Safe
  -- Haskell mode -- no restrictions apply but the module is marked
  -- as trusted as long as the package the module resides in is
  -- trusted.
  | Trustworthy

  -- | [GHC &#xa7; 7.40] Allow type class/implicit parameter/equality
  -- constraints to be used as types with the special kind Constraint.
  -- Also generalise the (ctxt => ty) syntax so that any type of kind
  -- Constraint can occur before the arrow.
  | ConstraintKind

  deriving (Show, Read, Eq, Enum, Bounded)

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
-- valid. We must not recomend aliases that cannot be used with older
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

