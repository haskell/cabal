{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Distribution.Described.Extension
  ( reEnableExtension
  , reKnownExtension
  , reDisableExtension
  , reXs
      -- * Extension groups
  , xGroupInteractive
  , xGroupPhase
  , xGroupSyntax
  , xGroupImportExport
  , xGroupTypes
  , xGroupRecords
  , xGroupDeriving
  , xGroupPatterns
  , xGroupClassesInstances
  , xGroupLiterals
  , xGroupConstraints
  , xGroupTypeSignatures
  , xGroupBindingsGeneralisation
  , xGroupTemplates
  , xGroupBangStrict
  , xGroupParallelConcurrent
  , xGroupUnboxedPrimitive
  , xGroupForeign
  , xGroupSafe
  , xGroupMiscellaneous
  , xGroupBugs
  , xUngrouped
  ) where

import Data.List ((\\))
import Data.String (IsString (..))
import Distribution.Pretty (prettyShow)
import Distribution.Described
import Language.Haskell.Extension

instance Described Extension where
    describe _ = REUnion
        [ RENamed "enable-extension" reKnownExtension
        , RENamed "disable-extension" reDisableExtension
        ]

reXs :: [KnownExtension] -> GrammarRegex a
reXs xs = REUnion (fromString . prettyShow <$> xs)

reEnableExtension :: GrammarRegex a
reEnableExtension = "enable-extension"

reKnownExtension :: GrammarRegex a
reKnownExtension = REUnion
    [ RENamed "interactive-extension" $ reXs xGroupInteractive
    , RENamed "phase-extension" $ reXs xGroupPhase
    , RENamed "syntax-extension" $ reXs xGroupSyntax
    , RENamed "import-export-extension" $ reXs xGroupImportExport
    , RENamed "type-extension" $ reXs xGroupTypes
    , RENamed "record-extension" $ reXs xGroupRecords
    , RENamed "deriving-extension" $ reXs xGroupDeriving
    , RENamed "pattern-extension" $ reXs xGroupPatterns
    , RENamed "classes-instances-extension" $ reXs xGroupClassesInstances
    , RENamed "literals-extension" $ reXs xGroupLiterals
    , RENamed "constraint-extension" $ reXs xGroupConstraints
    , RENamed "type-signature-extension" $ reXs xGroupTypeSignatures
    , RENamed "binding-generalisation-extension" $ reXs xGroupBindingsGeneralisation
    , RENamed "template-haskell-extension" $ reXs xGroupTemplates
    , RENamed "bang-strict-extension" $ reXs xGroupBangStrict
    , RENamed "parallel-concurrent-extension" $ reXs xGroupParallelConcurrent
    , RENamed "unboxed-primitive-extension" $ reXs xGroupUnboxedPrimitive
    , RENamed "foreign-extension" $ reXs xGroupForeign
    , RENamed "safe-extension" $ reXs xGroupSafe
    , RENamed "miscellaneous-extension" $ reXs xGroupMiscellaneous
    , RENamed "bugs-extension" $ reXs xGroupBugs
    , RENamed "ungrouped-extension" $ reXs xUngrouped
    ]

reDisableExtension :: GrammarRegex a
reDisableExtension = REUnion ["No" <> RENamed "enable-extension" reKnownExtension]

-- The comments in the xGroup* lists are taken from the GHC User's Guide. Stop
-- the formatter from rearranging them with:
{- FOURMOLU_DISABLE -}
xGroupInteractive :: [KnownExtension]
xGroupInteractive =
  [
  -- Type defaulting in GHCi
    ExtendedDefaultRules
  ]

xGroupPhase :: [KnownExtension]
xGroupPhase = [CPP]

xGroupSyntax :: [KnownExtension]
xGroupSyntax =
  [
  -- Unicode syntax
    UnicodeSyntax

  -- The magic hash
  , MagicHash

  -- The recursive do-notation
  , RecursiveDo

   -- Applicative do-notation
  , ApplicativeDo

  -- Qualified do-notation
  , QualifiedDo

  -- Parallel List Comprehensions
  , ParallelListComp

  -- Generalized (SQL-like) List comprehensions
  , TransformListComp

  -- Monad comprehensions
  , MonadComprehensions

  -- Overloaded lists
  , OverloadedLists

  -- Rebindable syntax and the implicit Prelude import
  , ImplicitPrelude
  , RebindableSyntax -- implies NoImplicitPrelude

  -- Postfix operators
  , PostfixOperators

  -- Tuple sections
  , TupleSections

  -- Lambda-case
  , LambdaCase

  -- Empty case
  , EmptyCase

  -- Multi-way if-expressions
  , MultiWayIf

  -- Arrow notation
  , Arrows

  -- Lexical negation
  , LexicalNegation

  -- More liberal syntax for function arguments
  , BlockArguments
  ]

xGroupImportExport :: [KnownExtension]
xGroupImportExport =
  [
  -- Package-qualified imports
    PackageImports

  -- Explicit namespaces in import/export
  , ExplicitNamespaces

  -- Writing qualified in postpositive position
  , ImportQualifiedPost
  ]

xGroupTypes :: [KnownExtension]
xGroupTypes =
  [
  -- Data types with no constructors
    EmptyDataDecls

  -- Data type contexts
  , DatatypeContexts -- deprecated

  -- Type operators
  , TypeOperators

  -- Liberalised type synonyms
  , LiberalTypeSynonyms -- implies ExplicitForAll

  -- Existentially quantified data constructors
  , ExistentialQuantification -- implies ExplicitForAll

  -- Declaring data types with explicit constructor signatures
  , GADTSyntax -- implied by GADTs

  -- Generalised algebraic data types (GADTs)
  , GADTs -- implies MonoLocalBinds, GADTSyntax

  -- Type families
  , TypeFamilies -- implies MonoLocalBinds, KindSignatures, ExplicitNamespaces

  -- Injective type families
  , TypeFamilyDependencies -- implies TypeFamilies

  -- Datatype promotion
  , DataKinds

  -- Type-level data declarations
  , TypeData

  -- Kind polymorphism
  , TypeInType -- implies PolyKinds, DataKinds, KindSignatures
  , PolyKinds -- implies KindSignatures
  , CUSKs -- legacy feature replaced by StandaloneKindSignatures
  , StandaloneKindSignatures -- implies NoCUSKs
  , StarIsType

  -- Visible type application
  , TypeApplications

  -- Type abstractions
  , TypeAbstractions

  -- Required type arguments
  , RequiredTypeArguments

  -- Arbitrary-rank polymorphism
  , RankNTypes -- implies ExplicitForAll
  , Rank2Types -- deprecated alias of RankNTypes

  -- Subsumption
  , DeepSubsumption

  -- Impredicative polymorphism
  , ImpredicativeTypes -- implies RankNTypes

  -- Linear types
  , LinearTypes -- implies MonoLocalBinds

  -- Role annotations
  , RoleAnnotations
  ]

xGroupRecords :: [KnownExtension]
xGroupRecords =
  [
  -- Traditional record syntax
    TraditionalRecordSyntax

  -- Record field disambiguation
  , DisambiguateRecordFields

  -- Duplicate record fields
  , DuplicateRecordFields

  -- Field selectors
  , FieldSelectors

  -- Record puns
  , NamedFieldPuns

  -- Record wildcards
  , RecordWildCards

  -- Overloaded record dot
  , OverloadedRecordDot

  -- Overloaded record update
  , OverloadedRecordUpdate
  ]

xGroupDeriving :: [KnownExtension]
xGroupDeriving =
  [ EmptyDataDeriving, StandaloneDeriving
  , DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveDataTypeable, DeriveLift
  , GeneralizedNewtypeDeriving, GeneralisedNewtypeDeriving
  , DeriveAnyClass
  , DerivingStrategies
  , DerivingVia
  ]

xGroupPatterns :: [KnownExtension]
xGroupPatterns =
  [
  -- Pattern guards
    PatternGuards

  -- View patterns
  , ViewPatterns

  -- n+k patterns
  , NPlusKPatterns

  -- Pattern synonyms
  , PatternSynonyms
  ]

xGroupClassesInstances :: [KnownExtension]
xGroupClassesInstances =
  [
  -- Multi-parameter type classes
    MultiParamTypeClasses -- implies ConstrainedClassMethods, implied by FunctionalDependencies

  -- Undecidable (or recursive) superclasses
  , UndecidableSuperClasses

  -- Constrained class method types
  , ConstrainedClassMethods -- implied by MultiParamTypeClasses

  -- Default method signatures
  , DefaultSignatures

  -- Nullary type classes
  , NullaryTypeClasses -- deprecated, replaced by MultiParamTypeClasses

  -- Functional dependencies
  , FunctionalDependencies -- implies MultiParamTypeClasses

  --Relaxed rules for the instance head
  , TypeSynonymInstances -- implied by FlexibleInstances
  , FlexibleInstances -- implies TypeSynonymInstances

  -- Undecided instances and loopy superclasses
  , UndecidableInstances

  -- Overlapping instances
  , OverlappingInstances -- deprecated
  , IncoherentInstances -- deprecated

  -- Instance signatures: type signatures in instance declarations
  , InstanceSigs
  ]

xGroupLiterals :: [KnownExtension]
xGroupLiterals =
  [
  -- Negative literals
    NegativeLiterals

  -- Bindary integer literals
  , BinaryLiterals

  -- Hexadecimal floating point literals
  , HexFloatLiterals

  -- Fractional looking integer literals
  , NumDecimals

  -- Sized primitive literal syntax
  , ExtendedLiterals

  -- Numeric underscores
  , NumericUnderscores

  -- Overloaded string literals
  , OverloadedStrings

  -- Overloaded labels 
  , OverloadedLabels
  ]

xGroupConstraints :: [KnownExtension]
xGroupConstraints =
  [
  -- Loosening restrictions on class contexts
    FlexibleContexts

  -- The Constraint kind
  , ConstraintKinds

  -- Quantified constraints
  , QuantifiedConstraints -- implies ExplicitForAll
  ]

xGroupTypeSignatures :: [KnownExtension]
xGroupTypeSignatures =
  [
  -- Explicit universal quantification (forall)
    ExplicitForAll

  -- Ambiguous types and the ambiguity check
  , AllowAmbiguousTypes

  -- Explicitly-kinded quantification
  , KindSignatures -- implied by TypeFamilies, PolyKinds

  -- Lexically scoped type variables
  , ScopedTypeVariables -- implies ExplicitForAll

  -- Implicit parameters
  , ImplicitParams

  -- Partial Type Signatures
  , PartialTypeSignatures

  -- Named Wildcards
  , NamedWildCards
  ]

xGroupBindingsGeneralisation :: [KnownExtension]
xGroupBindingsGeneralisation =
  [
  -- Switching off the monomorphism restriction
    MonomorphismRestriction

  -- Let-generalisation
  , MonoLocalBinds -- implied by TypeFamilies, GADTs
  ]

xGroupTemplates :: [KnownExtension]
xGroupTemplates =
  [
  -- Template Haskell
    TemplateHaskell -- implies TemplateHaskellQuotes
  , TemplateHaskellQuotes

  -- Template Haskell Quasi-quotation
  , QuasiQuotes
  ]

xGroupBangStrict :: [KnownExtension]
xGroupBangStrict =
  [
  -- Bang patterns
    BangPatterns

  -- Strict-by-default data types
  , StrictData

  -- Strict-by-default pattern bindings
  , Strict -- implies StrictData
  ]

-- | Concurrent Haskell is enabled by default
xGroupParallelConcurrent :: [KnownExtension]
xGroupParallelConcurrent =
  [
  -- Static pointers
    StaticPointers
  ]

xGroupUnboxedPrimitive :: [KnownExtension]
xGroupUnboxedPrimitive =
  [
  -- Unboxed tuples
    UnboxedTuples -- implies UnboxedSums

  -- Unboxed sums
  , UnboxedSums -- implied by UnboxedTuples

  -- Unlifted Newtypes
  , UnliftedNewtypes

  -- Unlifted Datatypes
  , UnliftedDatatypes -- implies DataKinds, StandaloneKindSignatures
  ]

xGroupForeign :: [KnownExtension]
xGroupForeign =
  [
  -- Foreign function interface (FFI)
    ForeignFunctionInterface

  -- Unlifted FFI Types
  , UnliftedFFITypes

  -- Primitive imports
  , GHCForeignImportPrim

  -- Interruptible foreign calls
  , InterruptibleFFI

  -- The CAPI calling convention
  , CApiFFI
  ]

xGroupSafe :: [KnownExtension]
xGroupSafe = [Safe, Trustworthy, Unsafe]

xGroupMiscellaneous :: [KnownExtension]
xGroupMiscellaneous =
  [
  -- Deriving representations
    DeriveGeneric
  ]

xGroupBugs :: [KnownExtension]
xGroupBugs =
  [
  -- Context-free syntax
    NondecreasingIndentation
  ]

-- | Extensions that are not in other groups, likley undocumented.
xUngrouped :: [KnownExtension]
xUngrouped =
  (((((((((((((((((((((knownExtensions
  \\ xGroupInteractive)
  \\ xGroupPhase)
  \\ xGroupSyntax)
  \\ xGroupImportExport)
  \\ xGroupTypes)
  \\ xGroupRecords)
  \\ xGroupDeriving)
  \\ xGroupPatterns)
  \\ xGroupClassesInstances)
  \\ xGroupLiterals)
  \\ xGroupConstraints)
  \\ xGroupTypeSignatures)
  \\ xGroupBindingsGeneralisation)
  \\ xGroupTemplates)
  \\ xGroupBangStrict)
  \\ xGroupParallelConcurrent)
  \\ xGroupUnboxedPrimitive)
  \\ xGroupForeign)
  \\ xGroupSafe)
  \\ xGroupMiscellaneous)
  \\ xGroupBugs)