.. _ghc-syntax:

Language Extensions
===================

Package Language Fields
-----------------------

These are cabal package build info fields that control language.

.. _ghc-default-extensions:

default-extensions
    * Monoidal field
    * Available since ``cabal-version: 1.10``.
    * Documentation of :pkg-field:`library:default-extensions`

    .. math::

        \mathrm{optcommalist}\left\{ \mathop{\mathit{enable\text{-}extension}}\mid\mathop{\mathit{disable\text{-}extension}} \right\}

.. _ghc-default-language:

default-language
    * Optional field
    * Available since ``cabal-version: 1.10``.
    * Documentation of :pkg-field:`library:default-language`

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{GHC2021}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Haskell2010}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Haskell98}\mathord{"}} \right\}

.. _ghc-extensions:

extensions
    * Monoidal field
    * Deprecated since ``cabal-version: 1.12``: Please use 'default-extensions' or 'other-extensions' fields.
    * Removed in ``cabal-version: 3.0``: Please use 'default-extensions' or 'other-extensions' fields.

    .. math::

        \mathrm{optcommalist}\left\{ \mathop{\mathit{enable\text{-}extension}}\mid\mathop{\mathit{disable\text{-}extension}} \right\}

.. _ghc-other-extensions:

other-extensions
    * Monoidal field
    * Documentation of :pkg-field:`library:other-extensions`

    .. math::

        \mathrm{optcommalist}\left\{ \mathop{\mathit{enable\text{-}extension}}\mid\mathop{\mathit{disable\text{-}extension}} \right\}

.. _ghc-other-languages:

other-languages
    * Monoidal field
    * Available since ``cabal-version: 1.10``.
    * Documentation of :pkg-field:`library:other-languages`

    .. math::

        \mathrm{optcommalist}\left\{ \mathop{\mathord{``}\mathtt{GHC2021}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Haskell2010}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Haskell98}\mathord{"}} \right\}


Langage Extension Groups
------------------------

Language extensions groups shown here correspond to subsections of the GHC
users' guide on language extensions.

.. Note::

    The Cabal package grammar accepts any tokens for extension fields. The
    extensions specified may be anything, something which a particular Cabal
    version doesn't know about and this list of "known" extensions is not part
    of the ``.cabal`` file specification and shown here only as a convenience.
    The GHC users' guide is the place to look these up.

.. _ghc-disable-extension:

disable-extension
    Disable a language extension by prepending the extension with "No".

    .. math::

        \mathop{\mathord{``}\mathtt{No}\mathord{"}}\mathop{\mathit{enable\text{-}extension}}

.. _ghc-enable-extension:

enable-extension
    All GHC language extensions known to cabal. There may be more and some of these may be on by default.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathit{interactive\text{-}extension}}\\\mathop{\mathit{phase\text{-}extension}}\\\mathop{\mathit{syntax\text{-}extension}}\\\mathop{\mathit{import\text{-}export\text{-}extension}}\\\mathop{\mathit{type\text{-}extension}}\\\mathop{\mathit{record\text{-}extension}}\\\mathop{\mathit{deriving\text{-}extension}}\\\mathop{\mathit{pattern\text{-}extension}}\\\mathop{\mathit{classes\text{-}instances\text{-}extension}}\\\mathop{\mathit{literals\text{-}extension}}\\\mathop{\mathit{constraint\text{-}extension}}\\\mathop{\mathit{type\text{-}signature\text{-}extension}}\\\mathop{\mathit{binding\text{-}generalisation\text{-}extension}}\\\mathop{\mathit{template\text{-}haskell\text{-}extension}}\\\mathop{\mathit{bang\text{-}strict\text{-}extension}}\\\mathop{\mathit{parallel\text{-}concurrent\text{-}extension}}\\\mathop{\mathit{unboxed\text{-}primitive\text{-}extension}}\\\mathop{\mathit{foreign\text{-}extension}}\\\mathop{\mathit{safe\text{-}extension}}\\\mathop{\mathit{miscellaneous\text{-}extension}}\\\mathop{\mathit{bugs\text{-}extension}}\\\mathop{\mathit{ungrouped\text{-}extension}}\end{gathered} \right\}

.. _ghc-interactive-extension:

interactive-extension
    Language Extensions related to GHC interactive.

    .. math::

        \mathop{\mathord{``}\mathtt{ExtendedDefaultRules}\mathord{"}}

.. _ghc-phase-extension:

phase-extension
    Language Extensions related to a particular GHC phase.

    .. math::

        \mathop{\mathord{``}\mathtt{CPP}\mathord{"}}

.. _ghc-syntax-extension:

syntax-extension
    Syntax Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{UnicodeSyntax}\mathord{"}}\\\mathop{\mathord{``}\mathtt{MagicHash}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RecursiveDo}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ApplicativeDo}\mathord{"}}\\\mathop{\mathord{``}\mathtt{QualifiedDo}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ParallelListComp}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TransformListComp}\mathord{"}}\\\mathop{\mathord{``}\mathtt{MonadComprehensions}\mathord{"}}\\\mathop{\mathord{``}\mathtt{OverloadedLists}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ImplicitPrelude}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RebindableSyntax}\mathord{"}}\\\mathop{\mathord{``}\mathtt{PostfixOperators}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TupleSections}\mathord{"}}\\\mathop{\mathord{``}\mathtt{LambdaCase}\mathord{"}}\\\mathop{\mathord{``}\mathtt{EmptyCase}\mathord{"}}\\\mathop{\mathord{``}\mathtt{MultiWayIf}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Arrows}\mathord{"}}\\\mathop{\mathord{``}\mathtt{LexicalNegation}\mathord{"}}\\\mathop{\mathord{``}\mathtt{BlockArguments}\mathord{"}}\end{gathered} \right\}

.. _ghc-import-export-extension:

import-export-extension
    Import and Export Language Extensions.

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{PackageImports}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{ExplicitNamespaces}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{ImportQualifiedPost}\mathord{"}} \right\}

.. _ghc-type-extension:

type-extension
    Language Extensions for Types.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{EmptyDataDecls}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DatatypeContexts}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeOperators}\mathord{"}}\\\mathop{\mathord{``}\mathtt{LiberalTypeSynonyms}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ExistentialQuantification}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GADTSyntax}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GADTs}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeFamilies}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeFamilyDependencies}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DataKinds}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeData}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeInType}\mathord{"}}\\\mathop{\mathord{``}\mathtt{PolyKinds}\mathord{"}}\\\mathop{\mathord{``}\mathtt{CUSKs}\mathord{"}}\\\mathop{\mathord{``}\mathtt{StandaloneKindSignatures}\mathord{"}}\\\mathop{\mathord{``}\mathtt{StarIsType}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeApplications}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeAbstractions}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RequiredTypeArguments}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RankNTypes}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Rank2Types}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeepSubsumption}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ImpredicativeTypes}\mathord{"}}\\\mathop{\mathord{``}\mathtt{LinearTypes}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RoleAnnotations}\mathord{"}}\end{gathered} \right\}

.. _ghc-record-extension:

record-extension
    Record Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{TraditionalRecordSyntax}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DisambiguateRecordFields}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DuplicateRecordFields}\mathord{"}}\\\mathop{\mathord{``}\mathtt{FieldSelectors}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NamedFieldPuns}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RecordWildCards}\mathord{"}}\\\mathop{\mathord{``}\mathtt{OverloadedRecordDot}\mathord{"}}\\\mathop{\mathord{``}\mathtt{OverloadedRecordUpdate}\mathord{"}}\end{gathered} \right\}

.. _ghc-deriving-extension:

deriving-extension
    Language Extensions for deriving mechanisms.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{EmptyDataDeriving}\mathord{"}}\\\mathop{\mathord{``}\mathtt{StandaloneDeriving}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeriveFoldable}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeriveFunctor}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeriveTraversable}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeriveDataTypeable}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeriveLift}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GeneralizedNewtypeDeriving}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GeneralisedNewtypeDeriving}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DeriveAnyClass}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DerivingStrategies}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DerivingVia}\mathord{"}}\end{gathered} \right\}

.. _ghc-pattern-extension:

pattern-extension
    Patterns Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{PatternGuards}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ViewPatterns}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NPlusKPatterns}\mathord{"}}\\\mathop{\mathord{``}\mathtt{PatternSynonyms}\mathord{"}}\end{gathered} \right\}

.. _ghc-classes-instances-extension:

classes-instances-extension
    Language Extensions for class and instance declarations.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{MultiParamTypeClasses}\mathord{"}}\\\mathop{\mathord{``}\mathtt{UndecidableSuperClasses}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ConstrainedClassMethods}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DefaultSignatures}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NullaryTypeClasses}\mathord{"}}\\\mathop{\mathord{``}\mathtt{FunctionalDependencies}\mathord{"}}\\\mathop{\mathord{``}\mathtt{TypeSynonymInstances}\mathord{"}}\\\mathop{\mathord{``}\mathtt{FlexibleInstances}\mathord{"}}\\\mathop{\mathord{``}\mathtt{UndecidableInstances}\mathord{"}}\\\mathop{\mathord{``}\mathtt{OverlappingInstances}\mathord{"}}\\\mathop{\mathord{``}\mathtt{IncoherentInstances}\mathord{"}}\\\mathop{\mathord{``}\mathtt{InstanceSigs}\mathord{"}}\end{gathered} \right\}

.. _ghc-literal-extension:

literal-extension
    Literals Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{NegativeLiterals}\mathord{"}}\\\mathop{\mathord{``}\mathtt{BinaryLiterals}\mathord{"}}\\\mathop{\mathord{``}\mathtt{HexFloatLiterals}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NumDecimals}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ExtendedLiterals}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NumericUnderscores}\mathord{"}}\\\mathop{\mathord{``}\mathtt{OverloadedStrings}\mathord{"}}\\\mathop{\mathord{``}\mathtt{OverloadedLabels}\mathord{"}}\end{gathered} \right\}

.. _ghc-constraint-extension:

constraint-extension
    Constraint Language Extensions.

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{FlexibleContexts}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{ConstraintKinds}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{QuantifiedConstraints}\mathord{"}} \right\}

.. _ghc-type-signature-extension:

type-signature-extension
    Type Signature Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{ExplicitForAll}\mathord{"}}\\\mathop{\mathord{``}\mathtt{AllowAmbiguousTypes}\mathord{"}}\\\mathop{\mathord{``}\mathtt{KindSignatures}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ScopedTypeVariables}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ImplicitParams}\mathord{"}}\\\mathop{\mathord{``}\mathtt{PartialTypeSignatures}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NamedWildCards}\mathord{"}}\end{gathered} \right\}

.. _ghc-binding-generalisation-extension:

binding-generalisation-extension
    Language Extensions for bindings and generalisation 

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{MonomorphismRestriction}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{MonoLocalBinds}\mathord{"}} \right\}

.. _ghc-template-haskell-extension:

template-haskell-extension
    Template Haskell Language Extensions.

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{TemplateHaskell}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{TemplateHaskellQuotes}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{QuasiQuotes}\mathord{"}} \right\}

.. _ghc-bang-strict-extension:

bang-strict-extension
    Bang pattern and Strict Haskell Language Extensions.

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{BangPatterns}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{StrictData}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Strict}\mathord{"}} \right\}

.. _ghc-parallel-concurrent-extension:

parallel-concurrent-extension
    Parallel and Concurrent Language Extensions.

    .. math::

        \mathop{\mathord{``}\mathtt{StaticPointers}\mathord{"}}

.. _ghc-unboxed-primitive-extension:

unboxed-primitive-extension
    Unboxed types and Primitive operations Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{UnboxedTuples}\mathord{"}}\\\mathop{\mathord{``}\mathtt{UnboxedSums}\mathord{"}}\\\mathop{\mathord{``}\mathtt{UnliftedNewtypes}\mathord{"}}\\\mathop{\mathord{``}\mathtt{UnliftedDatatypes}\mathord{"}}\end{gathered} \right\}

.. _ghc-foreign-extension:

foreign-extension
    Foreign function interface (FFI) Language Extensions.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{ForeignFunctionInterface}\mathord{"}}\\\mathop{\mathord{``}\mathtt{UnliftedFFITypes}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GHCForeignImportPrim}\mathord{"}}\\\mathop{\mathord{``}\mathtt{InterruptibleFFI}\mathord{"}}\\\mathop{\mathord{``}\mathtt{CApiFFI}\mathord{"}}\end{gathered} \right\}

.. _ghc-safe-extension:

safe-extension
    Safe Haskell Language Extensions.

    .. math::

        \left\{ \mathop{\mathord{``}\mathtt{Safe}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Trustworthy}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Unsafe}\mathord{"}} \right\}

.. _ghc-miscellaneous-extension:

miscellaneous-extension
    Miscellaneous Language Extensions.

    .. math::

        \mathop{\mathord{``}\mathtt{DeriveGeneric}\mathord{"}}

.. _ghc-bugs-extension:

bugs-extension
    Language Extensions related to GHC bugs and infelicities.

    .. math::

        \mathop{\mathord{``}\mathtt{NondecreasingIndentation}\mathord{"}}

.. _ghc-ungrouped-extension:

ungrouped-extension
    Language Extensions not belonging to other extension groups.

    .. math::

        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{DoRec}\mathord{"}}\\\mathop{\mathord{``}\mathtt{PolymorphicComponents}\mathord{"}}\\\mathop{\mathord{``}\mathtt{PatternSignatures}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Generics}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ExtensibleRecords}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RestrictedTypeSynonyms}\mathord{"}}\\\mathop{\mathord{``}\mathtt{HereDocuments}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RecordPuns}\mathord{"}}\\\mathop{\mathord{``}\mathtt{MonoPatBinds}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RelaxedPolyRec}\mathord{"}}\\\mathop{\mathord{``}\mathtt{NewQualifiedOperators}\mathord{"}}\\\mathop{\mathord{``}\mathtt{XmlSyntax}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RegularPatterns}\mathord{"}}\\\mathop{\mathord{``}\mathtt{DoAndIfThenElse}\mathord{"}}\\\mathop{\mathord{``}\mathtt{SafeImports}\mathord{"}}\\\mathop{\mathord{``}\mathtt{ParallelArrays}\mathord{"}}\\\mathop{\mathord{``}\mathtt{AutoDeriveTypeable}\mathord{"}}\\\mathop{\mathord{``}\mathtt{JavaScriptFFI}\mathord{"}}\\\mathop{\mathord{``}\mathtt{MonadFailDesugaring}\mathord{"}}\\\mathop{\mathord{``}\mathtt{AlternativeLayoutRule}\mathord{"}}\\\mathop{\mathord{``}\mathtt{AlternativeLayoutRuleTransitional}\mathord{"}}\\\mathop{\mathord{``}\mathtt{RelaxedLayout}\mathord{"}}\end{gathered} \right\}


.. Warning::

    Extensions of the :ref:`ungrouped-extension <ghc-ungrouped-extension>` group
    are undocumented in the GHC users' guide.

