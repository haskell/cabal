" Vim syntax file
" Language:	Haskell Cabal Build file
" Maintainer:	Cabal Development team <cabal-devel@haskell.org>
"
" This file is written so syntaxbased omnicompletion
"
" set omnifunc=syntaxcomplete#Complete
"
" is as useful as it could be.
"
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn keyword cabalCategory	executable
syn keyword cabalCategory	library
syn keyword cabalCategory	foreign-library
syn keyword cabalCategory	benchmark
syn keyword cabalCategory	test-suite
syn keyword cabalCategory	source-repository
syn keyword cabalCategory	flag
syn keyword cabalCategory	custom-setup

syn keyword     cabalConditional    if else elif
syn match       cabalOperator       "&&\|||\|!"
syn keyword     cabalFunction       os arch impl flag
syn match       cabalComment        /--.*$/
syn match       cabalVersion        "\(==\|>=\|<=\|<\|>\|\^>=\)\s*\d\+\(\.\(\d\)\+\)*\(\.\*\)\?"

syn keyword     cabalTruth      True
syn keyword     cabalTruth      False

syn match       cabalCompiler   "\c\<ghc\>"
syn match       cabalCompiler   "\c\<ghcjs\>"
syn match       cabalCompiler   "\c\<nhc\>"
syn match       cabalCompiler   "\c\<yhc\>"
syn match       cabalCompiler   "\c\<hugs\>"
syn match       cabalCompiler   "\c\<hbc\>"
syn match       cabalCompiler   "\c\<helium\>"
syn match       cabalCompiler   "\c\<jhc\>"
syn match       cabalCompiler   "\c\<lhc\>"

syn keyword cabalOs linux osx windows

syn match	cabalXStatement	/^\c\s*\<x-[a-z_\-]\+\s*:/me=e-1

syn keyword cabalLanguage Haskell2010 Haskell98
syn keyword cabalType     exitcode-stdio-1.0

" Regenerate by firing a repl: cabal repl Cabal
" :m *Distribution.PackageDescription.FieldGrammar
" _syntaxFieldNames
" _syntaxExtensions
"
" For field names we take opinionated approach and treat
" the colon as the part of the keyword.
" This makes completion to add it automatically.
"
syn keyword cabalStatement asm-options:
syn keyword cabalStatement asm-sources:
syn keyword cabalStatement author:
syn keyword cabalStatement autogen-includes:
syn keyword cabalStatement autogen-modules:
syn keyword cabalStatement benchmark-module:
syn keyword cabalStatement branch:
syn keyword cabalStatement bug-reports:
syn keyword cabalStatement build-depends:
syn keyword cabalStatement build-tool-depends:
syn keyword cabalStatement build-tools:
syn keyword cabalStatement build-type:
syn keyword cabalStatement buildable:
syn keyword cabalStatement c-sources:
syn keyword cabalStatement cabal-version:
syn keyword cabalStatement category:
syn keyword cabalStatement cc-options:
syn keyword cabalStatement cmm-options:
syn keyword cabalStatement cmm-sources:
syn keyword cabalStatement copyright:
syn keyword cabalStatement cpp-options:
syn keyword cabalStatement cxx-options:
syn keyword cabalStatement cxx-sources:
syn keyword cabalStatement data-dir:
syn keyword cabalStatement data-files:
syn keyword cabalStatement default:
syn keyword cabalStatement default-extensions:
syn keyword cabalStatement default-language:
syn keyword cabalStatement description:
syn keyword cabalStatement exposed:
syn keyword cabalStatement exposed-modules:
syn keyword cabalStatement extensions:
syn keyword cabalStatement extra-bundled-libraries:
syn keyword cabalStatement extra-doc-files:
syn keyword cabalStatement extra-dynamic-library-flavours:
syn keyword cabalStatement extra-framework-dirs:
syn keyword cabalStatement extra-ghci-libraries:
syn keyword cabalStatement extra-lib-dirs:
syn keyword cabalStatement extra-libraries:
syn keyword cabalStatement extra-library-flavours:
syn keyword cabalStatement extra-source-files:
syn keyword cabalStatement extra-tmp-files:
syn keyword cabalStatement frameworks:
syn keyword cabalStatement ghc-options:
syn keyword cabalStatement ghc-prof-options:
syn keyword cabalStatement ghc-shared-options:
syn keyword cabalStatement ghcjs-options:
syn keyword cabalStatement ghcjs-prof-options:
syn keyword cabalStatement ghcjs-shared-options:
syn keyword cabalStatement homepage:
syn keyword cabalStatement hs-source-dir:
syn keyword cabalStatement hs-source-dirs:
syn keyword cabalStatement hugs-options:
syn keyword cabalStatement include-dirs:
syn keyword cabalStatement includes:
syn keyword cabalStatement install-includes:
syn keyword cabalStatement jhc-options:
syn keyword cabalStatement js-sources:
syn keyword cabalStatement ld-options:
syn keyword cabalStatement lib-version-info:
syn keyword cabalStatement lib-version-linux:
syn keyword cabalStatement license:
syn keyword cabalStatement license-file:
syn keyword cabalStatement license-files:
syn keyword cabalStatement location:
syn keyword cabalStatement main-is:
syn keyword cabalStatement maintainer:
syn keyword cabalStatement manual:
syn keyword cabalStatement mixins:
syn keyword cabalStatement mod-def-file:
syn keyword cabalStatement module:
syn keyword cabalStatement name:
syn keyword cabalStatement nhc98-options:
syn keyword cabalStatement options:
syn keyword cabalStatement other-extensions:
syn keyword cabalStatement other-languages:
syn keyword cabalStatement other-modules:
syn keyword cabalStatement package-url:
syn keyword cabalStatement pkgconfig-depends:
syn keyword cabalStatement reexported-modules:
syn keyword cabalStatement scope:
syn keyword cabalStatement setup-depends:
syn keyword cabalStatement signatures:
syn keyword cabalStatement stability:
syn keyword cabalStatement subdir:
syn keyword cabalStatement synopsis:
syn keyword cabalStatement tag:
syn keyword cabalStatement test-module:
syn keyword cabalStatement tested-with:
syn keyword cabalStatement type:
syn keyword cabalStatement version:
syn keyword cabalStatement virtual-modules:

syn keyword cabalExtension AllowAmbiguousTypes
syn keyword cabalExtension ApplicativeDo
syn keyword cabalExtension Arrows
syn keyword cabalExtension AutoDeriveTypeable
syn keyword cabalExtension BangPatterns
syn keyword cabalExtension BinaryLiterals
syn keyword cabalExtension BlockArguments
syn keyword cabalExtension CApiFFI
syn keyword cabalExtension CPP
syn keyword cabalExtension CUSKs
syn keyword cabalExtension ConstrainedClassMethods
syn keyword cabalExtension ConstraintKinds
syn keyword cabalExtension DataKinds
syn keyword cabalExtension DatatypeContexts
syn keyword cabalExtension DefaultSignatures
syn keyword cabalExtension DeriveAnyClass
syn keyword cabalExtension DeriveDataTypeable
syn keyword cabalExtension DeriveFoldable
syn keyword cabalExtension DeriveFunctor
syn keyword cabalExtension DeriveGeneric
syn keyword cabalExtension DeriveLift
syn keyword cabalExtension DeriveTraversable
syn keyword cabalExtension DerivingStrategies
syn keyword cabalExtension DerivingVia
syn keyword cabalExtension DisambiguateRecordFields
syn keyword cabalExtension DoAndIfThenElse
syn keyword cabalExtension DoRec
syn keyword cabalExtension DuplicateRecordFields
syn keyword cabalExtension EmptyCase
syn keyword cabalExtension EmptyDataDecls
syn keyword cabalExtension EmptyDataDeriving
syn keyword cabalExtension ExistentialQuantification
syn keyword cabalExtension ExplicitForAll
syn keyword cabalExtension ExplicitNamespaces
syn keyword cabalExtension ExtendedDefaultRules
syn keyword cabalExtension ExtensibleRecords
syn keyword cabalExtension FlexibleContexts
syn keyword cabalExtension FlexibleInstances
syn keyword cabalExtension ForeignFunctionInterface
syn keyword cabalExtension FunctionalDependencies
syn keyword cabalExtension GADTSyntax
syn keyword cabalExtension GADTs
syn keyword cabalExtension GHCForeignImportPrim
syn keyword cabalExtension GeneralisedNewtypeDeriving
syn keyword cabalExtension GeneralizedNewtypeDeriving
syn keyword cabalExtension Generics
syn keyword cabalExtension HereDocuments
syn keyword cabalExtension HexFloatLiterals
syn keyword cabalExtension ImplicitParams
syn keyword cabalExtension ImplicitPrelude
syn keyword cabalExtension ImportQualifiedPost
syn keyword cabalExtension ImpredicativeTypes
syn keyword cabalExtension IncoherentInstances
syn keyword cabalExtension InstanceSigs
syn keyword cabalExtension InterruptibleFFI
syn keyword cabalExtension JavaScriptFFI
syn keyword cabalExtension KindSignatures
syn keyword cabalExtension LambdaCase
syn keyword cabalExtension LexicalNegation
syn keyword cabalExtension LiberalTypeSynonyms
syn keyword cabalExtension LinearTypes
syn keyword cabalExtension MagicHash
syn keyword cabalExtension MonadComprehensions
syn keyword cabalExtension MonadFailDesugaring
syn keyword cabalExtension MonoLocalBinds
syn keyword cabalExtension MonoPatBinds
syn keyword cabalExtension MonomorphismRestriction
syn keyword cabalExtension MultiParamTypeClasses
syn keyword cabalExtension MultiWayIf
syn keyword cabalExtension NPlusKPatterns
syn keyword cabalExtension NamedFieldPuns
syn keyword cabalExtension NamedWildCards
syn keyword cabalExtension NegativeLiterals
syn keyword cabalExtension NewQualifiedOperators
syn keyword cabalExtension NoAllowAmbiguousTypes
syn keyword cabalExtension NoApplicativeDo
syn keyword cabalExtension NoArrows
syn keyword cabalExtension NoAutoDeriveTypeable
syn keyword cabalExtension NoBangPatterns
syn keyword cabalExtension NoBinaryLiterals
syn keyword cabalExtension NoBlockArguments
syn keyword cabalExtension NoCApiFFI
syn keyword cabalExtension NoCPP
syn keyword cabalExtension NoCUSKs
syn keyword cabalExtension NoConstrainedClassMethods
syn keyword cabalExtension NoConstraintKinds
syn keyword cabalExtension NoDataKinds
syn keyword cabalExtension NoDatatypeContexts
syn keyword cabalExtension NoDefaultSignatures
syn keyword cabalExtension NoDeriveAnyClass
syn keyword cabalExtension NoDeriveDataTypeable
syn keyword cabalExtension NoDeriveFoldable
syn keyword cabalExtension NoDeriveFunctor
syn keyword cabalExtension NoDeriveGeneric
syn keyword cabalExtension NoDeriveLift
syn keyword cabalExtension NoDeriveTraversable
syn keyword cabalExtension NoDerivingStrategies
syn keyword cabalExtension NoDerivingVia
syn keyword cabalExtension NoDisambiguateRecordFields
syn keyword cabalExtension NoDoAndIfThenElse
syn keyword cabalExtension NoDoRec
syn keyword cabalExtension NoDuplicateRecordFields
syn keyword cabalExtension NoEmptyCase
syn keyword cabalExtension NoEmptyDataDecls
syn keyword cabalExtension NoEmptyDataDeriving
syn keyword cabalExtension NoExistentialQuantification
syn keyword cabalExtension NoExplicitForAll
syn keyword cabalExtension NoExplicitNamespaces
syn keyword cabalExtension NoExtendedDefaultRules
syn keyword cabalExtension NoExtensibleRecords
syn keyword cabalExtension NoFlexibleContexts
syn keyword cabalExtension NoFlexibleInstances
syn keyword cabalExtension NoForeignFunctionInterface
syn keyword cabalExtension NoFunctionalDependencies
syn keyword cabalExtension NoGADTSyntax
syn keyword cabalExtension NoGADTs
syn keyword cabalExtension NoGHCForeignImportPrim
syn keyword cabalExtension NoGeneralisedNewtypeDeriving
syn keyword cabalExtension NoGeneralizedNewtypeDeriving
syn keyword cabalExtension NoGenerics
syn keyword cabalExtension NoHereDocuments
syn keyword cabalExtension NoHexFloatLiterals
syn keyword cabalExtension NoImplicitParams
syn keyword cabalExtension NoImplicitPrelude
syn keyword cabalExtension NoImportQualifiedPost
syn keyword cabalExtension NoImpredicativeTypes
syn keyword cabalExtension NoIncoherentInstances
syn keyword cabalExtension NoInstanceSigs
syn keyword cabalExtension NoInterruptibleFFI
syn keyword cabalExtension NoJavaScriptFFI
syn keyword cabalExtension NoKindSignatures
syn keyword cabalExtension NoLambdaCase
syn keyword cabalExtension NoLexicalNegation
syn keyword cabalExtension NoLiberalTypeSynonyms
syn keyword cabalExtension NoLinearTypes
syn keyword cabalExtension NoMagicHash
syn keyword cabalExtension NoMonadComprehensions
syn keyword cabalExtension NoMonadFailDesugaring
syn keyword cabalExtension NoMonoLocalBinds
syn keyword cabalExtension NoMonoPatBinds
syn keyword cabalExtension NoMonomorphismRestriction
syn keyword cabalExtension NoMultiParamTypeClasses
syn keyword cabalExtension NoMultiWayIf
syn keyword cabalExtension NoNPlusKPatterns
syn keyword cabalExtension NoNamedFieldPuns
syn keyword cabalExtension NoNamedWildCards
syn keyword cabalExtension NoNegativeLiterals
syn keyword cabalExtension NoNewQualifiedOperators
syn keyword cabalExtension NoNondecreasingIndentation
syn keyword cabalExtension NoNullaryTypeClasses
syn keyword cabalExtension NoNumDecimals
syn keyword cabalExtension NoNumericUnderscores
syn keyword cabalExtension NoOverlappingInstances
syn keyword cabalExtension NoOverloadedLabels
syn keyword cabalExtension NoOverloadedLists
syn keyword cabalExtension NoOverloadedStrings
syn keyword cabalExtension NoPackageImports
syn keyword cabalExtension NoParallelArrays
syn keyword cabalExtension NoParallelListComp
syn keyword cabalExtension NoPartialTypeSignatures
syn keyword cabalExtension NoPatternGuards
syn keyword cabalExtension NoPatternSignatures
syn keyword cabalExtension NoPatternSynonyms
syn keyword cabalExtension NoPolyKinds
syn keyword cabalExtension NoPolymorphicComponents
syn keyword cabalExtension NoPostfixOperators
syn keyword cabalExtension NoQualifiedDo
syn keyword cabalExtension NoQuantifiedConstraints
syn keyword cabalExtension NoQuasiQuotes
syn keyword cabalExtension NoRank2Types
syn keyword cabalExtension NoRankNTypes
syn keyword cabalExtension NoRebindableSyntax
syn keyword cabalExtension NoRecordPuns
syn keyword cabalExtension NoRecordWildCards
syn keyword cabalExtension NoRecursiveDo
syn keyword cabalExtension NoRegularPatterns
syn keyword cabalExtension NoRelaxedPolyRec
syn keyword cabalExtension NoRestrictedTypeSynonyms
syn keyword cabalExtension NoRoleAnnotations
syn keyword cabalExtension NoSafeImports
syn keyword cabalExtension NoScopedTypeVariables
syn keyword cabalExtension NoStandaloneDeriving
syn keyword cabalExtension NoStandaloneKindSignatures
syn keyword cabalExtension NoStarIsType
syn keyword cabalExtension NoStaticPointers
syn keyword cabalExtension NoStrict
syn keyword cabalExtension NoStrictData
syn keyword cabalExtension NoTemplateHaskell
syn keyword cabalExtension NoTemplateHaskellQuotes
syn keyword cabalExtension NoTraditionalRecordSyntax
syn keyword cabalExtension NoTransformListComp
syn keyword cabalExtension NoTupleSections
syn keyword cabalExtension NoTypeApplications
syn keyword cabalExtension NoTypeFamilies
syn keyword cabalExtension NoTypeFamilyDependencies
syn keyword cabalExtension NoTypeInType
syn keyword cabalExtension NoTypeOperators
syn keyword cabalExtension NoTypeSynonymInstances
syn keyword cabalExtension NoUnboxedSums
syn keyword cabalExtension NoUnboxedTuples
syn keyword cabalExtension NoUndecidableInstances
syn keyword cabalExtension NoUndecidableSuperClasses
syn keyword cabalExtension NoUnicodeSyntax
syn keyword cabalExtension NoUnliftedFFITypes
syn keyword cabalExtension NoUnliftedNewtypes
syn keyword cabalExtension NoViewPatterns
syn keyword cabalExtension NoXmlSyntax
syn keyword cabalExtension NondecreasingIndentation
syn keyword cabalExtension NullaryTypeClasses
syn keyword cabalExtension NumDecimals
syn keyword cabalExtension NumericUnderscores
syn keyword cabalExtension OverlappingInstances
syn keyword cabalExtension OverloadedLabels
syn keyword cabalExtension OverloadedLists
syn keyword cabalExtension OverloadedStrings
syn keyword cabalExtension PackageImports
syn keyword cabalExtension ParallelArrays
syn keyword cabalExtension ParallelListComp
syn keyword cabalExtension PartialTypeSignatures
syn keyword cabalExtension PatternGuards
syn keyword cabalExtension PatternSignatures
syn keyword cabalExtension PatternSynonyms
syn keyword cabalExtension PolyKinds
syn keyword cabalExtension PolymorphicComponents
syn keyword cabalExtension PostfixOperators
syn keyword cabalExtension QualifiedDo
syn keyword cabalExtension QuantifiedConstraints
syn keyword cabalExtension QuasiQuotes
syn keyword cabalExtension Rank2Types
syn keyword cabalExtension RankNTypes
syn keyword cabalExtension RebindableSyntax
syn keyword cabalExtension RecordPuns
syn keyword cabalExtension RecordWildCards
syn keyword cabalExtension RecursiveDo
syn keyword cabalExtension RegularPatterns
syn keyword cabalExtension RelaxedPolyRec
syn keyword cabalExtension RestrictedTypeSynonyms
syn keyword cabalExtension RoleAnnotations
syn keyword cabalExtension Safe
syn keyword cabalExtension SafeImports
syn keyword cabalExtension ScopedTypeVariables
syn keyword cabalExtension StandaloneDeriving
syn keyword cabalExtension StandaloneKindSignatures
syn keyword cabalExtension StarIsType
syn keyword cabalExtension StaticPointers
syn keyword cabalExtension Strict
syn keyword cabalExtension StrictData
syn keyword cabalExtension TemplateHaskell
syn keyword cabalExtension TemplateHaskellQuotes
syn keyword cabalExtension TraditionalRecordSyntax
syn keyword cabalExtension TransformListComp
syn keyword cabalExtension Trustworthy
syn keyword cabalExtension TupleSections
syn keyword cabalExtension TypeApplications
syn keyword cabalExtension TypeFamilies
syn keyword cabalExtension TypeFamilyDependencies
syn keyword cabalExtension TypeInType
syn keyword cabalExtension TypeOperators
syn keyword cabalExtension TypeSynonymInstances
syn keyword cabalExtension UnboxedSums
syn keyword cabalExtension UnboxedTuples
syn keyword cabalExtension UndecidableInstances
syn keyword cabalExtension UndecidableSuperClasses
syn keyword cabalExtension UnicodeSyntax
syn keyword cabalExtension UnliftedFFITypes
syn keyword cabalExtension UnliftedNewtypes
syn keyword cabalExtension Unsafe
syn keyword cabalExtension ViewPatterns
syn keyword cabalExtension XmlSyntax

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_cabal_syn_inits")
  if version < 508
    let did_cabal_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink cabalVersion       Number
  HiLink cabalTruth         Boolean
  HiLink cabalComment       Comment
  HiLink cabalStatement     Statement
  HiLink cabalXStatement    Keyword
  HiLink cabalCategory      Type
  HiLink cabalFunction      Function
  HiLink cabalConditional   Conditional
  HiLink cabalOperator      Operator
  HiLink cabalCompiler      Constant
  HiLink cabalOs            Constant
  HiLink cabalLanguage      Constant
  HiLink cabalType          Constant
  HiLink cabalExtension     Constant

  delcommand HiLink
endif

let b:current_syntax = "cabal"

" vim: ts=8
