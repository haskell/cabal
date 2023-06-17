" Vim syntax file
if exists("b:current_syntax")
  finish
endif

" this file uses line continuation
let s:cpo_save = &cpo
set cpo&vim

" Lexical structure, keywords and comments
""""""""""""""""""""""""""""""""""""""""""

" set iskeyword for this syntax script
syn iskeyword a-z,A-Z,48-57,192-255,-,.

" Comments
syn match cabalComment    /--.*$/

" Enumerations
""""""""""""""

syn case match

syn keyword cabalBuildType contained
  \ Simple
  \ Configure
  \ Custom

" test-suite,benchmark: exitcode-stdio-1.0, detailed-0.9
" foreign-library:      native-shared, native-static
" source-repository:    git, ...
syn keyword cabalCompType contained
  \ exitcode-stdio-1.0
  \ detailed-0.0
  \ native-shared native-static
  \ cvs svn darcs mercurial git

syn keyword cabalLanguage contained
  \ Haskell98
  \ Haskell2010
  \ GHC2021

" To update this in Cabal, `cabal repl Cabal` and:
" >>> :m *Distribution.PackageDescription.FieldGrammar
" >>> _syntaxFieldNames
syn keyword cabalFieldName contained
  \ asm-options
  \ asm-sources
  \ author
  \ autogen-includes
  \ autogen-modules
  \ benchmark-module
  \ branch
  \ bug-reports
  \ build-depends
  \ build-tool-depends
  \ build-tools
  \ build-type
  \ buildable
  \ c-sources
  \ cabal-version
  \ category
  \ cc-options
  \ cmm-options
  \ cmm-sources
  \ copyright
  \ cpp-options
  \ cxx-options
  \ cxx-sources
  \ data-dir
  \ data-files
  \ default
  \ default-extensions
  \ default-language
  \ description
  \ exposed
  \ exposed-modules
  \ extensions
  \ extra-bundled-libraries
  \ extra-doc-files
  \ extra-dynamic-library-flavours
  \ extra-framework-dirs
  \ extra-ghci-libraries
  \ extra-lib-dirs
  \ extra-libraries
  \ extra-library-flavours
  \ extra-source-files
  \ extra-tmp-files
  \ frameworks
  \ ghc-options
  \ ghc-prof-options
  \ ghc-shared-options
  \ ghcjs-options
  \ ghcjs-prof-options
  \ ghcjs-shared-options
  \ homepage
  \ hs-source-dir
  \ hs-source-dirs
  \ hsc2hs-options
  \ hugs-options
  \ include-dirs
  \ includes
  \ install-includes
  \ jhc-options
  \ js-sources
  \ ld-options
  \ lib-version-info
  \ lib-version-linux
  \ license
  \ license-file
  \ license-files
  \ location
  \ main-is
  \ maintainer
  \ manual
  \ mixins
  \ mod-def-file
  \ module
  \ name
  \ nhc98-options
  \ options
  \ other-extensions
  \ other-languages
  \ other-modules
  \ package-url
  \ pkgconfig-depends
  \ reexported-modules
  \ scope
  \ setup-depends
  \ signatures
  \ stability
  \ subdir
  \ synopsis
  \ tag
  \ test-module
  \ tested-with
  \ type
  \ version
  \ virtual-modules

" To update this in Cabal, `cabal repl Cabal` and:
" >>> :m *Distribution.PackageDescription.FieldGrammar
" >>> _syntaxExtensions
syn keyword cabalExtension contained
  \ Safe
  \ Trustworthy
  \ Unsafe
  \ AllowAmbiguousTypes
  \ ApplicativeDo
  \ Arrows
  \ AutoDeriveTypeable
  \ BangPatterns
  \ BinaryLiterals
  \ BlockArguments
  \ CApiFFI
  \ CPP
  \ CUSKs
  \ ConstrainedClassMethods
  \ ConstraintKinds
  \ DataKinds
  \ DatatypeContexts
  \ DefaultSignatures
  \ DeepSubsumption
  \ DeriveAnyClass
  \ DeriveDataTypeable
  \ DeriveFoldable
  \ DeriveFunctor
  \ DeriveGeneric
  \ DeriveLift
  \ DeriveTraversable
  \ DerivingStrategies
  \ DerivingVia
  \ DisambiguateRecordFields
  \ DoAndIfThenElse
  \ DoRec
  \ DuplicateRecordFields
  \ EmptyCase
  \ EmptyDataDecls
  \ EmptyDataDeriving
  \ ExistentialQuantification
  \ ExplicitForAll
  \ ExplicitNamespaces
  \ ExtendedDefaultRules
  \ ExtendedLiterals
  \ ExtensibleRecords
  \ FieldSelectors
  \ FlexibleContexts
  \ FlexibleInstances
  \ ForeignFunctionInterface
  \ FunctionalDependencies
  \ GADTSyntax
  \ GADTs
  \ GHCForeignImportPrim
  \ GeneralisedNewtypeDeriving
  \ GeneralizedNewtypeDeriving
  \ Generics
  \ HereDocuments
  \ HexFloatLiterals
  \ ImplicitParams
  \ ImplicitPrelude
  \ ImportQualifiedPost
  \ ImpredicativeTypes
  \ IncoherentInstances
  \ InstanceSigs
  \ InterruptibleFFI
  \ JavaScriptFFI
  \ KindSignatures
  \ LambdaCase
  \ LexicalNegation
  \ LiberalTypeSynonyms
  \ LinearTypes
  \ RequiredTypeArguments
  \ MagicHash
  \ MonadComprehensions
  \ MonadFailDesugaring
  \ MonoLocalBinds
  \ MonoPatBinds
  \ MonomorphismRestriction
  \ MultiParamTypeClasses
  \ MultiWayIf
  \ NPlusKPatterns
  \ NamedFieldPuns
  \ NamedWildCards
  \ NegativeLiterals
  \ NewQualifiedOperators
  \ NondecreasingIndentation
  \ NullaryTypeClasses
  \ NumDecimals
  \ NumericUnderscores
  \ OverlappingInstances
  \ OverloadedLabels
  \ OverloadedLists
  \ OverloadedRecordDot
  \ OverloadedStrings
  \ PackageImports
  \ ParallelArrays
  \ ParallelListComp
  \ PartialTypeSignatures
  \ PatternGuards
  \ PatternSignatures
  \ PatternSynonyms
  \ PolyKinds
  \ PolymorphicComponents
  \ PostfixOperators
  \ QualifiedDo
  \ QuantifiedConstraints
  \ QuasiQuotes
  \ Rank2Types
  \ RankNTypes
  \ RebindableSyntax
  \ RecordPuns
  \ RecordWildCards
  \ RecursiveDo
  \ RegularPatterns
  \ RelaxedPolyRec
  \ RestrictedTypeSynonyms
  \ RoleAnnotations
  \ SafeImports
  \ ScopedTypeVariables
  \ StandaloneDeriving
  \ StandaloneKindSignatures
  \ StarIsType
  \ StaticPointers
  \ Strict
  \ StrictData
  \ TemplateHaskell
  \ TemplateHaskellQuotes
  \ TraditionalRecordSyntax
  \ TransformListComp
  \ TupleSections
  \ TypeApplications
  \ TypeData
  \ TypeFamilies
  \ TypeFamilyDependencies
  \ TypeInType
  \ TypeOperators
  \ TypeSynonymInstances
  \ UnboxedSums
  \ UnboxedTuples
  \ UndecidableInstances
  \ UndecidableSuperClasses
  \ UnicodeSyntax
  \ UnliftedDatatypes
  \ UnliftedFFITypes
  \ UnliftedNewtypes
  \ ViewPatterns
  \ XmlSyntax
  \ NoAllowAmbiguousTypes
  \ NoApplicativeDo
  \ NoArrows
  \ NoAutoDeriveTypeable
  \ NoBangPatterns
  \ NoBinaryLiterals
  \ NoBlockArguments
  \ NoCApiFFI
  \ NoCPP
  \ NoCUSKs
  \ NoConstrainedClassMethods
  \ NoConstraintKinds
  \ NoDataKinds
  \ NoDatatypeContexts
  \ NoDefaultSignatures
  \ NoDeepSubsumption
  \ NoDeriveAnyClass
  \ NoDeriveDataTypeable
  \ NoDeriveFoldable
  \ NoDeriveFunctor
  \ NoDeriveGeneric
  \ NoDeriveLift
  \ NoDeriveTraversable
  \ NoDerivingStrategies
  \ NoDerivingVia
  \ NoDisambiguateRecordFields
  \ NoDoAndIfThenElse
  \ NoDoRec
  \ NoDuplicateRecordFields
  \ NoEmptyCase
  \ NoEmptyDataDecls
  \ NoEmptyDataDeriving
  \ NoExistentialQuantification
  \ NoExplicitForAll
  \ NoExplicitNamespaces
  \ NoExtendedDefaultRules
  \ NoExtendedLiterals
  \ NoExtensibleRecords
  \ NoFieldSelectors
  \ NoFlexibleContexts
  \ NoFlexibleInstances
  \ NoForeignFunctionInterface
  \ NoFunctionalDependencies
  \ NoGADTSyntax
  \ NoGADTs
  \ NoGHCForeignImportPrim
  \ NoGeneralisedNewtypeDeriving
  \ NoGeneralizedNewtypeDeriving
  \ NoGenerics
  \ NoHereDocuments
  \ NoHexFloatLiterals
  \ NoImplicitParams
  \ NoImplicitPrelude
  \ NoImportQualifiedPost
  \ NoImpredicativeTypes
  \ NoIncoherentInstances
  \ NoInstanceSigs
  \ NoInterruptibleFFI
  \ NoJavaScriptFFI
  \ NoKindSignatures
  \ NoLambdaCase
  \ NoLexicalNegation
  \ NoLiberalTypeSynonyms
  \ NoLinearTypes
  \ NoRequiredTypeArguments
  \ NoMagicHash
  \ NoMonadComprehensions
  \ NoMonadFailDesugaring
  \ NoMonoLocalBinds
  \ NoMonoPatBinds
  \ NoMonomorphismRestriction
  \ NoMultiParamTypeClasses
  \ NoMultiWayIf
  \ NoNPlusKPatterns
  \ NoNamedFieldPuns
  \ NoNamedWildCards
  \ NoNegativeLiterals
  \ NoNewQualifiedOperators
  \ NoNondecreasingIndentation
  \ NoNullaryTypeClasses
  \ NoNumDecimals
  \ NoNumericUnderscores
  \ NoOverlappingInstances
  \ NoOverloadedLabels
  \ NoOverloadedLists
  \ NoOverloadedRecordDot
  \ NoOverloadedStrings
  \ NoPackageImports
  \ NoParallelArrays
  \ NoParallelListComp
  \ NoPartialTypeSignatures
  \ NoPatternGuards
  \ NoPatternSignatures
  \ NoPatternSynonyms
  \ NoPolyKinds
  \ NoPolymorphicComponents
  \ NoPostfixOperators
  \ NoQualifiedDo
  \ NoQuantifiedConstraints
  \ NoQuasiQuotes
  \ NoRank2Types
  \ NoRankNTypes
  \ NoRebindableSyntax
  \ NoRecordPuns
  \ NoRecordWildCards
  \ NoRecursiveDo
  \ NoRegularPatterns
  \ NoRelaxedPolyRec
  \ NoRestrictedTypeSynonyms
  \ NoRoleAnnotations
  \ NoSafeImports
  \ NoScopedTypeVariables
  \ NoStandaloneDeriving
  \ NoStandaloneKindSignatures
  \ NoStarIsType
  \ NoStaticPointers
  \ NoStrict
  \ NoStrictData
  \ NoTemplateHaskell
  \ NoTemplateHaskellQuotes
  \ NoTraditionalRecordSyntax
  \ NoTransformListComp
  \ NoTupleSections
  \ NoTypeApplications
  \ NoTypeData
  \ NoTypeFamilies
  \ NoTypeFamilyDependencies
  \ NoTypeInType
  \ NoTypeOperators
  \ NoTypeSynonymInstances
  \ NoUnboxedSums
  \ NoUnboxedTuples
  \ NoUndecidableInstances
  \ NoUndecidableSuperClasses
  \ NoUnicodeSyntax
  \ NoUnliftedDatatypes
  \ NoUnliftedFFITypes
  \ NoUnliftedNewtypes
  \ NoViewPatterns
  \ NoXmlSyntax

" Cabal format is (mostly) case-insensitive
syn case ignore

" Structure regions
"""""""""""""""""""

" Top level stanzas, cannot be nested, only library can be have optional name
syn match cabalStanzaLineRegion
  \ /^\clibrary\(\s\+\k\+\)\=\s*$/

" Top level stanzas with an identifier
syn match cabalStanzaLineRegion
  \ /^\c\(flag\|common\|source-repository\|executable\|test-suite\|benchmark\|foreign-library\)\s\+\k\+\s*$/

" Conditionals are nested
syn match cabalConditionalRegion
  \ contains=cabalOperator,cabalBoolean
  \ /^\c\s\+\(if\|elif\|else\)[^:]*$/

" Unindented fields
syn match cabalFieldRegion
  \ /^[a-zA-Z0-9-]\+\s*:.*\n\(\s\+.*\n\|\s*\n\)*/

" Indented fields
"
" We use backreferences to fake the recognition of indentation.
" Unfortunately I don't know a way to anchor contained
" matches to the boundaries of the enclosing region.
syn match cabalFieldRegion
  \ /^\(\s\+\)[a-zA-Z0-9-]\+\s*:.*\n\(\1\s\+.*\n\|\s*\n\)*/

" Component stanzas
"""""""""""""""""""

syn match cabalCategoryTitle
  \ contained
  \ /\c[a-z0-9-]\+/

syn keyword cabalCategory
  \ contained containedin=cabalStanzaLineRegion
  \ nextgroup=cabalCategoryTitle skipwhite
  \ flag common source-repository library executable test-suite benchmark foreign-library

" Version ranges
""""""""""""""""

syn match cabalOperator        contained /&&\|||\|!/
syn match cabalVersionOperator contained /==\|\^\?>=\|<=\|<\|>/
" match version: `[%]\@<!` is to exclude `%20` in http addresses.
syn match cabalVersion         contained /[%$_-]\@<!\<\d\+\%(\.\d\+\)*\%(\.\*\)\?\>/
" cabalVersionRegion which limits the scope of cabalVersion pattern.
syn match cabalVersionRegion
  \ contains=cabalVersionOperator,cabalVersion
  \ keepend
  \ /\%(==\|\^\?>=\|<=\|<\|>\)\s*\d\+\%(\.\d\+\)*\%(\.\*\)\?\>/

" Compilers
"""""""""""

syn keyword cabalCompiler contained ghc nhc yhc hugs hbc helium jhc lhc

" Conditionals
""""""""""""""

syn keyword cabalConditional contained containedin=cabalConditionalRegion
  \ if elif else

syn keyword cabalFunction contained
  \ os arch impl flag

syn region cabalFunctionRegion start=+\(os\|arch\|impl\|flag\)\s*(+ end=+)+
  \ contained containedin=cabalConditionalRegion
  \ contains=cabalOs,cabalFunction,cabalCompiler,cabalVersionRegion

" Common stanzas
""""""""""""""""

syn match cabalImportName contained /\c\<[a-z0-9-]\+\>/
syn keyword cabalImport contained import
syn match cabalImportRegion
  \ contains=cabalImport,cabalColon,cabalImportName
  \ /^\c\(\s\+\)import\s*:.*\n\(\1\s\+.*\n\|\s*\n\)*/

" Fields names
""""""""""""""

syn match cabalFieldNameRegion contained containedin=cabalFieldRegion
  \ contains=cabalFieldName
  \ /^\s*[a-zA-Z0-9-]\+\s*:/

syn match cabalColon contained containedin=cabalFieldNameRegion
  \ /:/

" Field Values
"""""""""""""""""""""""""""""""""

syn keyword cabalBoolean contained containedin=cabalFieldRegion
  \ true false

" cabal-version
syn match cabalSpecVersion contained
  \ /\(>=\s*1\.\(0\|2\|4\|6\|8\|10\)\|1\.\(12\|16\|18\|20\|22\|24\)\|2\.\(0\|2\|4\)\|3\.\(\0\|4\|6\)\)/
syn match cabalSpecVersionRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalSpecVersion
  \ /\c^cabal-version\s*:.*\n\(\s\+.*\n\|\s*\n\)*/

" version
syn match cabalPkgVersionRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalVersion
  \ /\c^version\s*:.*\n\(\s\+.*\n\|\s*\n\)*/

" description
syn match cabalDescriptionNameRegion
  \ contained
  \ contains=cabalFieldName
  \ /\c^description/
syn match cabalDescriptionRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalDescriptionNameRegion
  \ /\c^description\s*:.*\n\(\s\+.*\n\|\s*\n\)*/

" tested-with
syn match cabalTestedWithRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalVersionRegion,cabalOperator,cabalCompiler
  \ /\c^tested-with\s*:.*\n\(\s\+.*\n\|\s*\n\)*/

" build-depends,build-tool-depends
"
" fields with version ranges
syn match cabalDependsRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalVersionRegion,cabalOperator
  \ /\c^\(\s\+\)\(build-depends\|build-tool-depends\)\s*:.*\n\(\1\s\+.*\n\|\s*\n\)*/

" build-type
syn match cabalBuildTypeRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalBuildType
  \ /\c^build-type\s*:.*\n\(\s\+.*\n\|\s*\n\)*/

" type
syn match cabalCompTypeRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalCompType
  \ /\c^\(\s\+\)type\s*:.*\n\(\1\s\+.*\n\|\s*\n\)*/

" default-extensions other-extensions extensions
syn match cabalExtensionsRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalExtension
  \ /^\c\(\s\+\)\(default-extensions\|other-extensions\|extensions\)\s*:.*\n\(\1\s\+.*\n\|\s*\n\)*/

" default-language other-languages
syn match cabalLanguagesRegion
  \ contained containedin=cabalFieldRegion
  \ contains=cabalFieldNameRegion,cabalLanguage
  \ /^\c\(\s\+\)\(default-language\|other-languages\)\s*:.*\n\(\1\s\+.*\n\|\s*\n\)*/

" Define the default highlighting
"""""""""""""""""""""""""""""""""

hi link cabalComment           Comment

" We highlight stanza region, as it makes component name highlighted
hi link cabalStanzaLineRegion  NONE
hi link cabalFieldRegion       NONE
hi link cabalFieldNameRegion   NONE
hi link cabalConditionalRegion NONE

hi link cabalCategory          Type
hi link cabalCategoryTitle     Title

hi link cabalImport            Type
hi link cabalImportName        Title

hi link cabalFunction          Function

hi link cabalConditional       Conditional
hi link cabalOperator          Operator
hi link cabalVersionOperator   Special
hi link cabalSpecVersion       Special
hi link cabalVersion           Special

hi link cabalFieldName         Keyword
hi link cabalColon             Operator
hi link cabalBoolean           Boolean

hi link cabalBuildType         Constant
hi link cabalCompiler          Constant
hi link cabalCompType          Constant
hi link cabalExtension         Constant
hi link cabalLanguage          Constant
hi link cabalOs                Constant

let b:current_syntax="mini-cabal"

" vim: ts=2
