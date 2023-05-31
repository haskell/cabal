{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Distribution.Client.Init.Format
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Pretty printing and field formatting utilities used for file creation.
module Distribution.Client.Init.Format
  ( -- * cabal file formatters
    listFieldS
  , field
  , fieldD
  , commentedOutWithComments
  , withComments
  , annNoComments
  , postProcessFieldLines

    -- * stanza generation
  , mkCommonStanza
  , mkLibStanza
  , mkExeStanza
  , mkTestStanza
  , mkPkgDescription
  ) where

import Distribution.CabalSpecVersion
import Distribution.Client.Init.Types
import Distribution.FieldGrammar.Newtypes (SpecLicense (SpecLicense))
import Distribution.Fields
import Distribution.License
import Distribution.Package (unPackageName)
import Distribution.PackageDescription.FieldGrammar
import Distribution.Pretty
import qualified Distribution.SPDX.License as SPDX
import Distribution.Simple.Utils hiding (cabalVersion)
import Distribution.Solver.Compat.Prelude hiding (empty)
import Distribution.Utils.Path
import Text.PrettyPrint

-- | Construct a 'PrettyField' from a field that can be automatically
--   converted to a 'Doc' via 'display'.
field
  :: Pretty b
  => FieldName
  -> (a -> b)
  -> a
  -> [String]
  -> Bool
  -> WriteOpts
  -> PrettyField FieldAnnotation
field fieldName modifier fieldContents =
  fieldD fieldName (pretty $ modifier fieldContents)

-- | Construct a 'PrettyField' from a 'Doc' Flag.
fieldD
  :: FieldName
  -- ^ Name of the field
  -> Doc
  -- ^ Field contents
  -> [String]
  -- ^ Comment to explain the field
  -> Bool
  -- ^ Should the field be included (commented out) even if blank?
  -> WriteOpts
  -> PrettyField FieldAnnotation
fieldD fieldName fieldContents fieldComments includeField opts
  -- If the "--no-comments" or "--minimal" flag is set, strip comments.
  | hasNoComments || isMinimal = contents NoComment
  | otherwise = contents $ commentPositionFor fieldName fieldComments
  where
    commentPositionFor fn
      | fn == "cabal-version" = CommentAfter
      | otherwise = CommentBefore

    isMinimal = _optMinimal opts
    hasNoComments = _optNoComments opts

    contents
      -- If there is no content, optionally produce a commented out field.
      | fieldContents == empty = fieldSEmptyContents
      | otherwise = fieldSWithContents

    fieldSEmptyContents cs
      | not includeField || isMinimal = PrettyEmpty
      | otherwise =
          PrettyField
            (commentedOutWithComments cs)
            fieldName
            empty

    fieldSWithContents cs =
      PrettyField (withComments cs) fieldName fieldContents

-- | A field annotation instructing the pretty printer to comment out the field
--   and any contents, with no comments.
commentedOutWithComments :: CommentPosition -> FieldAnnotation
commentedOutWithComments (CommentBefore cs) = FieldAnnotation True . CommentBefore $ map commentNoTrailing cs
commentedOutWithComments (CommentAfter cs) = FieldAnnotation True . CommentAfter $ map commentNoTrailing cs
commentedOutWithComments NoComment = FieldAnnotation True NoComment

-- | A field annotation with the specified comment lines.
withComments :: CommentPosition -> FieldAnnotation
withComments (CommentBefore cs) = FieldAnnotation False . CommentBefore $ map commentNoTrailing cs
withComments (CommentAfter cs) = FieldAnnotation False . CommentAfter $ map commentNoTrailing cs
withComments NoComment = FieldAnnotation False NoComment

-- | A field annotation with no comments.
annNoComments :: FieldAnnotation
annNoComments = FieldAnnotation False NoComment

postProcessFieldLines :: FieldAnnotation -> [String] -> [String]
postProcessFieldLines ann
  | annCommentedOut ann = fmap commentNoTrailing
  | otherwise = id

-- -------------------------------------------------------------------- --
-- Stanzas

-- The common stanzas are hardcoded for simplicity purposes,
-- see https://github.com/haskell/cabal/pull/7558#discussion_r693173846
mkCommonStanza :: WriteOpts -> PrettyField FieldAnnotation
mkCommonStanza opts = case specHasCommonStanzas $ _optCabalSpec opts of
  NoCommonStanzas -> PrettyEmpty
  _ ->
    PrettySection
      annNoComments
      "common"
      [text "warnings"]
      [field "ghc-options" text "-Wall" [] False opts]

mkLibStanza :: WriteOpts -> LibTarget -> PrettyField FieldAnnotation
mkLibStanza opts (LibTarget srcDirs lang expMods otherMods exts deps tools) =
  PrettySection
    annNoComments
    (toUTF8BS "library")
    []
    [ case specHasCommonStanzas $ _optCabalSpec opts of
        NoCommonStanzas -> PrettyEmpty
        _ ->
          field
            "import"
            (hsep . map text)
            ["warnings"]
            ["Import common warning flags."]
            False
            opts
    , field
        "exposed-modules"
        formatExposedModules
        (toList expMods)
        ["Modules exported by the library."]
        True
        opts
    , field
        "other-modules"
        formatOtherModules
        otherMods
        ["Modules included in this library but not exported."]
        True
        opts
    , field
        "other-extensions"
        formatOtherExtensions
        exts
        ["LANGUAGE extensions used by modules in this package."]
        True
        opts
    , field
        "build-depends"
        formatDependencyList
        deps
        ["Other library packages from which modules are imported."]
        True
        opts
    , field
        "hs-source-dirs"
        formatHsSourceDirs
        (unsafeMakeSymbolicPath <$> srcDirs)
        ["Directories containing source files."]
        True
        opts
    , field
        (buildToolTag opts)
        formatDependencyList
        tools
        ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
        False
        opts
    , field
        "default-language"
        id
        lang
        ["Base language which the package is written in."]
        True
        opts
    ]

mkExeStanza :: WriteOpts -> ExeTarget -> PrettyField FieldAnnotation
mkExeStanza opts (ExeTarget exeMain appDirs lang otherMods exts deps tools) =
  PrettySection
    annNoComments
    (toUTF8BS "executable")
    [exeName]
    [ case specHasCommonStanzas $ _optCabalSpec opts of
        NoCommonStanzas -> PrettyEmpty
        _ ->
          field
            "import"
            (hsep . map text)
            ["warnings"]
            ["Import common warning flags."]
            False
            opts
    , field
        "main-is"
        unsafeFromHs
        exeMain
        [".hs or .lhs file containing the Main module."]
        True
        opts
    , field
        "other-modules"
        formatOtherModules
        otherMods
        ["Modules included in this executable, other than Main."]
        True
        opts
    , field
        "other-extensions"
        formatOtherExtensions
        exts
        ["LANGUAGE extensions used by modules in this package."]
        True
        opts
    , field
        "build-depends"
        formatDependencyList
        deps
        ["Other library packages from which modules are imported."]
        True
        opts
    , field
        "hs-source-dirs"
        formatHsSourceDirs
        (unsafeMakeSymbolicPath <$> appDirs)
        ["Directories containing source files."]
        True
        opts
    , field
        (buildToolTag opts)
        formatDependencyList
        tools
        ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
        False
        opts
    , field
        "default-language"
        id
        lang
        ["Base language which the package is written in."]
        True
        opts
    ]
  where
    exeName = pretty $ _optPkgName opts

mkTestStanza :: WriteOpts -> TestTarget -> PrettyField FieldAnnotation
mkTestStanza opts (TestTarget testMain dirs lang otherMods exts deps tools) =
  PrettySection
    annNoComments
    (toUTF8BS "test-suite")
    [suiteName]
    [ case specHasCommonStanzas $ _optCabalSpec opts of
        NoCommonStanzas -> PrettyEmpty
        _ ->
          field
            "import"
            (hsep . map text)
            ["warnings"]
            ["Import common warning flags."]
            False
            opts
    , field
        "default-language"
        id
        lang
        ["Base language which the package is written in."]
        True
        opts
    , field
        "other-modules"
        formatOtherModules
        otherMods
        ["Modules included in this executable, other than Main."]
        True
        opts
    , field
        "other-extensions"
        formatOtherExtensions
        exts
        ["LANGUAGE extensions used by modules in this package."]
        True
        opts
    , field
        "type"
        text
        "exitcode-stdio-1.0"
        ["The interface type and version of the test suite."]
        True
        opts
    , field
        "hs-source-dirs"
        formatHsSourceDirs
        (unsafeMakeSymbolicPath <$> dirs)
        ["Directories containing source files."]
        True
        opts
    , field
        "main-is"
        unsafeFromHs
        testMain
        ["The entrypoint to the test suite."]
        True
        opts
    , field
        "build-depends"
        formatDependencyList
        deps
        ["Test dependencies."]
        True
        opts
    , field
        (buildToolTag opts)
        formatDependencyList
        tools
        ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
        False
        opts
    ]
  where
    suiteName = text $ unPackageName (_optPkgName opts) ++ "-test"

mkPkgDescription :: WriteOpts -> PkgDescription -> [PrettyField FieldAnnotation]
mkPkgDescription opts pkgDesc =
  [ field
      "cabal-version"
      text
      ((if cabalSpec < CabalSpecV1_12 then ">=" else "") ++ showCabalSpecVersion cabalSpec)
      [ "The cabal-version field refers to the version of the .cabal specification,"
      , "and can be different from the cabal-install (the tool) version and the"
      , "Cabal (the library) version you are using. As such, the Cabal (the library)"
      , "version used must be equal or greater than the version stated in this field."
      , "Starting from the specification version 2.2, the cabal-version field must be"
      , "the first thing in the cabal file."
      ]
      False
      opts
  , field
      "name"
      pretty
      (_pkgName pkgDesc)
      [ "Initial package description '" ++ prettyShow (_optPkgName opts) ++ "' generated by"
      , "'cabal init'. For further documentation, see:"
      , "  http://haskell.org/cabal/users-guide/"
      , ""
      , "The name of the package."
      ]
      True
      opts
  , field
      "version"
      pretty
      (_pkgVersion pkgDesc)
      [ "The package version."
      , "See the Haskell package versioning policy (PVP) for standards"
      , "guiding when and how versions should be incremented."
      , "https://pvp.haskell.org"
      , "PVP summary:     +-+------- breaking API changes"
      , "                 | | +----- non-breaking API additions"
      , "                 | | | +--- code changes with no API change"
      ]
      True
      opts
  , field
      "synopsis"
      text
      (_pkgSynopsis pkgDesc)
      ["A short (one-line) description of the package."]
      True
      opts
  , field
      "description"
      text
      ""
      ["A longer description of the package."]
      True
      opts
  , field
      "homepage"
      text
      (_pkgHomePage pkgDesc)
      ["URL for the project homepage or repository."]
      False
      opts
  , field
      "bug-reports"
      text
      ""
      ["A URL where users can report bugs."]
      False
      opts
  , field
      "license"
      pretty
      (_pkgLicense pkgDesc)
      ["The license under which the package is released."]
      True
      opts
  , case _pkgLicense pkgDesc of
      SpecLicense (Left SPDX.NONE) -> PrettyEmpty
      SpecLicense (Right AllRightsReserved) -> PrettyEmpty
      SpecLicense (Right UnspecifiedLicense) -> PrettyEmpty
      _ ->
        field
          "license-file"
          text
          "LICENSE"
          ["The file containing the license text."]
          False
          opts
  , field
      "author"
      text
      (_pkgAuthor pkgDesc)
      ["The package author(s)."]
      True
      opts
  , field
      "maintainer"
      text
      (_pkgEmail pkgDesc)
      ["An email address to which users can send suggestions, bug reports, and patches."]
      True
      opts
  , field
      "copyright"
      text
      ""
      ["A copyright notice."]
      True
      opts
  , field
      "category"
      text
      (_pkgCategory pkgDesc)
      []
      False
      opts
  , field
      "build-type"
      text
      "Simple"
      []
      False
      opts
  , case _pkgExtraDocFiles pkgDesc of
      Nothing -> PrettyEmpty
      Just fs ->
        field
          "extra-doc-files"
          formatExtraSourceFiles
          (toList fs)
          ["Extra doc files to be distributed with the package, such as a CHANGELOG or a README."]
          True
          opts
  , field
      "extra-source-files"
      formatExtraSourceFiles
      (toList $ _pkgExtraSrcFiles pkgDesc)
      ["Extra source files to be distributed with the package, such as examples, or a tutorial module."]
      True
      opts
  ]
  where
    cabalSpec = _pkgCabalVersion pkgDesc

-- -------------------------------------------------------------------- --
-- Utils

listFieldS :: [String] -> Doc
listFieldS = text . intercalate ", "

unsafeFromHs :: HsFilePath -> Doc
unsafeFromHs = text . _hsFilePath

buildToolTag :: WriteOpts -> FieldName
buildToolTag opts
  | _optCabalSpec opts < CabalSpecV3_0 = "build-tools"
  | otherwise = "build-tool-depends"

commentNoTrailing :: String -> String
commentNoTrailing "" = "--"
commentNoTrailing c = "-- " ++ c
