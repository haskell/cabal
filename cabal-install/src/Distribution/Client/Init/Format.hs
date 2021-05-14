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
--
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
, mkLibStanza
, mkExeStanza
, mkTestStanza
, mkPkgDescription
) where


import Distribution.Pretty
import Distribution.Fields
import Distribution.Client.Init.Types
import Text.PrettyPrint
import Distribution.Solver.Compat.Prelude hiding (empty)
import Distribution.PackageDescription.FieldGrammar
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Package (unPackageName)
import qualified Distribution.SPDX.License as SPDX
import Distribution.CabalSpecVersion


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
    :: FieldName -- ^ Name of the field
    -> Doc       -- ^ Field contents
    -> [String]  -- ^ Comment to explain the field
    -> Bool      -- ^ Should the field be included (commented out) even if blank?
    -> WriteOpts
    -> PrettyField FieldAnnotation
fieldD fieldName fieldContents fieldComments includeField opts
    | fieldContents == empty =
      -- If there is no content, optionally produce a commented out field.
      fieldSEmptyContents fieldComments
    | otherwise =
        -- If the "--no-comments" or "--minimal" flag is set, strip comments.
        let comments
              | isMinimal = []
              | hasNoComments = []
              | otherwise = fieldComments

        -- If the "--minimal" flag is set, strip comments.
        in fieldSWithContents comments
  where
    isMinimal = _optMinimal opts
    hasNoComments = _optNoComments opts

    fieldSEmptyContents cs
      | not includeField || isMinimal = PrettyEmpty
      | otherwise = PrettyField
        (commentedOutWithComments cs)
        fieldName
        empty

    fieldSWithContents cs =
      PrettyField (withComments (map ("-- " ++) cs)) fieldName fieldContents


-- | A field annotation instructing the pretty printer to comment out the field
--   and any contents, with no comments.
commentedOutWithComments :: [String] -> FieldAnnotation
commentedOutWithComments = FieldAnnotation True . map ("-- " ++)

-- | A field annotation with the specified comment lines.
withComments :: [String] -> FieldAnnotation
withComments = FieldAnnotation False

-- | A field annotation with no comments.
annNoComments :: FieldAnnotation
annNoComments = FieldAnnotation False []

postProcessFieldLines :: FieldAnnotation -> [String] -> [String]
postProcessFieldLines ann
    | annCommentedOut ann = fmap ("-- " ++)
    | otherwise = id

-- -------------------------------------------------------------------- --
-- Stanzas

mkLibStanza :: WriteOpts -> LibTarget -> PrettyField FieldAnnotation
mkLibStanza opts (LibTarget srcDirs lang expMods otherMods exts deps tools) =
  PrettySection annNoComments (toUTF8BS "library") []
    [ field "exposed-modules" formatExposedModules (toList expMods)
      ["Modules exported by the library."]
      True
      opts

    , field "other-modules" formatOtherModules otherMods
      ["Modules included in this library but not exported."]
      True
      opts

    , field "other-extensions" formatOtherExtensions exts
      ["LANGUAGE extensions used by modules in this package."]
      True
      opts

    , field "build-depends" formatDependencyList deps
      ["Other library packages from which modules are imported."]
      True
      opts

    , field "hs-source-dirs" formatHsSourceDirs (unsafeMakeSymbolicPath <$> srcDirs)
      ["Directories containing source files."]
      True
      opts

    , field (buildToolTag opts) formatDependencyList tools
      ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
      False
      opts

    , field "default-language" id lang
      ["Base language which the package is written in."]
      True
      opts
    ]

mkExeStanza :: WriteOpts -> ExeTarget -> PrettyField FieldAnnotation
mkExeStanza opts (ExeTarget exeMain appDirs lang otherMods exts deps tools) =
    PrettySection annNoComments (toUTF8BS "executable") [exeName]
      [ field "main-is" unsafeFromHs exeMain
         [".hs or .lhs file containing the Main module."]
         True
        opts

      , field "other-modules" formatOtherModules otherMods
        [ "Modules included in this executable, other than Main." ]
        True
        opts

      , field "other-extensions" formatOtherExtensions exts
        ["LANGUAGE extensions used by modules in this package."]
        True
        opts
      , field "build-depends" formatDependencyList deps
        ["Other library packages from which modules are imported."]
        True
        opts

      , field "hs-source-dirs" formatHsSourceDirs
        (unsafeMakeSymbolicPath <$> appDirs)
        ["Directories containing source files."]
        True
        opts

      , field (buildToolTag opts) formatDependencyList tools
        ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
        False
        opts

      , field "default-language" id lang
        ["Base language which the package is written in."]
        True
        opts
      ]
    where
      exeName = pretty $ _optPkgName opts

mkTestStanza :: WriteOpts -> TestTarget -> PrettyField FieldAnnotation
mkTestStanza opts (TestTarget testMain dirs lang otherMods exts deps tools) =
    PrettySection annNoComments (toUTF8BS "test-suite") [suiteName]
       [ field "default-language" id lang
         ["Base language which the package is written in."]
         True
         opts
       , field "other-modules" formatOtherModules otherMods
         [ "Modules included in this executable, other than Main." ]
         True
         opts

       , field "other-extensions" formatOtherExtensions exts
         ["LANGUAGE extensions used by modules in this package."]
         True
         opts

       , field "type" text "exitcode-stdio-1.0"
         ["The interface type and version of the test suite."]
         True
         opts

       , field "hs-source-dirs" formatHsSourceDirs
         (unsafeMakeSymbolicPath <$> dirs)
         ["Directories containing source files."]
         True
         opts

       , field "main-is" unsafeFromHs testMain
         ["The entrypoint to the test suite."]
         True
         opts

       , field  "build-depends" formatDependencyList deps
         ["Test dependencies."]
         True
         opts

       , field (buildToolTag opts) formatDependencyList tools
         ["Extra tools (e.g. alex, hsc2hs, ...) needed to build the source."]
         False
         opts
       ]
     where
       suiteName = text $ unPackageName (_optPkgName opts) ++ "-test"

mkPkgDescription :: WriteOpts -> PkgDescription -> [PrettyField FieldAnnotation]
mkPkgDescription opts pkgDesc =
    [ field "cabal-version" text (showCabalSpecVersion cabalSpec) [] False opts
    , field "name" pretty (_pkgName pkgDesc)
      ["Initial package description '" ++ prettyShow (_optPkgName opts) ++ "' generated by"
      , "'cabal init'. For further documentation, see:"
      , "  http://haskell.org/cabal/users-guide/"
      , ""
      , "The name of the package."
      ]
      True
      opts

    , field  "version" pretty (_pkgVersion pkgDesc)
             ["The package version.",
              "See the Haskell package versioning policy (PVP) for standards",
              "guiding when and how versions should be incremented.",
              "https://pvp.haskell.org",
              "PVP summary:     +-+------- breaking API changes",
              "                 | | +----- non-breaking API additions",
              "                 | | | +--- code changes with no API change"]
      True
      opts

    , field "synopsis" text (_pkgSynopsis pkgDesc)
      ["A short (one-line) description of the package."]
      True
      opts

    , field "description" text ""
      ["A longer description of the package."]
      True
      opts

    , field "homepage" text (_pkgHomePage pkgDesc)
      ["URL for the project homepage or repository."]
      False
      opts

    , field "bug-reports" text ""
      ["A URL where users can report bugs."]
      False
      opts

    , field  "license" pretty (_pkgLicense pkgDesc)
      ["The license under which the package is released."]
      True
      opts

    , case _pkgLicense pkgDesc of
        SPDX.NONE -> PrettyEmpty
        _ -> field "license-file" text "LICENSE"
             ["The file containing the license text."]
             False
             opts

    , field "author" text (_pkgAuthor pkgDesc)
      ["The package author(s)."]
      True
      opts

    , field "maintainer" text (_pkgEmail pkgDesc)
      ["An email address to which users can send suggestions, bug reports, and patches."]
      True
      opts

    , field "copyright" text ""
      ["A copyright notice."]
      True
      opts

    , field "category" text (_pkgCategory pkgDesc)
      []
      False
      opts
    , if cabalSpec < CabalSpecV2_2
      then PrettyEmpty
      else field "build-type" text "Simple"
           []
           False
           opts
    , case _pkgExtraDocFiles pkgDesc of
        Nothing -> PrettyEmpty
        Just fs ->
          field "extra-doc-files" formatExtraSourceFiles  (toList fs)
          ["Extra doc files to be distributed with the package, such as a CHANGELOG or a README."]
          True
          opts

    , field "extra-source-files" formatExtraSourceFiles (toList $ _pkgExtraSrcFiles pkgDesc)
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
