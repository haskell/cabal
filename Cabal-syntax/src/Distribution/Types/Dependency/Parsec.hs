module Distribution.Types.Dependency.Parsec where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.VersionRange (isAnyVersionLight)
import Distribution.Version (VersionRange, anyVersion, simplifyVersionRange)

import Distribution.Types.Annotation

import Distribution.CabalSpecVersion
import Distribution.Compat.CharParsing (char, spaces)
import Distribution.Compat.Parsing (between, option)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.PackageName
import Distribution.Types.Dependency.Internal
import Distribution.Types.Version.Parsec
import Distribution.Types.VersionRange.Parsec
import Distribution.Types.UnqualComponentName
import Distribution.Types.UnqualComponentName.Parsec

import qualified Distribution.Compat.NonEmptySet as NES
import qualified Text.PrettyPrint as PP


-- |
--
-- >>> simpleParsec "mylib:sub" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub") :| [])))
--
-- >>> simpleParsec "mylib:{sub1,sub2}" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub1") :| [LSubLibName (UnqualComponentName "sub2")])))
--
-- >>> simpleParsec "mylib:{ sub1 , sub2 }" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub1") :| [LSubLibName (UnqualComponentName "sub2")])))
--
-- >>> simpleParsec "mylib:{ sub1 , sub2 } ^>= 42" :: Maybe Dependency
-- Just (Dependency (PackageName "mylib") (MajorBoundVersion (mkVersion [42])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub1") :| [LSubLibName (UnqualComponentName "sub2")])))
--
-- >>> simpleParsec "mylib:{ } ^>= 42" :: Maybe Dependency
-- Nothing
--
-- >>> traverse_ print (map simpleParsec ["mylib:mylib", "mylib:{mylib}", "mylib:{mylib,sublib}" ] :: [Maybe Dependency])
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LMainLibName :| [])))
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LMainLibName :| [])))
-- Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LMainLibName :| [LSubLibName (UnqualComponentName "sublib")])))
--
-- Spaces around colon are not allowed:
--
-- >>> map simpleParsec ["mylib: sub", "mylib :sub", "mylib: {sub1,sub2}", "mylib :{sub1,sub2}"] :: [Maybe Dependency]
-- [Nothing,Nothing,Nothing,Nothing]
--
-- Sublibrary syntax is accepted since @cabal-version: 3.0@
--
-- >>> map (`simpleParsec'` "mylib:sub") [CabalSpecV2_4, CabalSpecV3_0] :: [Maybe Dependency]
-- [Nothing,Just (Dependency (PackageName "mylib") (OrLaterVersion (mkVersion [0])) (fromNonEmpty (LSubLibName (UnqualComponentName "sub") :| [])))]
instance Parsec Dependency where
  parsec = do
    name <- parsec

    libs <- option mainLibSet $ do
      _ <- char ':'
      versionGuardMultilibs
      NES.singleton <$> parseLib <|> parseMultipleLibs

    spaces -- https://github.com/haskell/cabal/issues/5846
    ver <-
      parsec
      <|>
        ( do
            v <- pure anyVersion
            annotate (NSVersionRange v Nothing) IsInjected
            pure v
        )
    let dep = mkDependency name ver libs
    mapAnnotationKeys (NSDependency dep . Just)
    return dep
    where
      parseLib = LSubLibName <$> parsec
      parseMultipleLibs =
        between
          (char '{' *> spaces)
          (spaces *> char '}')
          (NES.fromNonEmpty <$> parsecCommaNonEmpty parseLib)

versionGuardMultilibs :: CabalParsing m => m ()
versionGuardMultilibs = do
  csv <- askCabalSpecVersion
  when (csv < CabalSpecV3_0) $
    fail $
      unwords
        [ "Sublibrary dependency syntax used."
        , "To use this syntax the package needs to specify at least 'cabal-version: 3.0'."
        , "Alternatively, if you are depending on an internal library, you can write"
        , "directly the library name as it were a package."
        ]
