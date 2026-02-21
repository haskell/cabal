{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Solver.Types.ProjectConfigPath
    (
    -- * Project Config Path Manipulation
      ProjectImport(..)
    , ProjectConfigPath(..)
    , projectConfigPathRoot
    , nullProjectConfigPath
    , consProjectConfigPath
    , unconsProjectConfigPath
    , currentProjectConfigPath

    -- * Messages
    , docProjectConfigPath
    , docProjectImportedBy
    , docProjectConfigFiles
    , cyclicalImportMsg
    , duplicateImportMsg
    , untrimmedUriImportMsg
    , docProjectConfigPathFailReason
    , quoteUntrimmed

    -- * Checks and Normalization
    , isCyclicConfigPath
    , isTopLevelConfigPath
    , isUntrimmedUriConfigPath
    , canonicalizeConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList, (<>))
import qualified Distribution.Solver.Compat.Prelude as P ((<>))
import Prelude (sequence)

import Data.Coerce (coerce)
import Data.List.NonEmpty ((<|))
import Network.URI (parseURI, parseAbsoluteURI)
import System.Directory
import System.FilePath hiding (splitPath)
import qualified System.FilePath as FP (splitPath)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import qualified Data.List.NonEmpty as NE
import Distribution.Solver.Modular.Version (VR)
import Distribution.Pretty (prettyShow, Pretty(..))
import Distribution.Utils.String (trim)
import Text.PrettyPrint
import Distribution.Simple.Utils (ordNub)
import Distribution.System (OS(Windows), buildOS)

data ProjectImport =
    ProjectImport
        { importOf :: FilePath
        , importBy :: ProjectConfigPath
        }
    deriving (Eq, Ord)

-- | Path to a configuration file, either a singleton project root, or a longer
-- list representing a path to an import.  The path is a non-empty list that we
-- build up by prepending relative imports with @consProjectConfigPath@.
--
-- An import can be a URI, such as [a stackage
-- cabal.config](https://www.stackage.org/nightly/cabal.config), but we do not
-- support URIs in the middle of the path, URIs that import other URIs, or URIs
-- that import local files.
--
-- List elements are relative to each other but once canonicalized, elements are
-- relative to the directory of the project root.
newtype ProjectConfigPath = ProjectConfigPath (NonEmpty FilePath)
    deriving (Eq, Show, Generic)

instance Pretty ProjectConfigPath where
  pretty = docProjectConfigPath

-- | Sorts URIs after local file paths and longer file paths after shorter ones
-- as measured by the number of path segments. If still equal, then sorting is
-- lexical.
--
-- The project itself, a single element root path, compared to any of the
-- configuration paths it imports, should always sort first. Comparing one
-- project root path against another is done lexically.
--
-- For comparison purposes, path separators are normalized to the @buildOS@
-- platform's path separator.
--
-- >>> let abFwd = ProjectConfigPath $ "a/b.config" :| []
-- >>> let abBwd = ProjectConfigPath $ "a\\b.config" :| []
-- >>> compare abFwd abBwd
-- EQ
instance Ord ProjectConfigPath where
    compare pa@(ProjectConfigPath (NE.toList -> as)) pb@(ProjectConfigPath (NE.toList -> bs)) =
        case (as, bs) of
            -- There should only ever be one root project path, only one path
            -- with length 1. Comparing it to itself should be EQ. Don't assume
            -- this though, do a comparison anyway when both sides have length
            -- 1.  The root path, the project itself, should always be the first
            -- path in a sorted listing.
            ([a], [b]) -> compare (splitPath a) (splitPath b)
            ([_], _) -> LT
            (_, [_]) -> GT

            (a:_, b:_) -> case (parseAbsoluteURI a, parseAbsoluteURI b) of
                (Just ua, Just ub) -> compare ua ub P.<> compare aImporters bImporters
                (Just _, Nothing) -> GT
                (Nothing, Just _) -> LT
                (Nothing, Nothing) -> compare (splitPath a) (splitPath b) P.<> compare aImporters bImporters
            _ ->
                compare (length as) (length bs)
                P.<> compare (length aPaths) (length bPaths)
                P.<> compare aPaths bPaths
        where
            splitPath = FP.splitPath . normSep where
                normSep p =
                    if buildOS == Windows
                        then
                            Windows.joinPath $ Windows.splitDirectories
                            [if Posix.isPathSeparator c then Windows.pathSeparator else c| c <- p]
                        else
                            Posix.joinPath $ Posix.splitDirectories
                            [if Windows.isPathSeparator c then Posix.pathSeparator else c| c <- p]

            aPaths = splitPath <$> as
            bPaths = splitPath <$> bs
            aImporters = snd $ unconsProjectConfigPath pa
            bImporters = snd $ unconsProjectConfigPath pb

instance Binary ProjectConfigPath
instance NFData ProjectConfigPath
instance Structured ProjectConfigPath

-- | Renders the path like this;
--
-- >D.config
-- >  imported by: C.config
-- >  imported by: B.config
-- >  imported by: A.project
--
-- >>> render . docProjectConfigPath $ ProjectConfigPath $ "D.config" :| ["C.config", "B.config", "A.project"]
-- "D.config\n  imported by: C.config\n  imported by: B.config\n  imported by: A.project"
docProjectConfigPath :: ProjectConfigPath -> Doc
docProjectConfigPath (ProjectConfigPath (p :| [])) = quoteUntrimmed p
docProjectConfigPath (ProjectConfigPath (p :| ps)) = vcat $ quoteUntrimmed p :
    [ text " " <+> text "imported by:" <+> quoteUntrimmed l | l <- ps ]

-- | Render the paths which imports this config.
docProjectImportedBy :: ProjectConfigPath -> Doc
docProjectImportedBy (ProjectConfigPath (_ :| [])) = text ""
docProjectImportedBy (ProjectConfigPath (_ :| ps)) = vcat $
    [ text " " <+> text "imported by:" <+> quoteUntrimmed l | l <- ps ]


-- | If the path has leading or trailing spaces then show it quoted.
quoteUntrimmed :: FilePath -> Doc
quoteUntrimmed s = if trim s /= s then quotes (text s) else text s

-- | Renders the paths as a list without showing which path imports another,
-- like this;
--
-- >- cabal.project
-- >- project-cabal/constraints.config
-- >- project-cabal/ghc-latest.config
-- >- project-cabal/ghc-options.config
-- >- project-cabal/pkgs.config
-- >- project-cabal/pkgs/benchmarks.config
-- >- project-cabal/pkgs/buildinfo.config
-- >- project-cabal/pkgs/cabal.config
-- >- project-cabal/pkgs/install.config
-- >- project-cabal/pkgs/integration-tests.config
-- >- project-cabal/pkgs/tests.config
--
--
-- >>> :{
--   do
--     let ps =
--              [ ProjectConfigPath ("cabal.project" :| [])
--              , ProjectConfigPath ("project-cabal/constraints.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/ghc-latest.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/ghc-options.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/benchmarks.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/buildinfo.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/cabal.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/install.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/integration-tests.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/tests.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              ]
--     return . render $ docProjectConfigFiles ps
-- :}
-- "- cabal.project\n- project-cabal/constraints.config\n- project-cabal/ghc-latest.config\n- project-cabal/ghc-options.config\n- project-cabal/pkgs.config\n- project-cabal/pkgs/benchmarks.config\n- project-cabal/pkgs/buildinfo.config\n- project-cabal/pkgs/cabal.config\n- project-cabal/pkgs/install.config\n- project-cabal/pkgs/integration-tests.config\n- project-cabal/pkgs/tests.config"
docProjectConfigFiles :: [ProjectConfigPath] -> Doc
docProjectConfigFiles ps = vcat
    [ text "-" <+> text p
    | p <- ordNub [ p | ProjectConfigPath (p :| _) <- ps ]
    ]

-- | A message for a cyclical import, a "cyclical import of".
cyclicalImportMsg :: ProjectConfigPath -> Doc
cyclicalImportMsg path@(ProjectConfigPath (duplicate :| _)) =
    seenImportMsg
        (text "cyclical import of" <+> text duplicate <> semi)
        (ProjectImport duplicate path)
        []

-- | A message for a duplicate import, a "duplicate import of". If a check for
-- cyclical imports has already been made then this would report a duplicate
-- import by two different paths.
duplicateImportMsg :: Doc -> ProjectImport -> [ProjectImport] -> Doc
duplicateImportMsg intro = seenImportMsg intro

seenImportMsg :: Doc -> ProjectImport -> [ProjectImport] -> Doc
seenImportMsg intro ProjectImport{importOf = duplicate, importBy = path} seenImports =
    vcat
    [ intro
    , nest 2 (docProjectConfigPath path)
    , nest 2 $
        vcat
        [ docProjectConfigPath importBy
        | ProjectImport{importBy} <- filter ((duplicate ==) . importOf) seenImports
        ]
    ]

-- | A message for an import that has leading or trailing spaces.
untrimmedUriImportMsg :: Doc -> ProjectConfigPath -> Doc
untrimmedUriImportMsg intro path =
    vcat
    [ intro <+> text "import has leading or trailing whitespace" <> semi
    , nest 2 (docProjectConfigPath path)
    ]

docProjectConfigPathFailReason :: VR -> ProjectConfigPath -> Doc
docProjectConfigPathFailReason vr pcp
    | ProjectConfigPath (p :| []) <- pcp =
        constraint p
    | ProjectConfigPath (p :| ps) <- pcp = vcat
        [ constraint p
        , cat [nest 2 $ text "imported by:" <+> text l | l <- ps ]
        ]
    where
        pathRequiresVersion p = text p <+> text "requires" <+> text (prettyShow vr)
        constraint p = parens $ text "constraint from" <+> pathRequiresVersion p

-- | The root of the path, the project itself.
projectConfigPathRoot :: ProjectConfigPath -> FilePath
projectConfigPathRoot (ProjectConfigPath xs) = last xs

-- | Used by some tests as a dummy "unused" project root.
nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = ProjectConfigPath $ "unused" :| []

-- | Check if the path has duplicates. A cycle of imports is not allowed. This
-- check should only be done after the path has been canonicalized with
-- @canonicalizeConfigPath@. This is because the import path may contain paths
-- that are the same in relation to their importers but different in relation to
-- the project root directory.
isCyclicConfigPath :: ProjectConfigPath -> Bool
isCyclicConfigPath (ProjectConfigPath p) = length p /= length (NE.nub p)

-- | Check if the last segment of the path (root or importee) is a URI that has
-- leading or trailing spaces.
isUntrimmedUriConfigPath :: ProjectConfigPath -> Bool
isUntrimmedUriConfigPath (ProjectConfigPath (p :| _)) = let p' = trim p in p' /= p && isURI p'

-- | Check if the project config path is top-level, meaning it was not included by
-- some other project config.
isTopLevelConfigPath :: ProjectConfigPath -> Bool
isTopLevelConfigPath (ProjectConfigPath p) = NE.length p == 1

-- | Prepends the path of the importee to the importer path.
consProjectConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
consProjectConfigPath p ps = ProjectConfigPath (p <| coerce ps)

-- | Split the path into the importee and the importer path.
unconsProjectConfigPath :: ProjectConfigPath -> (FilePath, Maybe ProjectConfigPath)
unconsProjectConfigPath ps = fmap ProjectConfigPath <$> NE.uncons (coerce ps)

currentProjectConfigPath :: ProjectConfigPath -> FilePath
currentProjectConfigPath (ProjectConfigPath (p :| _)) = p

-- | Make paths relative to the directory of the root of the project, not
-- relative to the file they were imported from.
makeRelativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
makeRelativeConfigPath dir (ProjectConfigPath p) =
    ProjectConfigPath
    $ (\segment@(trim -> trimSegment) -> (if isURI trimSegment then trimSegment else makeRelative dir segment))
    <$> p

-- | Normalizes and canonicalizes a path removing '.' and '..' indirections.
-- Makes the path relative to the given directory (typically the project root)
-- instead of relative to the file it was imported from.
--
-- It converts paths like this:
--
-- >   hops-0.project
-- >   └─ hops/hops-1.config
-- >      └─ ../hops-2.config
-- >         └─ hops/hops-3.config
-- >            └─ ../hops-4.config
-- >               └─ hops/hops-5.config
-- >                  └─ ../hops-6.config
-- >                     └─ hops/hops-7.config
-- >                        └─ ../hops-8.config
-- >                           └─ hops/hops-9.config
--
-- Into paths like this:
--
-- >   hops-0.project
-- >   └─ hops/hops-1.config
-- >      └─ hops-2.config
-- >         └─ hops/hops-3.config
-- >            └─ hops-4.config
-- >               └─ hops/hops-5.config
-- >                  └─ hops-6.config
-- >                     └─ hops/hops-7.config
-- >                        └─ hops-8.config
-- >                           └─ hops/hops-9.config
--
-- That way we have @hops-8.config@ instead of
-- @./hops/../hops/../hops/../hops/../hops-8.config@.
--
-- Let's see how @canonicalizePath@ works that is used in the implementation
-- then we'll see how @canonicalizeConfigPath@ works.
--
-- >>> let d = testDir
-- >>> makeRelative d <$> canonicalizePath (d </> "hops/../hops/../hops/../hops/../hops-8.config")
-- "hops-8.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ (d </> "hops/../hops/../hops/../hops/../hops-8.config") :| [])
-- >>> render $ docProjectConfigPath p
-- "hops-8.config"
--
-- >>> :{
--   do
--     let expected = unlines
--           [ "hops/hops-9.config"
--           , "  imported by: hops-8.config"
--           , "  imported by: hops/hops-7.config"
--           , "  imported by: hops-6.config"
--           , "  imported by: hops/hops-5.config"
--           , "  imported by: hops-4.config"
--           , "  imported by: hops/hops-3.config"
--           , "  imported by: hops-2.config"
--           , "  imported by: hops/hops-1.config"
--           , "  imported by: hops-0.project"
--           ]
--     let d = testDir
--     let configPath = ProjectConfigPath ("hops/hops-9.config" :|
--           [ "../hops-8.config"
--           , "hops/hops-7.config"
--           , "../hops-6.config"
--           , "hops/hops-5.config"
--           , "../hops-4.config"
--           , "hops/hops-3.config"
--           , "../hops-2.config"
--           , "hops/hops-1.config"
--           , d </> "hops-0.project"])
--     p <- canonicalizeConfigPath d configPath
--     return $ expected == render (docProjectConfigPath p) ++ "\n"
-- :}
-- True
--
-- "A string is a valid URL potentially surrounded by spaces if, after stripping leading and trailing whitespace from it, it is a valid URL."
-- [W3C/HTML5/URLs](https://www.w3.org/TR/2010/WD-html5-20100624/urls.html)
--
-- Trailing spaces for @ProjectConfigPath@ URLs are trimmed.
--
-- >>> p <- canonicalizeConfigPath "" (ProjectConfigPath $ ("https://www.stackage.org/nightly-2024-12-05/cabal.config ") :| [])
-- >>> render $ docProjectConfigPath p
-- "https://www.stackage.org/nightly-2024-12-05/cabal.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ ("https://www.stackage.org/nightly-2024-12-05/cabal.config ") :| [d </> "cabal.project"])
-- >>> render $ docProjectConfigPath p
-- "https://www.stackage.org/nightly-2024-12-05/cabal.config\n  imported by: cabal.project"
canonicalizeConfigPath :: FilePath -> ProjectConfigPath -> IO ProjectConfigPath
canonicalizeConfigPath d (ProjectConfigPath p) = do
    xs <- sequence $ NE.scanr (\importee@(trim -> trimImportee) -> (>>= \importer@(trim -> trimImporter) ->
            if isURI trimImportee || isURI trimImporter
                then pure trimImportee
                else canonicalizePath $ d </> takeDirectory importer </> importee))
            (pure ".") p
    return . makeRelativeConfigPath d . ProjectConfigPath . NE.fromList $ NE.init xs

isURI :: FilePath -> Bool
isURI = isJust . parseURI

-- $setup
-- >>> import Data.List
-- >>> testDir <- makeAbsolute =<< canonicalizePath "../cabal-testsuite/PackageTests/ConditionalAndImport"
