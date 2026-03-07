{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Solver.Types.ProjectConfigPath
    (
    -- * Project Config Path Manipulation
      ProjectConfigPath(..)
    , PCPLeaf(..)
    , projectConfigPathRoot
    , nullProjectConfigPath
    , consProjectConfigPath
    , unconsProjectConfigPath
    , currentProjectConfigPath

    -- * parsers and conversions
    , parseLeaf
    , leafToEither

    -- * Messages
    , docProjectConfigPath
    , docProjectImportedBy
    , docProjectConfigFiles
    , cyclicalImportMsg
    , untrimmedUriImportMsg
    , docProjectConfigPathFailReason
    , quoteUntrimmed

    -- * Checks and Normalization
    , isCyclicConfigPath
    , isTopLevelConfigPath
    , canonicalizeConfigPath
    ) where

import Distribution.Compat.Orphans ()
import Distribution.Solver.Compat.Prelude hiding (length, toList, last, head, nub, (<>))
import qualified Distribution.Solver.Compat.Prelude as P ((<>))
import Prelude (length, sequence)

import Data.List (nub)
import Data.List.NonEmpty ((<|))
import Network.URI (parseAbsoluteURI, URI)
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
data ProjectConfigPath = PCPWithImports PCPLeaf (NonEmpty FilePath)
                       | PCPWithoutImports FilePath
    deriving (Eq, Show, Generic)

-- | The last importee, which can be a local filepath
-- or a remote URI.
data PCPLeaf = PCPFilePath FilePath
             | PCPURI URI
    deriving (Eq, Show, Generic)

parseLeaf :: String -> PCPLeaf
parseLeaf (trim -> str) = case parseAbsoluteURI str of
                            Just uri -> PCPURI uri
                            _ -> PCPFilePath str

leafToEither :: PCPLeaf -> Either URI FilePath
leafToEither (PCPFilePath fp) = Right fp
leafToEither (PCPURI uri) = Left uri

instance Pretty PCPLeaf where
  pretty (PCPFilePath f) = quoteUntrimmed f
  pretty (PCPURI uri) = text $ show uri

instance Ord PCPLeaf where
    compare leaf_a leaf_b =
        case (leaf_a, leaf_b) of
                (PCPURI ua,     PCPURI ub)     -> compare ua ub
                (PCPURI _,      PCPFilePath _) -> GT
                (PCPFilePath _, PCPURI _)      -> LT
                (PCPFilePath a, PCPFilePath b) -> compare (splitPath a) (splitPath b)

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
    compare (PCPWithoutImports pa) (PCPWithoutImports pb) = compare (splitPath pa) (splitPath pb)
    compare (PCPWithImports _ _) (PCPWithoutImports _) = GT
    compare (PCPWithoutImports _) (PCPWithImports _ _) = LT
    compare (PCPWithImports leaf_a roots_a) (PCPWithImports leaf_b roots_b) =
        case (leaf_a, leaf_b) of
                (PCPURI ua,     PCPURI ub)     -> compare ua ub P.<> compareRoots
                (PCPURI _,      PCPFilePath _) -> GT
                (PCPFilePath _, PCPURI _)      -> LT
                (PCPFilePath a, PCPFilePath b) -> compare (splitPath a) (splitPath b) P.<> compareRoots

        where
            compareRoots =
                    case (NE.toList roots_a, NE.toList roots_b) of
                      -- There should only ever be one root project path, only one path
                      -- with length 1. Comparing it to itself should be EQ. Don't assume
                      -- this though, do a comparison anyway when both sides have length
                      -- 1.  The root path, the project itself, should always be the first
                      -- path in a sorted listing.
                      ([a], [b]) -> compare (splitPath a) (splitPath b)
                      ([_], _) -> LT
                      (_, [_]) -> GT

                      _ ->
                          compare (length roots_a) (length roots_b)
                          P.<> compare (length aPaths) (length bPaths)
                          P.<> compare aPaths bPaths


            aPaths = splitPath <$> roots_a
            bPaths = splitPath <$> roots_b

splitPath :: FilePath -> [FilePath]
splitPath = FP.splitPath . normSep where
    normSep p =
        if buildOS == Windows
            then
                Windows.joinPath $ Windows.splitDirectories
                [if Posix.isPathSeparator c then Windows.pathSeparator else c| c <- p]
            else
                Posix.joinPath $ Posix.splitDirectories
                [if Windows.isPathSeparator c then Posix.pathSeparator else c| c <- p]

instance Binary PCPLeaf
instance Binary ProjectConfigPath
instance NFData PCPLeaf
instance NFData ProjectConfigPath
instance Structured PCPLeaf
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
docProjectConfigPath (PCPWithoutImports p) = quoteUntrimmed p
docProjectConfigPath (PCPWithImports leaf (NE.toList -> roots)) = vcat $ pretty leaf :
    [ text " " <+> text "imported by:" <+> quoteUntrimmed l | l <- roots ]

-- | Render the paths which imports this config.
docProjectImportedBy :: ProjectConfigPath -> Doc
docProjectImportedBy (PCPWithoutImports _) = text ""
docProjectImportedBy (PCPWithImports _ (NE.toList -> ps)) = vcat $
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
    [ text "-" <+> pretty leaf
    | leaf <- ordNub [ currentProjectConfigPath p | p <- ps ]
    ]

-- | A message for a cyclical import, a "cyclical import of".
cyclicalImportMsg :: ProjectConfigPath -> Doc
cyclicalImportMsg (PCPWithoutImports _) = text ""
cyclicalImportMsg path@(PCPWithImports duplicate _) =
    vcat
    [ text "cyclical import of" <+> pretty duplicate <> semi
    , nest 2 (docProjectConfigPath path)
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
    | PCPWithoutImports p <- pcp =
        constraint (PCPFilePath p)
    | PCPWithImports p (NE.toList -> ps) <- pcp = vcat
        [ constraint p
        , cat [nest 2 $ text "imported by:" <+> text l | l <- ps ]
        ]
    where
        pathRequiresVersion p = pretty p <+> text "requires" <+> text (prettyShow vr)
        constraint p = parens $ text "constraint from" <+> pathRequiresVersion p

-- | The root of the path, the project itself.
--
-- Let's say @cabal.project@ imports @cabal.project.common@, which
-- imports @cabal.project.windows@. Then we have a path, such as:
--
-- > ("cabal.project.windows" :| ["cabal.project.common", "cabal.project])
--
-- And we want @cabal.project@.
projectConfigPathRoot :: ProjectConfigPath -> FilePath
projectConfigPathRoot (PCPWithoutImports fp)  = fp
projectConfigPathRoot (PCPWithImports _ roots) = NE.last roots

-- | The "current" project file of the path, the project itself.
--
-- Let's say @cabal.project@ imports @cabal.project.common@, which
-- imports @cabal.project.windows@. Then we have a path, such as:
--
-- > ("cabal.project.windows" :| ["cabal.project.common", "cabal.project])
--
-- And we want @cabal.project.windows@.
currentProjectConfigPath :: ProjectConfigPath -> PCPLeaf
currentProjectConfigPath (PCPWithImports leaf _) = leaf
currentProjectConfigPath (PCPWithoutImports fp)  = PCPFilePath fp

-- | Used by some tests as a dummy "unused" project root.
nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = PCPWithoutImports "unused"

-- | Check if the path has duplicates. A cycle of imports is not allowed. This
-- check should only be done after the path has been canonicalized with
-- @canonicalizeConfigPath@. This is because the import path may contain paths
-- that are the same in relation to their importers but different in relation to
-- the project root directory.
isCyclicConfigPath :: ProjectConfigPath -> Bool
isCyclicConfigPath (PCPWithoutImports _) = False
isCyclicConfigPath (PCPWithImports (PCPFilePath fp_leaf) (NE.toList -> roots)) =
  let p = fp_leaf : roots
  in length p /= length (nub p)
isCyclicConfigPath (PCPWithImports _ (NE.toList -> roots)) = length roots /= length (nub roots)

-- | Check if the project config path is top-level, meaning it was not included by
-- some other project config.
isTopLevelConfigPath :: ProjectConfigPath -> Bool
isTopLevelConfigPath (PCPWithoutImports _) = True
isTopLevelConfigPath _                     = False

-- | Prepends the path of the importee to the importer path.
consProjectConfigPath :: PCPLeaf -> ProjectConfigPath -> Maybe ProjectConfigPath
consProjectConfigPath leaf (PCPWithoutImports fp) = Just $ PCPWithImports leaf (fp :| [])
consProjectConfigPath leaf (PCPWithImports (PCPFilePath fp) roots) = Just $ PCPWithImports leaf (fp <| roots)
-- a project file pointed to by a URI must not import other files
consProjectConfigPath _ _ = Nothing

-- | Split the path into the importee and the importer path.
unconsProjectConfigPath :: ProjectConfigPath -> (PCPLeaf, Maybe ProjectConfigPath)
unconsProjectConfigPath (PCPWithoutImports fp)               = (PCPFilePath fp, Nothing)
unconsProjectConfigPath (PCPWithImports leaf (root :| []))   = (leaf, Just (PCPWithoutImports root))
unconsProjectConfigPath (PCPWithImports leaf (root :| rest)) = (leaf, Just (PCPWithImports (PCPFilePath root) (NE.fromList rest)))

-- | Make paths relative to the directory of the root of the project, not
-- relative to the file they were imported from.
makeRelativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
makeRelativeConfigPath dir (PCPWithoutImports p)
  = PCPWithoutImports $ makeRelative dir p
makeRelativeConfigPath dir (PCPWithImports (PCPFilePath fp) roots)
  = PCPWithImports (PCPFilePath $ makeRelative dir fp) ((makeRelative dir) <$> roots)
makeRelativeConfigPath dir (PCPWithImports (PCPURI uri) roots)
  = PCPWithImports (PCPURI uri) ((makeRelative dir) <$> roots)

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
canonicalizeConfigPath d (PCPWithoutImports p) = do
  can <- canonicalizePath p
  pure . makeRelativeConfigPath d . PCPWithoutImports $ can
canonicalizeConfigPath d (PCPWithImports leaf roots) = do
    newRoots <- sequence $ NE.scanr (\importee -> (>>= \importer ->
                canonicalizePath $ d </> takeDirectory importer </> importee))
            (pure ".") roots
    newLeaf <- case leaf of
                 PCPFilePath f -> PCPFilePath <$> canonicalizePath (d </> takeDirectory (NE.head newRoots) </> f)
                 PCPURI uri -> pure $ PCPURI uri
    pure . makeRelativeConfigPath d . PCPWithImports newLeaf . NE.fromList $ NE.init newRoots


-- $setup
-- >>> import Data.List
-- >>> testDir <- makeAbsolute =<< canonicalizePath "../cabal-testsuite/PackageTests/ConditionalAndImport"
