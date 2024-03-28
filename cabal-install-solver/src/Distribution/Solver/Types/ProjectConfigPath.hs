{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Solver.Types.ProjectConfigPath
    (
    -- * Project Config Path Manipulation
      ProjectConfigPath(..)
    , projectConfigPathRoot
    , nullProjectConfigPath
    , consProjectConfigPath

    -- * Messages
    , docProjectConfigPath
    , cyclicalImportMsg
    , docProjectConfigPathFailReason

    -- * Checks and Normalization
    , isCyclicConfigPath
    , canonicalizeConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList, (<>))
import Prelude (sequence)

import Data.Coerce (coerce)
import Data.List.NonEmpty ((<|))
import Network.URI (parseURI)
import System.Directory
import System.FilePath
import qualified Data.List.NonEmpty as NE
import Distribution.Solver.Modular.Version (VR)
import Distribution.Pretty (prettyShow)
import Text.PrettyPrint

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
    deriving (Eq, Ord, Show, Generic)

instance Binary ProjectConfigPath
instance Structured ProjectConfigPath

-- | Renders the path like this;
-- @
-- D.config
--   imported by: C.config
--   imported by: B.config
--   imported by: A.project
-- @
-- >>> render . docProjectConfigPath $ ProjectConfigPath $ "D.config" :| ["C.config", "B.config", "A.project" ]
-- "D.config\n  imported by: C.config\n  imported by: B.config\n  imported by: A.project"
docProjectConfigPath :: ProjectConfigPath -> Doc
docProjectConfigPath (ProjectConfigPath (p :| [])) = text p
docProjectConfigPath (ProjectConfigPath (p :| ps)) = vcat $
    text p : [ text " " <+> text "imported by:" <+> text l | l <- ps ]

-- | A message for a cyclical import, assuming the head of the path is the
-- duplicate.
cyclicalImportMsg :: ProjectConfigPath -> Doc
cyclicalImportMsg path@(ProjectConfigPath (duplicate :| _)) =
    vcat
    [ text "cyclical import of" <+> text duplicate <> semi
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

-- | Prepends the path of the importee to the importer path.
consProjectConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
consProjectConfigPath p ps = ProjectConfigPath (p <| coerce ps)

-- | Make paths relative to the directory of the root of the project, not
-- relative to the file they were imported from.
makeRelativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
makeRelativeConfigPath dir (ProjectConfigPath p) =
    ProjectConfigPath
    $ (\segment -> (if isURI segment then segment else makeRelative dir segment))
    <$> p

-- | Normalizes and canonicalizes a path removing '.' and '..' indirections.
-- Makes the path relative to the given directory (typically the project root)
-- instead of relative to the file it was imported from.
--
-- It converts paths like this:
-- @
-- └─ hops-0.project
--    └─ hops/hops-1.config
--       └─ ../hops-2.config
--          └─ hops/hops-3.config
--             └─ ../hops-4.config
--                └─ hops/hops-5.config
--                   └─ ../hops-6.config
--                      └─ hops/hops-7.config
--                         └─ ../hops-8.config
--                            └─ hops/hops-9.config
-- @
--
-- Into paths like this:
-- @
-- └─ hops-0.project
--    └─ hops/hops-1.config
--       └─ hops-2.config
--          └─ hops/hops-3.config
--             └─ hops-4.config
--                └─ hops/hops-5.config
--                   └─ hops-6.config
--                      └─ hops/hops-7.config
--                         └─ hops-8.config
--                            └─ hops/hops-9.config
-- @
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
canonicalizeConfigPath :: FilePath -> ProjectConfigPath -> IO ProjectConfigPath
canonicalizeConfigPath d (ProjectConfigPath p) = do
    xs <- sequence $ NE.scanr (\importee -> (>>= \importer ->
            if isURI importee
                then pure importee
                else canonicalizePath $ d </> takeDirectory importer </> importee))
            (pure ".") p
    return . makeRelativeConfigPath d . ProjectConfigPath . NE.fromList $ NE.init xs

isURI :: FilePath -> Bool
isURI = isJust . parseURI

-- $setup
-- >>> import Data.List
-- >>> testDir <- makeAbsolute =<< canonicalizePath "../cabal-testsuite/PackageTests/ConditionalAndImport"
