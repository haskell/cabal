{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Project configuration imports.
module Distribution.Client.ProjectConfig.Import
  ( -- * Parsing skeleton
    ProjectConfigSkeleton
  , projectSkeletonImports
  , fetchImportParse

    -- * Messages
  , docProjectConfigFiles
  , cyclicalImportMsg
  , untrimmedUriImportMsg
  ) where

import Control.Arrow (Kleisli (..), arr, (>>>))
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Distribution.Client.Compat.Prelude hiding (empty, (<>))
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig.Types
import Distribution.Compat.Lens (view)
import Distribution.PackageDescription (ConfVar (..))
import Distribution.Simple.Utils (debug, ordNub)
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Types.CondTree (CondTree (..), traverseCondTreeA)
import Distribution.Utils.String (trim)
import Network.URI (URI (..), parseURI)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, isPathSeparator, makeValid, (</>))
import Text.PrettyPrint

-- | ProjectConfigSkeleton is a tree of conditional blocks and imports wrapping
-- a config. It can be finalized by providing the conditional resolution info
-- and then resolving and downloading the imports
type ProjectConfigSkeleton = CondTree ConfVar ([ProjectConfigPath], ProjectConfig)

projectSkeletonImports :: ProjectConfigSkeleton -> [ProjectConfigPath]
projectSkeletonImports = fst . view traverseCondTreeA

-- | Fetch a local file import or remote URL import and parse it.
fetchImportParse
  :: (ProjectConfigToParse -> IO a)
  -> FilePath
  -> HttpTransport
  -> Verbosity
  -> FilePath
  -> ProjectConfigPath
  -> IO a
fetchImportParse parser cacheDir httpTransport verbosity projectDir normLocPath =
  fetchImportConfig normLocPath >>= runKleisli (arr ProjectConfigToParse >>> Kleisli parser) . snd
  where
    fetchImportConfig :: ProjectConfigPath -> IO (Maybe URI, BS.ByteString)
    fetchImportConfig (ProjectConfigPath (pci :| _)) = do
      debug verbosity $ "fetching import: " ++ pci
      let mbUri = parseURI (trim pci)
      (mbUri,) <$> case mbUri of
        Just uri -> do
          let fp = cacheDir </> map (\x -> if isPathSeparator x then '_' else x) (makeValid $ show uri)
          createDirectoryIfMissing True cacheDir
          _ <- downloadURI httpTransport verbosity uri fp
          BS.readFile fp
        Nothing ->
          BS.readFile $
            if isAbsolute pci then pci else coerce projectDir </> pci

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
docProjectConfigFiles ps =
  vcat
    [ text "-" <+> text p
    | p <- ordNub [p | ProjectConfigPath (p :| _) <- ps]
    ]

-- | A message for a cyclical import, a "cyclical import of".
cyclicalImportMsg :: ProjectConfigPath -> Doc
cyclicalImportMsg path@(ProjectConfigPath (duplicate :| _)) =
  vcat
    [ text "cyclical import of" <+> text duplicate <> semi
    , nest 2 (docProjectConfigPath path)
    ]

-- | A message for an import that has leading or trailing spaces.
untrimmedUriImportMsg :: Doc -> ProjectConfigPath -> Doc
untrimmedUriImportMsg intro path =
  vcat
    [ intro <+> text "import has leading or trailing whitespace" <> semi
    , nest 2 (docProjectConfigPath path)
    ]
