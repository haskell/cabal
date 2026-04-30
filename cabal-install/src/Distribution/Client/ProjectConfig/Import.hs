{-# LANGUAGE TupleSections #-}

-- | Project configuration imports.
module Distribution.Client.ProjectConfig.Import
  ( -- * Parsing skeleton
    ProjectConfigSkeleton
  , projectSkeletonImports
  , fetchImportParse
  ) where

import Control.Arrow (Kleisli (..), arr, (>>>))
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Distribution.Client.Compat.Prelude
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig.Types
import Distribution.Compat.Lens (view)
import Distribution.PackageDescription (ConfVar (..))
import Distribution.Simple.Utils (debug)
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Types.CondTree (CondTree (..), traverseCondTreeA)
import Distribution.Utils.String (trim)
import Network.URI (URI (..), parseURI)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, isPathSeparator, makeValid, (</>))

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
