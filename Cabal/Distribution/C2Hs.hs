{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- Based on https://github.com/gtk2hs/gtk2hs/blob/master/tools/src/Gtk2HsSetup.hs#L414
module Distribution.C2Hs ( reorderC2Hs ) where

import Prelude()
import Data.Functor (($>))
import Distribution.Compat.Graph
import Distribution.Compat.Prelude
import Distribution.C2Hs.Lexer
import Distribution.ModuleName (ModuleName, components)
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Utils (warn, findFileWithExtension)
import Distribution.Verbosity (Verbosity)
import System.FilePath (joinPath)

-- | Given a list of 'ModuleName's, sort it according to @c2hs@ @{#import#}@
-- declarations.
reorderC2Hs :: Verbosity
            -> [FilePath] -- ^ Source directories
            -> [ModuleName] -- ^ Module names
            -> IO [ModuleName]
reorderC2Hs v dirs preMods = do

    let findModule = findFileWithExtension [".chs"] dirs . joinPath . components

    mFiles <- traverse findModule preMods

    let preDeps = zip (fmap (\m -> N m m []) preMods) mFiles

    modDeps <- traverse (extractDeps v) preDeps

    let mods = reverse (topSort $ fromDistinctList modDeps)

    pure (fmap (\(N m _ _) -> m) mods)

extractDeps :: Verbosity -> (Node ModuleName ModuleName, Maybe FilePath) -> IO (Node ModuleName ModuleName)
extractDeps _ (md, Nothing) = pure md
extractDeps v (N m m' _, Just f) = do
    con <- readFile f
    mods <- case getImports con of
        Right ms -> case traverse simpleParsec ms of
            Just ms' -> pure ms'
            Nothing -> warn v ("Cannot parse module name in c2hs file " ++ f) $> []
        Left err -> warn v ("Cannot parse c2hs import in " ++ f ++ ": " ++ err) $> []
    pure (N m m' mods)
