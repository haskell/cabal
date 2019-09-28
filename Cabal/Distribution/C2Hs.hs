{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module contains a function to order @.chs@ files based on their
-- dependencies on one another thusly: it lexes the @.chs@ source files
-- looking for @{\# import \#}@ declarations and then topologically sorts
-- the modules. This ensures that a module's dependencies are preprocessed
-- first.
module Distribution.C2Hs ( reorderC2Hs ) where

import Prelude()
import Distribution.Compat.Graph
import Distribution.Compat.Prelude
import Distribution.C2Hs.Lexer
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Utils (warn, findFileWithExtension)
import Distribution.Verbosity (Verbosity)

-- | Given a list of 'ModuleName's, sort it according to @c2hs@ @{\#import\#}@
-- declarations.
reorderC2Hs :: Verbosity
            -> [FilePath] -- ^ Source directories
            -> [ModuleName] -- ^ Module names
            -> IO [ModuleName] -- ^ Sorted modules
reorderC2Hs v dirs preMods = do

    chsFiles <- traverse findCHS preMods

    modDeps <- traverse (extractDeps v) (zip preMods chsFiles)

    pure $ fmap (\(N m _ _) -> m) (revTopSort $ fromDistinctList modDeps)

        where findCHS = findFileWithExtension [".chs"] dirs . toFilePath


-- | Given a 'ModuleName' and its corresponding filepath, return a 'Node'
-- with its associated @c2hs@ dependencies
extractDeps :: Verbosity -> (ModuleName, Maybe FilePath) -> IO (Node ModuleName ModuleName)
-- If the 'FilePath' is 'Nothing', it's not a @.chs@ file
extractDeps _ (m, Nothing) = pure (N m m [])
extractDeps v (m, Just f) = do
    con <- readFile f
    mods <- case getImports con of
        Right ms -> case traverse simpleParsec ms of
            Just ms' -> pure ms'
            Nothing -> do { warn v ("Cannot parse module name in c2hs file " ++ f) ; pure [] }
        Left err -> do { warn v ("Cannot parse c2hs import in " ++ f ++ ": " ++ err) ; pure [] }
    pure (N m m mods)
