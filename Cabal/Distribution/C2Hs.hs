{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Distribution.C2Hs ( reorderC2Hs ) where

import Prelude()
import Data.Functor (($>))
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
            Nothing -> warn v ("Cannot parse module name in c2hs file " ++ f) $> []
        Left err -> warn v ("Cannot parse c2hs import in " ++ f ++ ": " ++ err) $> []
    pure (N m m mods)
