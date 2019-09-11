-- Based on https://github.com/gtk2hs/gtk2hs/blob/master/tools/src/Gtk2HsSetup.hs#L414
module Distribution.C2Hs ( reorderC2Hs ) where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Distribution.C2Hs.Lexer
import Distribution.ModuleName (ModuleName, components)
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Utils (dieNoVerbosity, findFileWithExtension, withUTF8FileContents)
import System.FilePath (joinPath)

reorderC2Hs :: [FilePath] -> [ModuleName] -> IO [ModuleName]
reorderC2Hs dirs preMods = do

  let findModule = findFileWithExtension [".chs"] dirs . joinPath . components

  mFiles <- traverse findModule preMods

  let preDeps = zip (fmap (ModDep []) preMods) mFiles

  modDeps <- traverse extractDeps preDeps

  let mods = reverse (sortTopological modDeps)

  pure (moduleOriginal <$> mods)

data ModDep = ModDep { moduleRequires :: [ModuleName]
                     , moduleOriginal :: ModuleName
                     }

extractDeps :: (ModDep, Maybe FilePath) -> IO ModDep
extractDeps (md, Nothing) = pure md
extractDeps (md, (Just f)) = withUTF8FileContents f $ \con -> do
  mods <- case getImports con of
        Right ms -> case traverse simpleParsec ms of
            Just ms' -> pure ms'
            Nothing -> dieNoVerbosity ("Cannot parse module name in c2hs file " ++ f)
        Left err -> dieNoVerbosity ("Cannot parse c2hs import in " ++ f ++ ": " ++ err)
  pure (md { moduleRequires = mods })

sortTopological :: [ModDep] -> [ModDep]
sortTopological ms = fst $ foldl' visit (([]), S.empty) (moduleOriginal <$> ms)
  where
    set = M.fromList (fmap (\m -> (moduleOriginal m, m)) ms)
    visit (out, visited) m
      | m `S.member` visited = (out,visited)
      | otherwise = case m `M.lookup` set of
          Nothing -> (out, m `S.insert` visited)
          Just md -> (md:out', visited')
            where
              (out',visited') = foldl' visit (out, m `S.insert` visited) (moduleRequires md)
