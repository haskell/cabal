module Distribution.C2Hs ( fixDeps ) where

import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Distribution.C2Hs.Lexer
import           Distribution.ModuleName         (ModuleName, components)
import           Distribution.PackageDescription (PackageDescription)
import qualified Distribution.PackageDescription as PD
import           Distribution.Simple.Utils       (dieNoVerbosity,
                                                  findFileWithExtension,
                                                  withUTF8FileContents)
import           Distribution.Text               (simpleParse)
import           System.FilePath                 (joinPath)

fixDeps :: PackageDescription -> IO PackageDescription
fixDeps pd@PD.PackageDescription {
          PD.library = Just lib@PD.Library {
            PD.exposedModules = expMods,
            PD.libBuildInfo = bi@PD.BuildInfo {
              PD.hsSourceDirs = srcDirs,
              PD.otherModules = othMods
            }}} = do

  let findModule = findFileWithExtension [".chs"] srcDirs . joinPath . components

  mExpFiles <- traverse findModule expMods
  mOthFiles <- traverse findModule othMods

  let modDeps = zipWith (ModDep True []) expMods mExpFiles ++
                zipWith (ModDep False []) othMods mOthFiles
  modDeps <- traverse extractDeps modDeps
  let (othMods, expMods) = span (not . mdExposed) $ sortTopological modDeps
  return pd { PD.library = Just lib {
    PD.exposedModules = map mdOriginal (reverse expMods),
    PD.libBuildInfo = bi { PD.otherModules = map mdOriginal (reverse othMods) }
  }}
fixDeps pd = pure pd

data ModDep = ModDep {
  mdExposed  :: Bool,
  mdRequires :: [ModuleName],
  mdOriginal :: ModuleName,
  mdLocation :: Maybe FilePath
}

instance Show ModDep where
  show x = show (mdLocation x)

instance Eq ModDep where
  ModDep { mdOriginal = m1 } == ModDep { mdOriginal = m2 } = m1==m2
instance Ord ModDep where
  compare ModDep { mdOriginal = m1 } ModDep { mdOriginal = m2 } = compare m1 m2

extractDeps :: ModDep -> IO ModDep
extractDeps md@ModDep { mdLocation = Nothing } = return md
extractDeps md@ModDep { mdLocation = Just f } = withUTF8FileContents f $ \con -> do
  mods <- case getImports con of
        Right ms -> case traverse simpleParse ms of
            Just ms -> pure ms
            Nothing -> dieNoVerbosity ("Cannot parse module name in c2hs file " ++ f)
        Left err -> dieNoVerbosity ("Cannot parse c2hs import in " ++ f ++ ": " ++ err)
  return md { mdRequires = mods }

sortTopological :: [ModDep] -> [ModDep]
sortTopological ms = fst $ foldl visit (([]), S.empty) (mdOriginal <$> ms)
  where
  set = M.fromList (map (\m -> (mdOriginal m, m)) ms)
  visit (out,visited) m
    | m `S.member` visited = (out,visited)
    | otherwise = case m `M.lookup` set of
        Nothing -> (out, m `S.insert` visited)
        Just md -> (md:out', visited')
          where
            (out',visited') = foldl visit (out, m `S.insert` visited) (mdRequires md)
