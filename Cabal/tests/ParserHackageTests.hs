{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Prelude ()
import Prelude.Compat

import           Control.Monad                          (when, unless)
import           Data.Foldable
                 (for_, traverse_)
import           Data.List                              (isPrefixOf, isSuffixOf)
import           Data.Maybe                             (mapMaybe)
import           Data.Monoid                            (Sum (..))
import           Distribution.Simple.Utils              (fromUTF8LBS, ignoreBOM)
import           System.Directory
                 (getAppUserDataDirectory)
import           System.Environment                     (getArgs)
import           System.Exit                            (exitFailure)
import           System.FilePath                        ((</>))

import           Data.Orphans ()

import qualified Codec.Archive.Tar                      as Tar
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Map                               as Map
import qualified Distribution.PackageDescription.Parse  as ReadP
import qualified Distribution.PackageDescription.Parsec as Parsec
import qualified Distribution.Parsec.Parser             as Parsec
import qualified Distribution.Parsec.Types.Common       as Parsec
import qualified Distribution.ParseUtils                as ReadP

import           Distribution.Compat.Lens
import qualified Distribution.Types.BuildInfo.Lens                 as  L
import qualified Distribution.Types.GenericPackageDescription.Lens as  L
import qualified Distribution.Types.PackageDescription.Lens        as  L

#ifdef HAS_STRUCT_DIFF
import           DiffInstances ()
import           StructDiff
#endif

parseIndex :: Monoid a => (FilePath -> BSL.ByteString -> IO a) -> IO a
parseIndex action = do
    cabalDir  <- getAppUserDataDirectory "cabal"
    cfg       <- B.readFile (cabalDir </> "config")
    cfgFields <- either (fail . show) pure $ Parsec.readFields cfg
    let repos        = reposFromConfig cfgFields
        repoCache    = case lookupInConfig "remote-repo-cache" cfgFields of
            []        -> cabalDir </> "packages"  -- Default
            (rrc : _) -> rrc                      -- User-specified
        tarName repo = repoCache </> repo </> "01-index.tar"
    mconcat <$> traverse (parseIndex' action . tarName) repos


parseIndex' :: Monoid a => (FilePath -> BSL.ByteString -> IO a) -> FilePath -> IO a
parseIndex' action path = do
    putStrLn $ "Reading index from: " ++ path
    contents <- BSL.readFile path
    let entries = Tar.read contents
    Tar.foldEntries (\e m -> mappend <$> f e <*> m) (return mempty) (fail . show) entries

  where
    f entry = case Tar.entryContent entry of
        Tar.NormalFile contents _
            | ".cabal" `isSuffixOf` fpath -> action fpath contents
            | otherwise                   -> return mempty
        Tar.Directory -> return mempty
        _             -> putStrLn ("Unknown content in " ++ fpath) >> return mempty
     where
       fpath = Tar.entryPath entry

readFieldTest :: FilePath -> BSL.ByteString -> IO ()
readFieldTest fpath bsl = case Parsec.readFields $ bslToStrict bsl of
    Right _  -> return ()
    Left err -> putStrLn $ fpath ++ "\n" ++ show err

-- | Map with unionWith monoid
newtype M k v = M (Map.Map k v)
    deriving (Show)
instance (Ord k, Monoid v) => Monoid (M k v) where
    mempty = M Map.empty
    mappend (M a) (M b) = M (Map.unionWith mappend a b)

compareTest
    :: String  -- ^ prefix of first packages to start traversal
    -> FilePath -> BSL.ByteString -> IO (Sum Int, Sum Int, M Parsec.PWarnType (Sum Int))
compareTest pfx fpath bsl
    | not $ pfx `isPrefixOf` fpath   = mempty
    | otherwise = do
    let str = ignoreBOM $ fromUTF8LBS bsl

    putStrLn $ "::: " ++ fpath
    (readp, readpWarnings)  <- case ReadP.parseGenericPackageDescription str of
        ReadP.ParseOk ws x    -> return (x, ws)
        ReadP.ParseFailed err -> print err >> exitFailure
    traverse_ (putStrLn . ReadP.showPWarning fpath) readpWarnings

    let (warnings, errors, parsec') = Parsec.runParseResult $ Parsec.parseGenericPackageDescription (bslToStrict bsl)
    traverse_ (putStrLn . Parsec.showPWarning fpath) warnings
    traverse_ (putStrLn . Parsec.showPError fpath) errors
    parsec <- maybe (print readp >> exitFailure) return parsec'

    -- Old parser is broken for many descriptions, and other free text fields
    let readp0  = readp
            & L.packageDescription . L.description .~ ""
            & L.packageDescription . L.synopsis    .~ ""
            & L.packageDescription . L.maintainer  .~ ""
    let parsec0  = parsec
            & L.packageDescription . L.description .~ ""
            & L.packageDescription . L.synopsis    .~ ""
            & L.packageDescription . L.maintainer  .~ ""

    -- hs-source-dirs ".", old parser broken
    -- See e.g. http://hackage.haskell.org/package/hledger-ui-0.27/hledger-ui.cabal executable
    let parsecHsSrcDirs = parsec0 & toListOf (L.buildInfos . L.hsSourceDirs)
    let readpHsSrcDirs  = readp0  & toListOf (L.buildInfos . L.hsSourceDirs)
    let filterDotDirs   = filter (/= ".")

    let parsec1 = if parsecHsSrcDirs /= readpHsSrcDirs && fmap filterDotDirs parsecHsSrcDirs == readpHsSrcDirs
        then parsec0 & L.buildInfos . L.hsSourceDirs %~ filterDotDirs
        else parsec0

    -- Compare two parse results
    -- ixset-1.0.4 has invalid prof-options, it's the only exception!
    unless (readp0 == parsec1 || fpath == "ixset/1.0.4/ixset.cabal") $ do
#if HAS_STRUCT_DIFF
            prettyResultIO $ diff readp parsec
#else
            putStrLn "<<<<<<"
            print readp
            putStrLn "======"
            print parsec
            putStrLn ">>>>>>"
#endif
            exitFailure

    let readpWarnCount  = Sum (length readpWarnings)
    let parsecWarnCount = Sum (length warnings)

    when (readpWarnCount > parsecWarnCount) $ do
        putStrLn "There are more readpWarnings"
        -- hint has -- in brace syntax, readp thinks it's a section
        -- http://hackage.haskell.org/package/hint-0.3.2.3/revision/0.cabal
        unless ("/hint.cabal" `isSuffixOf` fpath) exitFailure

    let parsecWarnMap   = foldMap (\(Parsec.PWarning t _ _) -> M $ Map.singleton t 1) warnings
    return (readpWarnCount, parsecWarnCount, parsecWarnMap)

parseReadpTest :: FilePath -> BSL.ByteString -> IO ()
parseReadpTest fpath bsl = do
    let str = ignoreBOM $ fromUTF8LBS bsl
    case ReadP.parseGenericPackageDescription str of
        ReadP.ParseOk _ _     -> return ()
        ReadP.ParseFailed err -> putStrLn fpath >> print err >> exitFailure

parseParsecTest :: FilePath -> BSL.ByteString -> IO ()
parseParsecTest fpath bsl = do
    let bs = bslToStrict bsl
    let (_warnings, errors, parsec) = Parsec.runParseResult $ Parsec.parseGenericPackageDescription bs
    case parsec of
        Just _ -> return ()
        Nothing -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["read-field"]   -> parseIndex readFieldTest
        ["parse-readp"]  -> parseIndex parseReadpTest
        ["parse-parsec"] -> parseIndex parseParsecTest
        [pfx]            -> defaultMain pfx
        _                -> defaultMain ""
  where
    defaultMain pfx = do
        (Sum readpCount, Sum parsecCount, M warn) <- parseIndex (compareTest pfx)
        putStrLn $ "readp warnings: " ++ show readpCount
        putStrLn $ "parsec count:   " ++ show parsecCount
        for_ (Map.toList warn) $ \(t, Sum c) ->
            putStrLn $ " - " ++ show t ++ " : " ++ show c

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

bslToStrict :: BSL.ByteString -> B.ByteString
#if MIN_VERSION_bytestring(0,10,0)
bslToStrict = BSL.toStrict
#else
-- Not effective!
bslToStrict = B.concat . BSL.toChunks
#endif

-------------------------------------------------------------------------------
-- Index shuffling
-------------------------------------------------------------------------------

-- TODO: Use 'Cabal' for this?
reposFromConfig :: [Parsec.Field ann] -> [String]
reposFromConfig fields = takeWhile (/= ':') <$> mapMaybe f fields
  where
    f (Parsec.Field (Parsec.Name _ name) fieldLines)
        | B8.unpack name == "remote-repo" =
            Just $ fieldLinesToString fieldLines
    f (Parsec.Section (Parsec.Name _ name) [Parsec.SecArgName _ secName] _fieldLines)
        | B8.unpack name == "repository" =
            Just $ B8.unpack secName
    f _ = Nothing

-- | Looks up the given key in the cabal configuration file
lookupInConfig :: String -> [Parsec.Field ann] -> [String]
lookupInConfig key = mapMaybe f
  where
    f (Parsec.Field (Parsec.Name _ name) fieldLines)
        | B8.unpack name == key =
            Just $ fieldLinesToString fieldLines
    f _ = Nothing

fieldLinesToString :: [Parsec.FieldLine ann] -> String
fieldLinesToString fieldLines =
    B8.unpack $ B.concat $ bsFromFieldLine <$> fieldLines
  where
    bsFromFieldLine (Parsec.FieldLine _ bs) = bs
