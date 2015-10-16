module Main where

import           Control.Applicative
                 (Applicative (..), (<$>))
import           Control.Monad                          (when)
import           Data.Char                              (isSpace)
import           Data.Foldable
                 (foldMap, for_, traverse_)
import           Data.List                              (isPrefixOf, isSuffixOf)
import           Data.Maybe                             (catMaybes)
import           Data.Monoid                            (Monoid (..), Sum (..))
import           Data.Traversable                       (traverse)
import           Distribution.Simple.Utils              (fromUTF8LBS)
import           System.Directory
                 (getAppUserDataDirectory)
import           System.Environment                     (getArgs)
import           System.Exit                            (exitFailure)
import           System.FilePath                        ((</>))


import           Distribution.PackageDescription

import qualified Codec.Archive.Tar                      as Tar
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Map                               as Map
import qualified Distribution.PackageDescription.Parse  as ReadP
import qualified Distribution.PackageDescription.Parsec as Parsec
import qualified Distribution.Parsec.Parser             as Parsec
import qualified Distribution.Parsec.Types.Common       as Parsec
import qualified Distribution.Parsec.Types.ParseResult  as Parsec
import qualified Distribution.ParseUtils                as ReadP

import           DiffInstances ()
import           StructDiff

parseIndex :: Monoid a => (FilePath -> BSL.ByteString -> IO a) -> IO a
parseIndex action = do
    c <- getAppUserDataDirectory "cabal"
    cfg <- readFile (c </> "config")
    let repos        = reposFromConfig cfg
        repoCache    = case lookupInConfig "remote-repo-cache" cfg of
            []        -> c </> "packages"  -- Default
            (rrc : _) -> rrc               -- User-specified
        tarName repo = repoCache </> repo </> "00-index.tar"
    mconcat <$> traverse (parseIndex' action . tarName) repos


parseIndex' :: Monoid a => (FilePath -> BSL.ByteString -> IO a) -> FilePath -> IO a
parseIndex' action path = do
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
readFieldTest fpath bsl = case Parsec.readFields $ BSL.toStrict bsl of
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
    | any ($ fpath) problematicFiles = mempty
    | fpath < pfx                    = mempty
    | otherwise = do
    let str = fromUTF8LBS bsl

    putStrLn $ "::: " ++ fpath
    (readp, readpWarnings)  <- case ReadP.parsePackageDescription str of
        ReadP.ParseOk ws x    -> return (x, ws)
        ReadP.ParseFailed err -> print err >> exitFailure
    traverse_ (putStrLn . ReadP.showPWarning fpath) readpWarnings

    let (warnings, errors, parsec') = Parsec.runParseResult $ Parsec.parseGenericPackageDescription (BSL.toStrict bsl)
    traverse_ (putStrLn . Parsec.showPWarning fpath) warnings
    traverse_ (putStrLn . Parsec.showPError fpath) errors
    parsec <- maybe (print readp >> exitFailure) return parsec'

    -- Old parser is broken for many descriptions, and other free text fields
    let readp0  = readp  { packageDescription = (packageDescription readp)  { description = "", synopsis = "", maintainer = "" }}
    let parsec0 = parsec { packageDescription = (packageDescription parsec) { description = "", synopsis = "", maintainer = "" }}

    if readp0 == parsec0
        then return ()
        else do
            {-
            putStrLn "<<<<<<"
            print readp
            putStrLn "======"
            print parsec
            putStrLn ">>>>>>"
            -}
            prettyResultIO $ diff readp parsec
            exitFailure

    let readpWarnCount  = Sum (length readpWarnings)
    let parsecWarnCount = Sum (length warnings)

    when (readpWarnCount > parsecWarnCount) $ do
        putStrLn "There are more readpWarnings"
        exitFailure

    let parsecWarnMap   = foldMap (\(Parsec.PWarning t _ _) -> M $ Map.singleton t 1) warnings
    return (readpWarnCount, parsecWarnCount, parsecWarnMap)

parseReadpTest :: FilePath -> BSL.ByteString -> IO ()
parseReadpTest fpath bsl = when (not $ any ($ fpath) problematicFiles) $ do
    let str = fromUTF8LBS bsl
    case ReadP.parsePackageDescription str of
        ReadP.ParseOk _ _     -> return ()
        ReadP.ParseFailed err -> print err >> exitFailure

parseParsecTest :: FilePath -> BSL.ByteString -> IO ()
parseParsecTest fpath bsl = when (not $ any ($ fpath) problematicFiles) $ do
    let bs = BSL.toStrict bsl
    let (_warnings, errors, parsec) = Parsec.runParseResult $ Parsec.parseGenericPackageDescription bs
    case parsec of
        Just _ -> return ()
        Nothing -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

problematicFiles :: [FilePath -> Bool]
problematicFiles =
    -- used tag in version
    [
    -- Indent failure
      eq "control-monad-exception-mtl/0.10.3/control-monad-exception-mtl.cabal"
    -- readp parser fails (description: {} )
    , eq "Cardinality/0.1/Cardinality.cabal"
    , eq "Cardinality/0.2/Cardinality.cabal"
    , eq "ConfigFileTH/0.1/ConfigFileTH.cabal"
    -- Other modules <- no dash
    , eq "DSTM/0.1/DSTM.cabal"
    , eq "DSTM/0.1.1/DSTM.cabal"
    , eq "DSTM/0.1.2/DSTM.cabal"
    -- executable
    {-
    , eq "DefendTheKing/0.1/DefendTheKing.cabal"
    , eq "DefendTheKing/0.2/DefendTheKing.cabal"
    , eq "DefendTheKing/0.2.1/DefendTheKing.cabal"
    , isPrefixOf "HaLeX/"
    , isPrefixOf "PerfectHash/"
    , isPrefixOf "hburg/"
    , isPrefixOf "hemkay/"
    , isPrefixOf "historian/"
    , isPrefixOf "hp2any-graph/"
    , isPrefixOf "hp2any-manager/"
    -}
    -- hs-source-dirs:
    --   .
    , isPrefixOf "writer-cps-mtl/"
    , isPrefixOf "writer-cps-monads-tf/"
    , isPrefixOf "writer-cps-transformers/"
    -- unexpected character in input '\194' - nbsp
    , eq "Octree/0.5/Octree.cabal"
    , eq "hermit/0.1.8.0/hermit.cabal"
    , eq "oeis/0.3.0/oeis.cabal"
    , eq "oeis/0.3.1/oeis.cabal"
    , eq "oeis/0.3.2/oeis.cabal"
    , eq "oeis/0.3.3/oeis.cabal"
    , eq "oeis/0.3.4/oeis.cabal"
    , eq "oeis/0.3.5/oeis.cabal"
    , eq "oeis/0.3.6/oeis.cabal"
    , eq "oeis/0.3.7/oeis.cabal"
    -- colon : after section header
    , eq "ds-kanren/0.2.0.0/ds-kanren.cabal"
    , eq "ds-kanren/0.2.0.1/ds-kanren.cabal"
    , eq "metric/0.1.4/metric.cabal"
    , eq "metric/0.2.0/metric.cabal"
    , eq "phasechange/0.1/phasechange.cabal"
    , eq "shelltestrunner/1.3/shelltestrunner.cabal"
    , eq "smartword/0.0.0.5/smartword.cabal"
    -- \DEL
    , eq "vacuum-opengl/0.0/vacuum-opengl.cabal"
    , eq "vacuum-opengl/0.0.1/vacuum-opengl.cabal"
    -- dashes in version, not even tag
    , isPrefixOf "free-theorems-webui/"
    -- whitespace difference in x-fields
    {-
    , isPrefixOf "gtk/"
    , isPrefixOf "hsqml/"
    , isPrefixOf "hsqml-datamodel/"
    , isPrefixOf "lhae/"
    , isPrefixOf "vte/"
    -}
    -- hs-source-dirs ".", old parser broken
    , isPrefixOf "hledger-ui/"
    , eq "hspec-expectations-pretty/0.1/hspec-expectations-pretty.cabal"
    -- {- comment -}
    , eq "ixset/1.0.4/ixset.cabal"
    ]
  where
    eq = (==)

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
-- Index shuffling
-------------------------------------------------------------------------------

-- TODO: Use 'Cabal' for this?
reposFromConfig :: String -> [String]
reposFromConfig = map (takeWhile (/= ':')) . lookupInConfig "remote-repo"

-- | Looks up the given key in the cabal configuration file
lookupInConfig :: String -> String -> [String]
lookupInConfig key = map trim . catMaybes . map (dropPrefix prefix) . lines
  where
    prefix = key ++ ":"

-- | Utility: drop leading and trailing spaces from a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix prefix s =
  if prefix `isPrefixOf` s
  then Just . drop (length prefix) $ s
  else Nothing
