module Main where

import           Control.Applicative
                 (Applicative (..), (<$>))
import           Control.Monad                          (when)
import           Data.Foldable
                 (foldMap, for_, traverse_)
import           Data.List                              (isPrefixOf, isSuffixOf)
import           Data.Maybe                             (mapMaybe)
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
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Map                               as Map
import qualified Distribution.PackageDescription.Parse  as ReadP
import qualified Distribution.PackageDescription.Parsec as Parsec
import qualified Distribution.Parsec.Parser             as Parsec
import qualified Distribution.Parsec.Types.Common       as Parsec
import qualified Distribution.Parsec.Types.ParseResult  as Parsec
import qualified Distribution.ParseUtils                as ReadP

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
    | not $ pfx `isPrefixOf` fpath   = mempty
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
#if HAS_STRUCT_DIFF
            prettyResultIO $ diff readp parsec
#else
            putStrLn "<<<<<<"
            print readp0
            putStrLn "======"
            print parsec0
            putStrLn ">>>>>>"
#endif
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
    [
    -- Indent failure
      eq "control-monad-exception-mtl/0.10.3/control-monad-exception-mtl.cabal"
    -- Other modules <- no dash
    , eq "DSTM/0.1/DSTM.cabal"
    , eq "DSTM/0.1.1/DSTM.cabal"
    , eq "DSTM/0.1.2/DSTM.cabal"
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
    , isPrefixOf "writer-cps-mtl/"
    , isPrefixOf "writer-cps-monads-tf/"
    , isPrefixOf "writer-cps-transformers/"
    -- {- comment -}
    , eq "ixset/1.0.4/ixset.cabal"
    -- comments in braces
    , isPrefixOf "hint/"
    -- something weird: FromString "unrecognised field or section: \"\\65279\"" (Just 1)
    , eq "Workflow/0.8.3/Workflow.cabal"
    , eq "dictionary-sharing/0.1.0.0/dictionary-sharing.cabal"
    , eq "testing-type-modifiers/0.1.0.0/testing-type-modifiers.cabal"
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
