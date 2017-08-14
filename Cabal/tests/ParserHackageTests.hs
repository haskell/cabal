{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Prelude ()
import Prelude.Compat

import           Control.Applicative
                 (Const (..))
import           Control.Monad                          (when, unless)
import           Data.Foldable
                 (for_, traverse_)
import           Data.List                              (isPrefixOf, isSuffixOf)
import           Data.Maybe                             (mapMaybe, listToMaybe)
import           Data.Monoid                            (Sum (..))
import           Distribution.Simple.Utils              (fromUTF8LBS, ignoreBOM)
import           System.Directory
                 (getAppUserDataDirectory)
import           System.Environment                     (getArgs)
import           System.Exit                            (exitFailure)
import           System.FilePath                        ((</>))

import           Distribution.Types.Dependency
import           Distribution.Types.UnqualComponentName
import           Distribution.PackageDescription

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
import qualified Distribution.Compat.DList              as DList

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#else
import Unsafe.Coerce
#endif

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
    | any ($ fpath) problematicFiles = mempty
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
            & set (packageDescription_ .  description_) ""
            & set (packageDescription_ .  synopsis_)    ""
            & set (packageDescription_ .  maintainer_)  ""
    let parsec0  = parsec
            & set (packageDescription_ .  description_) ""
            & set (packageDescription_ .  synopsis_)    ""
            & set (packageDescription_ .  maintainer_)  ""

    -- hs-source-dirs ".", old parser broken
    -- See e.g. http://hackage.haskell.org/package/hledger-ui-0.27/hledger-ui.cabal executable
    let parsecHsSrcDirs = parsec0 & toListOf (buildInfos_ . hsSourceDirs_)
    let readpHsSrcDirs  = readp0  & toListOf (buildInfos_ . hsSourceDirs_)
    let filterDotDirs   = filter (/= ".")

    let parsec1 = if parsecHsSrcDirs /= readpHsSrcDirs && fmap filterDotDirs parsecHsSrcDirs == readpHsSrcDirs
        then parsec0 & over (buildInfos_ . hsSourceDirs_) filterDotDirs
        else parsec0

    -- Compare two parse results
    unless (readp0 == parsec1) $ do
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
        exitFailure

    let parsecWarnMap   = foldMap (\(Parsec.PWarning t _ _) -> M $ Map.singleton t 1) warnings
    return (readpWarnCount, parsecWarnCount, parsecWarnMap)

parseReadpTest :: FilePath -> BSL.ByteString -> IO ()
parseReadpTest fpath bsl = unless (any ($ fpath) problematicFiles) $ do
    let str = fromUTF8LBS bsl
    case ReadP.parseGenericPackageDescription str of
        ReadP.ParseOk _ _     -> return ()
        ReadP.ParseFailed err -> print err >> exitFailure

parseParsecTest :: FilePath -> BSL.ByteString -> IO ()
parseParsecTest fpath bsl = unless (any ($ fpath) problematicFiles) $ do
    let bs = bslToStrict bsl
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
    -- {- comment -}
    , eq "ixset/1.0.4/ixset.cabal"
    -- comments in braces
    , isPrefixOf "hint/"
    -- other-modules:\n  .
    , eq "unicode-transforms/0.3.3/unicode-transforms.cabal"
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

-------------------------------------------------------------------------------
-- Distribution.Compat.Lens
-------------------------------------------------------------------------------

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

type Getting r s a = (a -> Const r a) -> s -> Const r s
type ASetter' s a = (a -> I a) -> s -> I s



-- | View the value pointed to by a 'Getting' or 'Lens' or the
-- result of folding over all the results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points at a monoidal values.
view :: s -> Getting a s a -> a
view s l = getConst (l Const s)

-- | Replace the target of a 'Lens'' or 'Traversal'' with a constant value.
set :: ASetter' s a -> a -> s -> s
set l x = over l (const x)

-- | Modify the target of a 'Lens'' or all the targets of a 'Traversal''
-- with a function.
over :: ASetter' s a -> (a -> a) -> s -> s
#if __GLASGOW_HASKELL__ >= 708
over l f = coerce . l (coerce . f)
#else
over l f = unsafeCoerce . l (unsafeCoerce . f)
#endif

-- | Build a 'Lens'' from a getter and a setter.
lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens sa sbt afb s = sbt s <$> afb (sa s)

-- | Build an 'Getting' from an arbitrary Haskell function.
to :: (s -> a) -> Getting r s a
to f g a = Const $ getConst $ g (f a)

-- | Extract a list of the targets of a 'Lens'' or 'Traversal''.
toListOf :: Getting (DList.DList a) s a -> s -> [a]
toListOf l = DList.runDList . getConst . l (Const . DList.singleton)

-- | Retrieve the first entry of a 'Traversal'' or retrieve 'Just' the result
-- from a 'Getting' or 'Lens''.
firstOf :: Getting (DList.DList a) s a -> s -> Maybe a
firstOf l = listToMaybe . toListOf l

-- | '&' is a reverse application operator
(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINE (&) #-}
infixl 1 &

-------------------------------------------------------------------------------
-- Distribution.Compat.BasicFunctors
-------------------------------------------------------------------------------

newtype I a = I a

unI :: I a -> a
unI (I x) = x

instance Functor I where
    fmap f (I x) = I (f x)

instance Applicative I where
    pure        = I
    I f <*> I x = I (f x)
    _ *> x      = x

_2 :: Lens' (a, b) b
_2 = lens snd $ \(a, _) b -> (a, b)

-------------------------------------------------------------------------------
-- Distribution.PackageDescription.Lens
-------------------------------------------------------------------------------

packageDescription_ :: Lens' GenericPackageDescription PackageDescription
packageDescription_ = lens packageDescription $ \s a -> s { packageDescription = a }

condLibrary_ :: Lens' GenericPackageDescription (Maybe (CondTree ConfVar [Dependency] Library))
condLibrary_ = lens condLibrary $ \s a -> s { condLibrary = a}

condExecutables_ :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
condExecutables_ = lens condExecutables $ \s a -> s { condExecutables = a }

condTreeData_ :: Lens' (CondTree v c a) a
condTreeData_ = lens condTreeData $ \s a -> s { condTreeData = a }

description_, synopsis_, maintainer_ :: Lens' PackageDescription String
description_ = lens description $ \s a -> s { description = a }
synopsis_    = lens synopsis    $ \s a -> s { synopsis    = a }
maintainer_  = lens maintainer  $ \s a -> s { maintainer  = a }

class HasBuildInfo a where
    buildInfo_ :: Lens' a BuildInfo

instance HasBuildInfo Library where
    buildInfo_ = lens libBuildInfo $ \s a -> s { libBuildInfo = a }

instance HasBuildInfo Executable where
    buildInfo_ = lens buildInfo $ \s a -> s { buildInfo = a }

-- | This forgets a lot of structure, but might be nice for some stuff
buildInfos_ :: Traversal' GenericPackageDescription BuildInfo
buildInfos_ f gpd = mkGpd
    <$> (traverse . traverse . buildInfo_) f (condLibrary gpd)
    <*> (traverse . _2 . traverse . buildInfo_) f (condExecutables gpd)
  where
      mkGpd lib exe = gpd
          { condLibrary     = lib
          , condExecutables = exe
          }

hsSourceDirs_ :: Lens' BuildInfo [FilePath]
hsSourceDirs_ = lens hsSourceDirs $ \s a -> s { hsSourceDirs = a }
