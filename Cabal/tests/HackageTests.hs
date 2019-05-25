{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if !MIN_VERSION_deepseq(1,4,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

-- | The following RTS parameters seem to speed up running the test
--
-- @
-- +RTS -s -qg -I0 -A64M -N2 -RTS
-- @
--
-- * @-qg@ No parallel GC (you can try @-qn2@ on GHC-8.2+)
-- * @-I0@ No idle GC (shouldn't matter, but to be sure)
-- * @-A64M@ Set allocation area to about the maximum residence size tests have
-- * @-N4@ More capabilities (depends on your machine)
--
-- @-N1@ vs. @-N4@ gives
--
-- * @1m 48s@ to @1m 00s@ speedup for full Hackage @parsec@ test, and
--
-- * @6m 16s@ to @3m 30s@ speedup for full Hackage @roundtrip@ test.
--
-- i.e. not linear, but substantial improvement anyway.
--
module Main where

import Distribution.Compat.Semigroup
import Prelude ()
import Prelude.Compat

import Control.Applicative                         (many, (<**>), (<|>))
import Control.Concurrent
       (ThreadId, forkIO, getNumCapabilities, killThread, myThreadId, throwTo)
import Control.Concurrent.STM
import Control.DeepSeq                             (NFData (..), force)
import Control.Exception
       (AsyncException (ThreadKilled), SomeException, bracket, catch, evaluate, fromException,
       mask, throwIO)
import Control.Monad                               (forever, join, replicateM, unless, when)
import Data.Foldable                               (for_, traverse_)
import Data.IORef                                  (modifyIORef', newIORef, readIORef)
import Data.List                                   (isPrefixOf, isSuffixOf)
import Data.Maybe                                  (mapMaybe)
import Data.Monoid                                 (Sum (..))
import Distribution.PackageDescription.Check       (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.PackageDescription.Quirks      (patchQuirks)
import Distribution.Simple.Utils                   (fromUTF8BS, toUTF8BS)
import System.Directory                            (getAppUserDataDirectory)
import System.Environment                          (lookupEnv)
import System.Exit                                 (exitFailure)
import System.FilePath                             ((</>))

import Data.Orphans ()

import qualified Codec.Archive.Tar                      as Tar
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Data.ByteString.Lazy                   as BSL
import qualified Distribution.Fields.Parser             as Parsec
import qualified Distribution.Fields.Pretty             as PP
import qualified Distribution.PackageDescription.Parsec as Parsec
import qualified Distribution.Parsec                    as Parsec

import           Distribution.Compat.Lens
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens        as L
import qualified Options.Applicative                               as O

#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff        (ediff)
import Data.TreeDiff.Pretty (ansiWlEditExprCompact)
import Instances.TreeDiff ()
#endif

-------------------------------------------------------------------------------
-- parseIndex: Index traversal
-------------------------------------------------------------------------------

parseIndex :: (Monoid a, NFData a) => (FilePath -> Bool)
           -> (FilePath -> B.ByteString -> IO a) -> IO a
parseIndex predicate action = do
    cabalDir   <- getAppUserDataDirectory "cabal"
    configPath <- getCabalConfigPath cabalDir
    cfg        <- B.readFile configPath
    cfgFields  <- either (fail . show) pure $ Parsec.readFields cfg
    let repos        = reposFromConfig cfgFields
        repoCache    = case lookupInConfig "remote-repo-cache" cfgFields of
            []        -> cabalDir </> "packages"  -- Default
            (rrc : _) -> rrc                      -- User-specified
        tarName repo = repoCache </> repo </> "01-index.tar"
    mconcat <$> traverse (parseIndex' predicate action . tarName) repos
  where
    getCabalConfigPath cabalDir = do
        mx <- lookupEnv "CABAL_CONFIG"
        case mx of
            Just x  -> return x
            Nothing -> return (cabalDir </> "config")
    

parseIndex'
    :: (Monoid a, NFData a)
    => (FilePath -> Bool)
    -> (FilePath -> B.ByteString -> IO a) -> FilePath -> IO a
parseIndex' predicate action path = do
    putStrLn $ "Reading index from: " ++ path
    contents <- BSL.readFile path
    let entries = Tar.read contents
        entries' = Tar.foldEntries cons [] (error . show) entries
    foldIO f entries'

  where
    cons entry entries
        | predicate (Tar.entryPath entry) = entry : entries
        | otherwise = entries

    f entry = case Tar.entryContent entry of
        Tar.NormalFile contents _
            | ".cabal" `isSuffixOf` fpath -> do
                bs <- evaluate (BSL.toStrict contents)
                res <- action fpath bs
                evaluate (force res)
            | otherwise ->
                return mempty
        Tar.Directory -> return mempty
        _             -> putStrLn ("Unknown content in " ++ fpath)
                         >> return mempty
     where
       fpath = Tar.entryPath entry

-------------------------------------------------------------------------------
-- readFields tests: very fast test for 'readFields' - first step of parser
-------------------------------------------------------------------------------

readFieldTest :: FilePath -> B.ByteString -> IO ()
readFieldTest fpath bs = case Parsec.readFields bs' of
    Right _  -> return ()
    Left err -> do
        putStrLn fpath
        print err
        exitFailure
  where
    (_, bs') = patchQuirks bs

-------------------------------------------------------------------------------
-- Parsec test: whether we can parse everything
-------------------------------------------------------------------------------

parseParsecTest :: FilePath -> B.ByteString -> IO (Sum Int)
parseParsecTest fpath bs = do
    let (_warnings, parsec) = Parsec.runParseResult $
                              Parsec.parseGenericPackageDescription bs
    case parsec of
        Right _ -> return (Sum 1)
        Left (_, errors) -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

-------------------------------------------------------------------------------
-- Check test
-------------------------------------------------------------------------------

parseCheckTest :: FilePath -> B.ByteString -> IO CheckResult
parseCheckTest fpath bs = do
    let (_warnings, parsec) = Parsec.runParseResult $
                              Parsec.parseGenericPackageDescription bs
    case parsec of
        Right gpd -> do
            let checks = checkPackage gpd Nothing
            -- one for file, many checks
            return (CheckResult 1 0 0 0 0 0 <> foldMap toCheckResult checks)
        Left (_, errors) -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

data CheckResult = CheckResult !Int !Int !Int !Int !Int !Int

instance NFData CheckResult where
    rnf !_ = ()

instance Semigroup CheckResult where
    CheckResult n a b c d e <> CheckResult n' a' b' c' d' e' =
        CheckResult (n + n') (a + a') (b + b') (c + c') (d + d') (e + e')

instance Monoid CheckResult where
    mempty = CheckResult 0 0 0 0 0 0
    mappend = (<>)

toCheckResult :: PackageCheck -> CheckResult
toCheckResult PackageBuildImpossible {}    = CheckResult 0 1 0 0 0 0
toCheckResult PackageBuildWarning {}       = CheckResult 0 0 1 0 0 0
toCheckResult PackageDistSuspicious {}     = CheckResult 0 0 0 1 0 0
toCheckResult PackageDistSuspiciousWarn {} = CheckResult 0 0 0 0 1 0
toCheckResult PackageDistInexcusable {}    = CheckResult 0 0 0 0 0 1

-------------------------------------------------------------------------------
-- Roundtrip test
-------------------------------------------------------------------------------

roundtripTest :: Bool -> FilePath -> B.ByteString -> IO (Sum Int)
roundtripTest testFieldsTransform fpath bs = do
    x0 <- parse "1st" bs
    let bs' = showGenericPackageDescription x0
    y0 <- parse "2nd" (toUTF8BS bs')

    -- we mungled license here
    let y1 = y0

    -- license-files: ""
    let stripEmpty = filter (/="")
    let x1 = x0 & L.packageDescription . L.licenseFiles %~ stripEmpty
    let y2 = y1 & L.packageDescription . L.licenseFiles %~ stripEmpty

    let y = y2 & L.packageDescription . L.description .~ ""
    let x = x1 & L.packageDescription . L.description .~ ""

    assertEqual' bs' x y

    -- fromParsecField, "shallow" parser/pretty roundtrip
    when testFieldsTransform $
        if checkUTF8 bs
        then do
            parsecFields <- assertRight $ Parsec.readFields $ snd $ patchQuirks bs
            let prettyFields = PP.fromParsecFields parsecFields
            let bs'' = PP.showFields (return []) prettyFields
            z0 <- parse "3rd" (toUTF8BS bs'')

            -- note: we compare "raw" GPDs, on purpose; stricter equality
            assertEqual' bs'' x0 z0
        else
            putStrLn $ fpath ++ " : looks like invalid UTF8"

    return (Sum 1)
  where
    checkUTF8 bs' = replacementChar `notElem` fromUTF8BS bs' where
        replacementChar = '\xfffd'


    assertRight (Right x) = return x
    assertRight (Left err) = do
        putStrLn fpath
        print err
        exitFailure

    assertEqual' bs' x y = unless (x == y || fpath == "ixset/1.0.4/ixset.cabal") $ do
        putStrLn fpath
#ifdef MIN_VERSION_tree_diff
        print $ ansiWlEditExprCompact $ ediff x y
#else
        putStrLn "<<<<<<"
        print x
        putStrLn "======"
        print y
        putStrLn ">>>>>>"
#endif
        putStrLn bs'
        exitFailure

    parse phase c = do
        let (_, x') = Parsec.runParseResult $
                      Parsec.parseGenericPackageDescription c
        case x' of
            Right gpd -> pure gpd
            Left (_, errs) -> do
                putStrLn $ fpath ++ " " ++ phase
                traverse_ print errs
                B.putStr c
                fail "parse error"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = join (O.execParser opts)
  where
    opts = O.info (optsP <**> O.helper) $ mconcat
        [ O.fullDesc
        , O.progDesc "tests using Hackage's index"
        ]

    optsP = subparser
        [ command "read-fields" readFieldsP
          "Parse outer format (to '[Field]', TODO: apply Quirks)"
        , command "parsec"      parsecP     "Parse GPD with parsec"
        , command "roundtrip"   roundtripP  "parse . pretty . parse = parse"
        , command "check"       checkP      "Check GPD"
        ] <|> pure defaultA

    defaultA = do
        putStrLn "Default action: parsec k"
        parsecA (mkPredicate ["k"])

    readFieldsP = readFieldsA <$> prefixP
    readFieldsA pfx = parseIndex pfx readFieldTest

    parsecP = parsecA <$> prefixP
    parsecA pfx = do
        Sum n <- parseIndex pfx parseParsecTest
        putStrLn $ show n ++ " files processed"

    roundtripP = roundtripA <$> prefixP <*> testFieldsP
    roundtripA pfx testFieldsTransform = do
        Sum n <- parseIndex pfx (roundtripTest testFieldsTransform)
        putStrLn $ show n ++ " files processed"

    checkP = checkA <$> prefixP
    checkA pfx = do
        CheckResult n a b c d e <- parseIndex pfx parseCheckTest
        putStrLn $ show n ++ " files processed"
        putStrLn $ show a ++ " build impossible"
        putStrLn $ show b ++ " build warning"
        putStrLn $ show c ++ " build dist suspicious"
        putStrLn $ show d ++ " build dist suspicious warning"
        putStrLn $ show e ++ " build dist inexcusable"

    prefixP = fmap mkPredicate $ many $ O.strArgument $ mconcat
        [ O.metavar "PREFIX"
        , O.help "Check only files starting with a prefix"
        ]

    testFieldsP = O.switch $ mconcat
        [ O.long "fields-transform"
        , O.help "Test also 'showFields . fromParsecFields . readFields' transform"
        ]

    mkPredicate [] = const True
    mkPredicate pfxs = \n -> any (`isPrefixOf` n) pfxs

    command name p desc = O.command name
                          (O.info (p <**> O.helper) (O.progDesc desc))
    subparser = O.subparser . mconcat

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
    f (Parsec.Section (Parsec.Name _ name)
       [Parsec.SecArgName _ secName]    _fieldLines)
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
-- Utilities
-------------------------------------------------------------------------------

-- | We assume that monoid is commutative.
--
-- First we chunk input (as single cabal file is little work)
foldIO :: forall a m. (Monoid m, NFData m) => (a -> IO m) -> [a] -> IO m
foldIO f = foldIO' (g mempty) . chunks
  where
    chunks [] = []
    chunks xs = let ~(ys, zs) = splitAt 256 xs in ys : chunks zs

    -- strict foldM
    g :: m -> [a] -> IO m
    g !acc []     = return acc
    g !acc (x:xs) = f x >>= \ m -> g (mappend acc m) xs

-- | This 'parallelInterleaved' from @parallel-io@ but like (effectful) 'foldMap', not 'sequence'
foldIO' :: (Monoid m, NFData m) => (a -> IO m) -> [a] -> IO m
foldIO' f ys = do
    cap <- getNumCapabilities
    -- we leave one capability to do management (and read index)
    let cap' = max 1 (pred cap)

    tid <- myThreadId
    ref <- newIORef mempty

    withPool cap' $ \pool -> mask $ \restore -> do
        for_ ys $ \y -> submitToPool pool $ reflectExceptionsTo tid $ do
            m <- restore (f y)
            modifyIORef' ref (force . mappend m)

        readIORef ref
  where
    reflectExceptionsTo :: ThreadId -> IO () -> IO ()
    reflectExceptionsTo tid act = catchNonThreadKilled act (throwTo tid)

    catchNonThreadKilled :: IO a -> (SomeException -> IO a) -> IO a
    catchNonThreadKilled act handler = act `catch` \e -> case fromException e of Just ThreadKilled -> throwIO e; _ -> handler e

-------------------------------------------------------------------------------
-- Worker pool
-------------------------------------------------------------------------------

data Pool = Pool
    { poolThreadsN :: Int
    , poolThreads  :: [ThreadId]
    , poolQueue    :: TVar Queue
    , poolInflight :: TVar Int
    }

data Queue = Queue !Int [IO ()]

submitToPool ::  Pool -> IO () -> IO ()
submitToPool (Pool threadsN _ queue _) act = atomically $ do
    Queue n acts <- readTVar queue
    if n >= threadsN -- some work for every worker already in the queue
    then retry
    else writeTVar queue (Queue (succ n) (act : acts)) -- order is messed

withPool :: Int -> (Pool -> IO a) -> IO a
withPool n kont = do
    queue    <- newTVarIO (Queue 0 [])
    inflight <- newTVarIO 0
    bracket (replicateM n $ forkIO $ worker queue inflight) cleanup $ \threads -> do

        -- run work
        x <- kont (Pool n threads queue inflight)

        -- wait for jobs to complete
        atomically $ readTVar inflight >>= \m -> check (m <= 0)

        -- return
        return x
  where
    cleanup threads = for_ threads killThread

    -- worker pulls work from the queue in the loop
    worker queue inflight = forever $ bracket pull cleanupW id where
        pull = atomically $ do
            Queue actsN acts <- readTVar queue
            case acts of
                []           -> retry
                (act : acts') -> do
                    modifyTVar' inflight succ
                    writeTVar queue (Queue (pred actsN) acts')
                    return act

        cleanupW _ = atomically $ modifyTVar' inflight pred

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

#if !MIN_VERSION_deepseq(1,4,0)
instance NFData a => NFData (Sum a) where
    rnf (Sum a)  = rnf a
#endif
