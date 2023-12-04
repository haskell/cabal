{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if !MIN_VERSION_deepseq(1,4,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Main where

import Distribution.Compat.Semigroup
import Prelude ()
import Prelude.Compat

import Control.Applicative                         (many, (<**>), (<|>))
import Control.DeepSeq                             (NFData (..), force)
import Control.Exception                           (evaluate)
import Control.Monad                               (join, unless, when)
import Data.Foldable                               (traverse_)
import Data.List                                   (isPrefixOf, isSuffixOf)
import Data.Maybe                                  (mapMaybe)
import Data.Monoid                                 (Sum (..))
import Distribution.PackageDescription.Check       (PackageCheck (..), checkPackage)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.PackageDescription.Quirks      (patchQuirks)
import Distribution.Simple.Utils                   (fromUTF8BS, toUTF8BS)
import Numeric                                     (showFFloat)
import System.Directory                            (getXdgDirectory, XdgDirectory(XdgCache, XdgConfig), getAppUserDataDirectory, doesDirectoryExist)
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
import qualified Options.Applicative                    as O
import qualified System.Clock                           as Clock

import           Distribution.Compat.Lens
import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.PackageDescription.Lens        as L

-- import Distribution.Types.BuildInfo                (BuildInfo (cppOptions))
-- import qualified Distribution.Types.BuildInfo.Lens                 as L

#ifdef MIN_VERSION_tree_diff
import Data.TreeDiff                 (ediff)
import Data.TreeDiff.Instances.Cabal ()
import Data.TreeDiff.Pretty          (ansiWlEditExprCompact)
#endif

-------------------------------------------------------------------------------
-- parseIndex: Index traversal
-------------------------------------------------------------------------------

parseIndex :: (Monoid a, NFData a) => (FilePath -> Bool)
           -> (FilePath -> B.ByteString -> IO a) -> IO a
parseIndex predicate action = do
    configPath <- getCabalConfigPath
    cfg        <- B.readFile configPath
    cfgFields  <- either (fail . show) pure $ Parsec.readFields cfg
    repoCache  <- case lookupInConfig "remote-repo-cache" cfgFields of
                    []        -> getCacheDirPath   -- Default
                    (rrc : _) -> return rrc        -- User-specified
    let repos        = reposFromConfig cfgFields
        tarName repo = repoCache </> repo </> "01-index.tar"
    mconcat <$> traverse (parseIndex' predicate action . tarName) repos
  where
    getCacheDirPath =
        getXdgDirectory XdgCache $ "cabal" </> "packages"
    getCabalConfigPath = do
        mx <- lookupEnv "CABAL_CONFIG"
        case mx of
            Just x  -> return x
            Nothing -> do
              mDir <- maybeGetCabalDir
              case mDir of
                Nothing -> getXdgDirectory XdgConfig $ "cabal" </> "config"
                Just dir -> return $ dir </> "config"
    maybeGetCabalDir :: IO (Maybe FilePath)
    maybeGetCabalDir = do
      mDir <- lookupEnv "CABAL_DIR"
      case mDir of
        Just dir -> return $ Just dir
        Nothing -> do
          defaultDir <- getAppUserDataDirectory "cabal"
          dotCabalExists <- doesDirectoryExist defaultDir
          return $ if dotCabalExists
                   then Just defaultDir
                   else Nothing


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

parseParsecTest :: Bool -> FilePath -> B.ByteString -> IO ParsecResult
parseParsecTest keepGoing fpath bs = do
    let (warnings, result) = Parsec.runParseResult $
                             Parsec.parseGenericPackageDescription bs

    let w | null warnings = 0
          | otherwise     = 1

    case result of
        Right gpd                    -> do
            forEachGPD fpath bs gpd
            return (ParsecResult 1 w 0)

        Left (_, errors) | keepGoing -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            return (ParsecResult 1 w 1)
                         | otherwise -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

-- | A hook to make queries on Hackage
forEachGPD :: FilePath -> B8.ByteString -> L.GenericPackageDescription -> IO ()
forEachGPD _ _ _ = return ()

-------------------------------------------------------------------------------
-- ParsecResult
-------------------------------------------------------------------------------

data ParsecResult = ParsecResult !Int !Int !Int
  deriving (Eq, Show)

instance Semigroup ParsecResult where
    ParsecResult x y z <> ParsecResult u v w = ParsecResult (x + u) (y + v) (z + w)

instance Monoid ParsecResult where
    mempty  = ParsecResult 0 0 0
    mappend = (<>)

instance NFData ParsecResult where
    rnf (ParsecResult _ _ _) = ()

-------------------------------------------------------------------------------
-- Check test
-------------------------------------------------------------------------------

parseCheckTest :: FilePath -> B.ByteString -> IO CheckResult
parseCheckTest fpath bs = do
    let (warnings, parsec) = Parsec.runParseResult $
                             Parsec.parseGenericPackageDescription bs
    case parsec of
        Right gpd -> do
            let checks = checkPackage gpd
            let w [] = 0
                w _  = 1

            -- Look into invalid cpp options
            -- _ <- L.traverseBuildInfos checkCppFlags gpd

            -- one for file, many checks
            return (CheckResult 1 (w warnings) 0 0 0 0 0 0 <> foldMap toCheckResult checks)
        Left (_, errors) -> do
            traverse_ (putStrLn . Parsec.showPError fpath) errors
            exitFailure

-- checkCppFlags :: BuildInfo -> IO BuildInfo
-- checkCppFlags bi = do
--     for_ (cppOptions bi) $ \opt ->
--         unless (any (`isPrefixOf` opt) ["-D", "-U", "-I"]) $
--             putStrLn opt
--
--     return bi

data CheckResult = CheckResult !Int !Int !Int !Int !Int !Int !Int !Int

instance NFData CheckResult where
    rnf !_ = ()

instance Semigroup CheckResult where
    CheckResult n w a b c d e f <> CheckResult n' w' a' b' c' d' e' f' =
        CheckResult (n + n') (w + w') (a + a') (b + b') (c + c') (d + d') (e + e') (f + f')

instance Monoid CheckResult where
    mempty = CheckResult 0 0 0 0 0 0 0 0
    mappend = (<>)

toCheckResult :: PackageCheck -> CheckResult
toCheckResult PackageBuildImpossible {}    = CheckResult 0 0 1 1 0 0 0 0
toCheckResult PackageBuildWarning {}       = CheckResult 0 0 1 0 1 0 0 0
toCheckResult PackageDistSuspicious {}     = CheckResult 0 0 1 0 0 1 0 0
toCheckResult PackageDistSuspiciousWarn {} = CheckResult 0 0 1 0 0 0 1 0
toCheckResult PackageDistInexcusable {}    = CheckResult 0 0 1 0 0 0 0 1

-------------------------------------------------------------------------------
-- Roundtrip test
-------------------------------------------------------------------------------

roundtripTest :: Bool -> FilePath -> B.ByteString -> IO (Sum Int)
roundtripTest testFieldsTransform fpath bs = do
    x0 <- parse "1st" bs
    let bs' = showGenericPackageDescription x0
    y0 <- parse "2nd" (toUTF8BS bs')

    -- strip description, there are format variations
    let y = y0 & L.packageDescription . L.description .~ mempty
    let x = x0 & L.packageDescription . L.description .~ mempty

    assertEqual' bs' x y

    -- fromParsecField, "shallow" parser/pretty roundtrip
    when testFieldsTransform $
        if checkUTF8 patchedBs
        then do
            parsecFields <- assertRight $ Parsec.readFields patchedBs
            let prettyFields = PP.fromParsecFields parsecFields
            let bs'' = PP.showFields (return PP.NoComment) prettyFields
            z0 <- parse "3rd" (toUTF8BS bs'')

            -- note: we compare "raw" GPDs, on purpose; stricter equality
            assertEqual' bs'' x0 z0
        else
            putStrLn $ fpath ++ " : looks like invalid UTF8"

    return (Sum 1)
  where
    patchedBs = snd (patchQuirks bs)

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
        putStrLn "====== tree-diff:"
        print $ ansiWlEditExprCompact $ ediff x y
#else
        putStrLn "<<<<<<"
        print x
        putStrLn "======"
        print y
        putStrLn ">>>>>>"
#endif
        putStrLn "====== contents:"
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
        parsecA (mkPredicate ["k"]) False

    readFieldsP = readFieldsA <$> prefixP
    readFieldsA pfx = parseIndex pfx readFieldTest

    parsecP = parsecA <$> prefixP <*> keepGoingP
    keepGoingP =
        O.flag' True  (O.long "keep-going") <|>
        O.flag' False (O.long "no-keep-going") <|>
        pure False

    parsecA pfx keepGoing = do
        begin <- Clock.getTime Clock.Monotonic
        ParsecResult n w f <- parseIndex pfx (parseParsecTest keepGoing)
        end <- Clock.getTime Clock.Monotonic
        let diff = Clock.toNanoSecs $ Clock.diffTimeSpec end begin

        putStrLn $ show n ++ " files processed"
        putStrLn $ show w ++ " files contained warnings"
        putStrLn $ show f ++ " files failed to parse"
        putStrLn $ showFFloat (Just 6) (fromInteger diff / 1e9                  :: Double) " seconds elapsed"
        putStrLn $ showFFloat (Just 6) (fromInteger diff / 1e6 / fromIntegral n :: Double) " milliseconds per file"

    roundtripP = roundtripA <$> prefixP <*> testFieldsP
    roundtripA pfx testFieldsTransform = do
        Sum n <- parseIndex pfx (roundtripTest testFieldsTransform)
        putStrLn $ show n ++ " files processed"

    checkP = checkA <$> prefixP
    checkA pfx = do
        CheckResult n w x a b c d e <- parseIndex pfx parseCheckTest
        putStrLn $ show n ++ " files processed"
        putStrLn $ show w ++ " files have lexer/parser warnings"
        putStrLn $ show x ++ " files have check warnings"
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
foldIO f = go mempty where
    go !acc [] = acc
    go !acc (x:xs) = go (mappend acc (f x)) xs

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

#if !MIN_VERSION_deepseq(1,4,0)
instance NFData a => NFData (Sum a) where
    rnf (Sum a)  = rnf a
#endif
