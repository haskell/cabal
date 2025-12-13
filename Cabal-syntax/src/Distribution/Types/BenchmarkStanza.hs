{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Distribution.Types.BenchmarkStanza where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Fields
import Distribution.ModuleName (ModuleName)
import Distribution.Parsec
import Distribution.Pretty (prettyShow)
import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BenchmarkType
import Distribution.Types.BuildInfo
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Utils.Path

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza
  { _benchmarkStanzaBenchmarkType :: Maybe BenchmarkType
  , _benchmarkStanzaMainIs :: Maybe (RelativePath Source File)
  , _benchmarkStanzaBenchmarkModule :: Maybe ModuleName
  , _benchmarkStanzaBuildInfo :: BuildInfo
  }
  deriving (Show, Eq, Data, Generic)

instance Binary BenchmarkStanza
instance Structured BenchmarkStanza
instance NFData BenchmarkStanza where rnf = genericRnf

validateBenchmark :: Position -> BenchmarkStanza -> ParseResult src ()
validateBenchmark pos stanza = case _benchmarkStanzaBenchmarkType stanza of
  Nothing -> pure ()
  Just (BenchmarkTypeUnknown _ _) -> pure ()
  Just tt | tt `notElem` knownBenchmarkTypes -> pure ()
  Just tt@(BenchmarkTypeExe _ver) -> case _benchmarkStanzaMainIs stanza of
    Nothing -> parseFailure pos (missingField "main-is" tt)
    Just _file ->
      when (isJust (_benchmarkStanzaBenchmarkModule stanza)) $
        parseWarning pos PWTExtraBenchmarkModule (extraField "benchmark-module" tt)
  where
    missingField name tt =
      "The '"
        ++ name
        ++ "' field is required for the "
        ++ prettyShow tt
        ++ " benchmark type."

    extraField name tt =
      "The '"
        ++ name
        ++ "' field is not used for the '"
        ++ prettyShow tt
        ++ "' benchmark type."

convertBenchmark :: BenchmarkStanza -> Benchmark
convertBenchmark stanza = case _benchmarkStanzaBenchmarkType stanza of
  Nothing ->
    emptyBenchmark
      { benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
      }
  Just tt@(BenchmarkTypeUnknown _ _) ->
    emptyBenchmark
      { benchmarkInterface = BenchmarkUnsupported tt
      , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
      }
  Just tt
    | tt `notElem` knownBenchmarkTypes ->
        emptyBenchmark
          { benchmarkInterface = BenchmarkUnsupported tt
          , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
          }
  Just (BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
    Nothing -> emptyBenchmark
    Just file ->
      emptyBenchmark
        { benchmarkInterface = BenchmarkExeV10 ver file
        , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
        }

unvalidateBenchmark :: Benchmark -> BenchmarkStanza
unvalidateBenchmark b =
  BenchmarkStanza
    { _benchmarkStanzaBenchmarkType = ty
    , _benchmarkStanzaMainIs = ma
    , _benchmarkStanzaBenchmarkModule = mo
    , _benchmarkStanzaBuildInfo = benchmarkBuildInfo b
    }
  where
    (ty, ma, mo) = case benchmarkInterface b of
      BenchmarkExeV10 ver ma'
        | getSymbolicPath ma' == "" ->
            (Just $ BenchmarkTypeExe ver, Nothing, Nothing)
        | otherwise ->
            (Just $ BenchmarkTypeExe ver, Just ma', Nothing)
      _ -> (Nothing, Nothing, Nothing)

patchBenchmarkType :: CabalSpecVersion -> BenchmarkStanza -> BenchmarkStanza
patchBenchmarkType cabalSpecVersion stanza =
  stanza
    { _benchmarkStanzaBenchmarkType =
        _benchmarkStanzaBenchmarkType stanza <|> do
          guard (cabalSpecVersion >= CabalSpecV3_8)
          benchmarkTypeExe <$ _benchmarkStanzaMainIs stanza
    }

instance L.HasBuildInfo BenchmarkStanza where
  buildInfo = benchmarkStanzaBuildInfo

benchmarkStanzaBenchmarkType :: Lens' BenchmarkStanza (Maybe BenchmarkType)
benchmarkStanzaBenchmarkType f s = fmap (\x -> s{_benchmarkStanzaBenchmarkType = x}) (f (_benchmarkStanzaBenchmarkType s))
{-# INLINE benchmarkStanzaBenchmarkType #-}

benchmarkStanzaMainIs :: Lens' BenchmarkStanza (Maybe (RelativePath Source File))
benchmarkStanzaMainIs f s = fmap (\x -> s{_benchmarkStanzaMainIs = x}) (f (_benchmarkStanzaMainIs s))
{-# INLINE benchmarkStanzaMainIs #-}

benchmarkStanzaBenchmarkModule :: Lens' BenchmarkStanza (Maybe ModuleName)
benchmarkStanzaBenchmarkModule f s = fmap (\x -> s{_benchmarkStanzaBenchmarkModule = x}) (f (_benchmarkStanzaBenchmarkModule s))
{-# INLINE benchmarkStanzaBenchmarkModule #-}

benchmarkStanzaBuildInfo :: Lens' BenchmarkStanza BuildInfo
benchmarkStanzaBuildInfo f s = fmap (\x -> s{_benchmarkStanzaBuildInfo = x}) (f (_benchmarkStanzaBuildInfo s))
{-# INLINE benchmarkStanzaBuildInfo #-}
