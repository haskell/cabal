{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Distribution.Types.BenchmarkStanza where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Language.Haskell.Extension
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compat.Newtype (Newtype, pack', unpack')
import Distribution.Compiler (CompilerFlavor (..), PerCompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.Parsec
import Distribution.Pretty (Pretty (..), prettyShow, showToken)
import Distribution.Types.Imports
import Distribution.Types.ConfVar
import Distribution.Types.CondTree
import Distribution.Types.BuildInfo
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.BenchmarkType
import Distribution.Types.BenchmarkInterface
import Distribution.Types.Benchmark
import Distribution.Utils.Path
import Distribution.Version (Version, VersionRange)

import qualified Data.ByteString.Char8 as BS8
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX as SPDX

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
  Just tt@(BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
    Nothing -> emptyBenchmark
    Just file -> 
        emptyBenchmark
          { benchmarkInterface = BenchmarkExeV10 ver file
          , benchmarkBuildInfo = _benchmarkStanzaBuildInfo stanza
          }
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

validateBenchmark :: Position -> BenchmarkStanza -> ParseResult src ()
validateBenchmark pos stanza = case _benchmarkStanzaBenchmarkType stanza of
  Nothing -> pure ()
  Just tt@(BenchmarkTypeUnknown _ _) -> pure ()
  Just tt | tt `notElem` knownBenchmarkTypes -> pure ()
  Just tt@(BenchmarkTypeExe ver) -> case _benchmarkStanzaMainIs stanza of
    Nothing -> parseFailure pos (missingField "main-is" tt)
    Just file ->
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
  stanza {
    _benchmarkStanzaBenchmarkType =
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
