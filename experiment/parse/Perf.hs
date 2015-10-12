module Main where

import IndexUtils

import qualified Distribution.ParseUtils as ParseUtils (readFields, ParseResult(..))
import Distribution.Simple.Utils (fromUTF8)

import qualified Parser
import qualified PostParser
import qualified Lexer
import qualified LexerMonad as Lexer

import Text.Parsec.Error (ParseError)

import System.Environment
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L

checkedFiles :: [FilePath]
checkedFiles =
  [
{-
Executable NameServer
  Main-Is:        NameServer.hs
  Other modules:  DSTM, NameService
-}
    "DSTM/0.1/DSTM.cabal"
  , "DSTM/0.1.1/DSTM.cabal"
  , "DSTM/0.1.2/DSTM.cabal"

{-
 default- extensions:
               ScopedTypeVariables,
-}

  , "control-monad-exception-mtl/0.10.3/control-monad-exception-mtl.cabal"

{-
  default-language:    Haskell2010
Test-Suite test-unify:
  hs-source-dirs: test
-}

  , "ds-kanren/0.2.0.0/ds-kanren.cabal"
  , "ds-kanren/0.2.0.1/ds-kanren.cabal"

-- Using {- as a comment
  , "ixset/1.0.4/ixset.cabal"

-- test-suite metric-tests:
  , "metric/0.1.4/metric.cabal"
  , "metric/0.2.0/metric.cabal"

--     impl(ghc >= 7.4):
  , "phasechange/0.1/phasechange.cabal"
  , "shelltestrunner/1.3/shelltestrunner.cabal"
  , "smartword/0.0.0.5/smartword.cabal"

-- unexpected character in input '\DEL'
  , "vacuum-opengl/0.0/vacuum-opengl.cabal"
  , "vacuum-opengl/0.0.1/vacuum-opengl.cabal"
  ]

-- NBSP files
{-
  , "Octree/0.5/Octree.cabal"
  , "hermit/0.1.8.0/hermit.cabal"
  , "oeis/0.3.0/oeis.cabal"
  , "oeis/0.3.1/oeis.cabal"
  , "oeis/0.3.2/oeis.cabal"
  , "oeis/0.3.3/oeis.cabal"
  , "oeis/0.3.4/oeis.cabal"
  , "oeis/0.3.5/oeis.cabal"
  , "oeis/0.3.6/oeis.cabal"
-}

-- Parser results
checkedFiles2 :: [FilePath]
checkedFiles2 =
  -- Curly braces
  -- There are a lot in descriptions, but HaTex have them in author field
  [ "HaTeX/3."
  , "HaTeX-meta/1."

  -- other-modules without colon
  , "graphs/0.1/graphs.cabal"
  , "graphs/0.2/graphs.cabal"

  -- hs-source-dirs:
  --   .
  , "hspec-expectations-pretty/0.1/"

  -- multiline copyright
  , "tn/1.0.0/tn.cabal"
  ]

main :: IO ()
main = do
  [which, n, indexfile] <- getArgs
  cabalFiles <- IndexUtils.readPackageIndexFile indexfile

  case which of
    "perf-baseline" -> print (length cabalFiles)

    "perf-old" -> let parse  = ParseUtils.readFields . fromUTF8 . LBS.unpack . snd
                      parsed = [ pkg | ParseUtils.ParseOk _ pkg <- map parse cabalFiles ]
                  in  print (length parsed)

    "perf-new" -> let parse  = Parser.readFields . toStrict . snd
                      parsed = [ pkg | Right pkg <- map parse cabalFiles ]
                   in  print (length parsed)

    "check-old" -> let parse  = ParseUtils.readFields . fromUTF8 . LBS.unpack . snd
                       parsed = [ (msg, f)
                                | (ParseUtils.ParseFailed msg, f) <-
                                  map (\f -> (parse f, f)) cabalFiles ]
                   in  case parsed of
                         []           -> print "all ok!"
                         ((msg, (name, f)):_) -> do print msg
                                                    LBS.putStr f

    "check-new" -> let parse  = Parser.readFields . toStrict . snd
                       parsed = [ (msg, f)
                                | (Left msg, f) <-
                                  map (\f -> (parse f, f)) cabalFiles ]
                   in  case drop (read n) $ filter (\(_, (name, _)) -> name `notElem` checkedFiles) parsed of
                         []           -> print "all ok!"
                         ((msg, (name, f)):_) -> do putStrLn name
                                                    print msg
                                                    LBS.putStr f

    "compare" -> let parseOld :: LBS.ByteString -> ParseUtils.ParseResult [Parser.Field ()]
                     parseOld = (fmap . fmap . fmap) (const ()) . fmap (PostParser.postProcessFields2 . PostParser.postProcessFields) . ParseUtils.readFields . fromUTF8 . LBS.unpack
                     parseNew :: LBS.ByteString -> Either ParseError [Parser.Field ()]
                     parseNew = (fmap . fmap . fmap) (const ()) . fmap PostParser.postProcessFields2 . Parser.readFields . toStrict
                     parsed   = [ (f, parseOld c, parseNew c) | (f, c) <- cabalFiles ]
                     parsed'  = [ (f, o, n) | (f, ParseUtils.ParseOk _ o, Right n) <- parsed, o /= n, all (\prefix -> not $ prefix `L.isPrefixOf` f) checkedFiles2 ]
                     printDiff (f, o, n) = do putStrLn f
                                              putStrLn (show (length o) ++ " " ++ show (length n))
                                              mapM_ id [ putStr "- " >> print o' >> putStr "+ " >> print n' | (o', n') <- zip o n, o' /= n' ]
                 in mapM_ printDiff $ take 1 parsed'

    "lexer-warnings" -> let parse  = Parser.readFields' . toStrict
                            parsed :: [(FilePath, [Lexer.LexWarning])]
                            parsed = [ (name, ws)
                                     | (name, bs) <- cabalFiles
                                     , let p = parse bs
                                     , isRight p
                                     , let Right (_, ws) = p
                                     ]
                            output (f, ws) | null ws   = return ()
                                           | otherwise = putStrLn f >> mapM_ print ws >> putStrLn "----"
                        in mapM_ output parsed


    "extract-fail" -> let parse  = Parser.readFields . toStrict . snd
                          parsed = [ f
                                   | (Left msg, f) <-
                                     map (\f -> (parse f, f)) cabalFiles ]
                      in  sequence_
                            [ LBS.writeFile ("fail/" ++ show n ++ ".cabal") (snd f)
                            | (f,n) <- zip parsed [0..] ]

  where
    toStrict = BS.concat . LBS.toChunks

isRight (Right _) = True
isRight _ = False
