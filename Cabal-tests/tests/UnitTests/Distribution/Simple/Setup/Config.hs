module UnitTests.Distribution.Simple.Setup.Config
  ( tests
  ) where

import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Simple.Command (CommandParse (..), commandParseArgs)
import Distribution.Simple.Compiler (DebugInfoLevel (..))
import Distribution.Simple.Flag qualified as Flag
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Simple.Setup
  ( ConfigFlags (..)
  , configureCommand
  , emptyConfigFlags
  )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Distribution.Simple.Setup.Config"
    [ testGroup
        "--ghc / -g compiler selection"
        [ testCase "--ghc selects GHC" $
            configHcFlavor (parse ["--ghc"]) @?= Flag.Flag GHC
        , testCase "--ghcjs selects GHCJS" $
            configHcFlavor (parse ["--ghcjs"]) @?= Flag.Flag GHCJS
        , testCase "-g no longer selects GHC" $
            configHcFlavor (parse ["-g"]) @?= Flag.NoFlag
        , testCase "--ghc -g selects GHC and does not conflict" $
            configHcFlavor (parse ["--ghc", "-g"]) @?= Flag.Flag GHC
        , testCase "-g --ghcjs selects GHCJS and does not conflict" $
            configHcFlavor (parse ["-g", "--ghcjs"]) @?= Flag.Flag GHCJS
        ]
    , testGroup
        "--enable-debug-info / -g debug info"
        [ testCase "-g sets NormalDebugInfo" $
            configDebugInfo (parse ["-g"]) @?= Flag.Flag NormalDebugInfo
        , testCase "--enable-debug-info sets NormalDebugInfo" $
            configDebugInfo (parse ["--enable-debug-info"]) @?= Flag.Flag NormalDebugInfo
        , testCase "-g1 sets MinimalDebugInfo" $
            configDebugInfo (parse ["-g1"]) @?= Flag.Flag MinimalDebugInfo
        , testCase "-g3 sets MaximalDebugInfo" $
            configDebugInfo (parse ["-g3"]) @?= Flag.Flag MaximalDebugInfo
        , testCase "--enable-debug-info=2 sets NormalDebugInfo" $
            configDebugInfo (parse ["--enable-debug-info=2"]) @?= Flag.Flag NormalDebugInfo
        , testCase "--disable-debug-info sets NoDebugInfo" $
            configDebugInfo (parse ["--disable-debug-info"]) @?= Flag.Flag NoDebugInfo
        ]
    ]

-- | Parse the given @configure@ command line arguments starting from
-- 'emptyConfigFlags', so that only the options actually given are set.
-- Parsing failures are reported as test failures.
parse :: [String] -> ConfigFlags
parse args =
  case commandParseArgs (configureCommand emptyProgramDb) False args of
    CommandReadyToGo (f, _) -> f emptyConfigFlags
    CommandErrors errs ->
      error $ "unexpected parse errors: " ++ show errs
    CommandHelp _ ->
      error "unexpected help"
    CommandList _ ->
      error "unexpected list"
