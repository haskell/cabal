module UnitTests.Distribution.Simple.Setup.Config
  ( tests
  ) where

import Control.Monad (forM_)
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Simple.Command (CommandParse (..), commandParseArgs, commandShowOptions)
import Distribution.Types.DebugInfoLevel (DebugInfoLevel (..))
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
        , testCase "-g no longer selects GHC" $
            configHcFlavor (parse ["-g"]) @?= Flag.NoFlag
        , testCase "--ghc -g selects GHC and does not conflict" $
            configHcFlavor (parse ["--ghc", "-g"]) @?= Flag.Flag GHC
        ]
    , testGroup
        "--enable-debug-info / -g debug info"
        [ testCase "-g sets NoDebugInfo" $
            configDebugInfo (parse ["-g"]) @?= Flag.Flag NoDebugInfo
        , testCase "--enable-debug-info sets NoDebugInfo" $
            configDebugInfo (parse ["--enable-debug-info"]) @?= Flag.Flag NoDebugInfo
        , testCase "-g1 sets MinimalDebugInfo" $
            configDebugInfo (parse ["-g1"]) @?= Flag.Flag MinimalDebugInfo
        , testCase "-g3 sets MaximalDebugInfo" $
            configDebugInfo (parse ["-g3"]) @?= Flag.Flag MaximalDebugInfo
        , testCase "--enable-debug-info=2 sets NormalDebugInfo" $
            configDebugInfo (parse ["--enable-debug-info=2"]) @?= Flag.Flag NormalDebugInfo
        , testCase "--disable-debug-info sets NoDebugInfo" $
            configDebugInfo (parse ["--disable-debug-info"]) @?= Flag.Flag NoDebugInfo
        ]
    , testGroup
        "debug-info serialised options round-trip"
        [ testCase "NormalDebugInfo is serialised explicitly, not as the bare form" $ do
            let args =
                  commandShowOptions
                    (configureCommand emptyProgramDb)
                    emptyConfigFlags{configDebugInfo = Flag.Flag NormalDebugInfo}
            assertBool
              ("expected --enable-debug-info=2, got " ++ show args)
              ("--enable-debug-info=2" `elem` args)
            assertBool
              ("bare --enable-debug-info leaked into " ++ show args)
              ("--enable-debug-info" `notElem` args)
        , testCase "all levels round-trip through the serialised options"
            $ forM_
              [NoDebugInfo, MinimalDebugInfo, NormalDebugInfo, MaximalDebugInfo]
            $ \lvl -> do
              let cmd = configureCommand emptyProgramDb
                  cfg = emptyConfigFlags{configDebugInfo = Flag.Flag lvl}
                  args = commandShowOptions cmd cfg
              case commandParseArgs cmd False args of
                CommandReadyToGo (f, _) ->
                  configDebugInfo (f emptyConfigFlags) @?= Flag.Flag lvl
                CommandErrors errs ->
                  error ("unexpected parse errors: " ++ show errs)
                CommandHelp _ -> error "unexpected help"
                CommandList _ -> error "unexpected list"
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
