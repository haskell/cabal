{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- runghc -package-env=default Validate.hs validate.yml.zinza .github/workflows/validate.yml
module Main (main) where

import Data.List          (isPrefixOf)
import GHC.Generics       (Generic)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import Zinza              (Zinza (..), genericFromValueSFP, genericToTypeSFP, genericToValueSFP, parseAndCompileTemplateIO)

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.YAML                  as YAML

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src,tgt] -> do
            run <- parseAndCompileTemplateIO src
            -- this shouldn't fail (run-time errors are due bugs in zinza)
            w <- run Z
                { zJobs =
                    [ GhcJob "8.8.3"  False "--solver-benchmarks" False [] defSteps
                    , GhcJob "8.6.5"  False ""                    False ["8.8.3"] defSteps
                    , GhcJob "8.4.4"  False ""                    False ["8.8.3"] defSteps
                    , GhcJob "8.2.2"  False ""                    False ["8.8.3"] defSteps
                    , GhcJob "8.0.2"  False ""                    False ["8.8.3"] defSteps
                    , GhcJob "7.10.3" False ""                    False ["8.8.3"] defSteps
                    , GhcJob "7.8.4"  False "--lib-only"          False ["8.8.3"] libSteps
                    , GhcJob "7.6.3"  True  "--lib-only"          False ["8.8.3"] libSteps
                    , GhcJob "8.8.3"  True  "--lib-only"          True  ["8.8.3"] $
                        libSteps ++
                        [ "lib-suite-extras --extra-hc /opt/ghc/7.0.4/bin/ghc-7.0.4"
                        , "lib-suite-extras --extra-hc /opt/ghc/7.2.2/bin/ghc-7.2.2"
                        , "lib-suite-extras --extra-hc /opt/ghc/7.4.2/bin/ghc-7.4.2"
                        ]
                    ]
                , zMacosJobs =
                    [ mkMacGhcJob "8.8.3" "https://downloads.haskell.org/~ghc/8.8.3/ghc-8.8.3-x86_64-apple-darwin.tar.xz"
                    , mkMacGhcJob "8.6.5" "https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-apple-darwin.tar.xz"
                    ]
                , zWinJobs =
                    -- 8.8.1 fails atm,
                    -- Shutting down GHCi sessions (please be patient)...
                    -- Unexpected failure on GHCi exit: fd:10: hClose: resource vanished (Broken pipe)
                    -- cabal-tests: fd:10: hClose: resource vanished (Broken pipe)
                    -- [ WinGhcJob "8.8.1" ["8.6.5"]
                    [ WinGhcJob "8.8.3" []
                    , WinGhcJob "8.6.5" []
                    , WinGhcJob "8.10.1" []
                    ]
                , zMangleVersion = map mangleChar
                , zOr            = (||)
                , zNotNull       = not . null
                , zFalse         = False
                }

            -- check that YAML is syntactically valid
            let bs = LBS8.pack w
            case YAML.decode1 bs of
                Right (_ :: YAML.Node YAML.Pos) -> return ()
                Left (pos, err)                 -> do
                    putStrLn $ "ERROR:" ++ YAML.prettyPosWithSource pos bs err
                    exitFailure

            writeFile tgt w
        _ -> putStrLn "Usage source.yml.zinza target.yml"

mangleChar :: Char -> Char
mangleChar '.' = '_'
mangleChar c   = c

defSteps :: [String]
defSteps =
    [ "print-config"
    , "print-tool-versions"
    , "make-cabal-install-dev"
    , "build"
    , "lib-tests"
    , "lib-suite"
    , "cli-tests"
    , "cli-suite"
    ]

libSteps :: [String]
libSteps =
    [ "print-config"
    , "print-tool-versions"
    , "build"
    , "lib-tests"
    , "lib-suite"
    ]

data Z = Z
    { zJobs          :: [GhcJob]
    , zMacosJobs     :: [MacGhcJob]
    , zWinJobs       :: [WinGhcJob]
    , zMangleVersion :: String -> String
    , zOr            :: Bool -> Bool -> Bool
    , zNotNull       :: [String] -> Bool
    , zFalse         :: Bool
    }
  deriving (Generic)

data GhcJob = GhcJob
    { gjVersion :: String
    , gjXenial  :: Bool
    , gjFlags   :: String
    , gjOld     :: Bool
    , gjNeeds   :: [String]
    , gjSteps   :: [String]
    }
  deriving (Generic)

data MacGhcJob = MacGhcJob
    { mgjVersion :: String
    , mgjGhcUrl  :: String
    , mgjFlags   :: String
    , mgjNeeds   :: [String]
    , mgjSteps   :: [String]
    }
  deriving (Generic)

data WinGhcJob = WinGhcJob
    { wgjVersion :: String
    , wgjNeeds   :: [String]
    }
  deriving (Generic)

mkMacGhcJob :: String -> String -> MacGhcJob
mkMacGhcJob v u = MacGhcJob
    { mgjVersion = v
    , mgjGhcUrl  = u
    , mgjFlags   = ""
    , mgjNeeds   = ["8.8.3" | not $ "8.8" `isPrefixOf` v ]
    , mgjSteps   = defSteps
    }

instance Zinza Z where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza GhcJob where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza MacGhcJob where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza WinGhcJob where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP
