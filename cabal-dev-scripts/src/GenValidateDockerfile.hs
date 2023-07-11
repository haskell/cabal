{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception  (SomeException (..), catch, displayException)
import GHC.Generics       (Generic)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Data.Map as Map
import qualified Zinza as Z

withIO :: (String -> FilePath -> FilePath -> IO a) -> IO a
withIO k = do
    args <- getArgs
    case args of
        [version,src,tgt]
            -> k version src tgt `catch` \(SomeException e) -> do
                putStrLn $ "Exception: " ++ displayException e
                exitFailure
        _         -> do
            putStrLn "Usage cabal run ... version"
            exitFailure

main :: IO ()
main = withIO $ \version src tgt -> do
    render <- Z.parseAndCompileTemplateIO src
    case Map.lookup version params of
        Just z -> do
            contents <- render z
            writeFile tgt contents

        Nothing -> do
            putStrLn $ "Unknown version " ++ version
            exitFailure

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

params :: Map.Map String Z
params = Map.fromList
    [ pair "8.10.4" $ Z "ghc-8.10.4" "8.10.4-bionic" False True  False True  ""
    , pair "8.8.4"  $ Z "ghc-8.8.4"  "8.8.4-bionic"  False True  False True  "--doctest --solver-benchmarks --complete-hackage"
    , pair "8.6.5"  $ Z "ghc-8.6.5"  "8.6.5-bionic"  False True  False True  ""
    , pair "8.4.4"  $ Z "ghc-8.4.4"  "8.4.4-bionic"  False True  False True  ""
    , pair "8.2.2"  $ Z "ghc-8.2.2"  "8.2.2-bionic"  True  True  False True  ""
    ]
  where
    pair = (,)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Z = Z
    { zGhc             :: String
    , zImage           :: String
    , zParsecCompat    :: Bool
    , zHasTransformers :: Bool
    , zNeedsDynamic    :: Bool
    , zClient          :: Bool
    , zArgs            :: String
    }
  deriving (Generic)

instance Z.Zinza Z where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP
