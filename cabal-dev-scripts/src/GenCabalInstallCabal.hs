{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception  (SomeException (..), catch, displayException)
import GHC.Generics       (Generic)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Zinza as Z

withIO :: (Bool -> FilePath -> FilePath -> IO a) -> IO a
withIO k = do
    args <- getArgs
    case args of
        [dev',src,tgt]
            | Just dev <- parseBool dev'
            -> k dev src tgt `catch` \(SomeException e) -> do
                putStrLn $ "Exception: " ++ displayException e
                exitFailure
        _         -> do
            putStrLn "Usage cabal run ... source.temeplate.ext target.ext"
            exitFailure
  where
    parseBool "True"  = Just True
    parseBool "False" = Just False
    parseBool _       = Nothing


main :: IO ()
main = withIO $ \dev src tgt -> do
    render <- Z.parseAndCompileTemplateIO src
    contents <- render $ Z dev ()
    writeFile tgt contents

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Z = Z
    { zDev    :: Bool
    , zUnused :: ()
    }
  deriving (Generic)

instance Z.Zinza Z where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP
