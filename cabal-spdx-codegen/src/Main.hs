{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens
import Data.Char (toUpper)
import Data.Text  (Text)
import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecode)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO         as T
import qualified Options.Applicative  as O

main :: IO ()
main = generate =<< O.execParser opts where
    opts = O.info (O.helper <*> parser) $ mconcat 
        [ O.fullDesc  
        , O.progDesc "Generate SPDX module"
        ]

    parser :: O.Parser FilePath
    parser = O.strArgument $ mconcat
        [ O.metavar "licenses.json"
        , O.help    "Licenses JSON. https://github.com/spdx/license-list-data"
        ]

generate :: FilePath -> IO ()
generate fn = do
    contents <- LBS.readFile fn
    LicenseList ls <- either fail pure $ eitherDecode contents
    let constructorNames = map (toConstructorName . licenseId) ls
    mapM_ T.putStrLn constructorNames

toConstructorName :: Text -> Text
toConstructorName t = t
    & each %~ f
    & ix 0 %~ toUpper
  where
    f '.' = '_'
    f '-' = '_'
    f '+' = '\''
    f c   = c

-------------------------------------------------------------------------------
-- Licenses
-------------------------------------------------------------------------------

data License = License
    { licenseId          :: !Text
    , licenseName        :: !Text
    , licenseOsiApproved :: !Bool
    , licenseDeprecated  :: !Bool
    }
  deriving (Show)

instance FromJSON License where
    parseJSON = withObject "License" $ \obj -> License
        <$> obj .: "licenseId"
        <*> obj .: "name"
        <*> obj .: "isOsiApproved"
        <*> obj .: "isDeprecatedLicenseId"

newtype LicenseList = LicenseList [License]
  deriving (Show)

instance FromJSON LicenseList where
    parseJSON = withObject "License list" $ \obj -> LicenseList
        <$> obj .: "licenses"
