{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens     (imap)
import Data.Aeson       (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.List        (sortOn)
import Data.Semigroup   ((<>))
import Data.Text        (Text)
import Data.Traversable (for)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Options.Applicative  as O
import qualified Zinza                as Z

import GenUtils

data Opts = Opts FilePath (PerV FilePath) FilePath

main :: IO ()
main = generate =<< O.execParser opts where
    opts = O.info (O.helper <*> parser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Generate SPDX LicenseId module"
        ]

    parser :: O.Parser Opts
    parser = Opts <$> template <*> licensesAll <*> output

    licensesAll = PerV
        <$> licenses "3.0"
        <*> licenses "3.2"
        <*> licenses "3.6"
        <*> licenses "3.8"

    template = O.strArgument $ mconcat
        [ O.metavar "SPDX.LicenseId.template.hs"
        , O.help    "Module template file"
        ]

    licenses ver = O.strArgument $ mconcat
        [ O.metavar $ "licenses-" ++ ver ++ ".json"
        , O.help    "Licenses JSON. https://github.com/spdx/license-list-data"
        ]

    output = O.strArgument $ mconcat
        [ O.metavar "Output.hs"
        , O.help    "Output file"
        ]

generate :: Opts -> IO ()
generate (Opts tmplFile fns out) = do
    lss <- for fns $ \fn -> either fail pure . eitherDecode =<< LBS.readFile fn
    template <- Z.parseAndCompileTemplateIO tmplFile
    output <- generate' lss template
    writeFile out (header <> "\n" <> output)
    putStrLn $ "Generated file " ++ out

generate'
    :: PerV LicenseList
    -> (Input -> IO String)
    -> IO String
generate' lss template = template $ Input
    { inputLicenseIds      = licenseIds
    , inputLicenses        = licenseValues
    , inputLicenseList_all = mkLicenseList (== allVers)
    , inputLicenseList_3_0 = mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_0 vers)
    , inputLicenseList_3_2 = mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_2 vers)
    , inputLicenseList_3_6 = mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_6 vers)
    , inputLicenseList_3_9 = mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_9 vers)
    }
  where
    PerV (LL ls_3_0) (LL ls_3_2) (LL ls_3_6) (LL ls_3_9) = lss

    constructorNames :: [(Text, License, Set.Set SPDXLicenseListVersion)]
    constructorNames
        = map (\(l, tags) -> (toConstructorName $ licenseId l, l, tags))
        $ combine licenseId $ \ver -> case ver of
            SPDXLicenseListVersion_3_9 -> filterDeprecated ls_3_9
            SPDXLicenseListVersion_3_6 -> filterDeprecated ls_3_6
            SPDXLicenseListVersion_3_2 -> filterDeprecated ls_3_2
            SPDXLicenseListVersion_3_0 -> filterDeprecated ls_3_0

    filterDeprecated = filter (not . licenseDeprecated)

    licenseValues :: [InputLicense]
    licenseValues = flip map constructorNames $ \(c, l, _) -> InputLicense
        { ilConstructor   = c
        , ilId            = textShow (licenseId l)
        , ilName          = textShow (licenseName l)
        , ilIsOsiApproved = licenseOsiApproved l
        , ilIsFsfLibre    = licenseFsfLibre l
        }

    licenseIds :: Text
    licenseIds = T.intercalate "\n" $ flip imap constructorNames $ \i (c, l, vers) ->
        let pfx = if i == 0 then "    = " else "    | "
            versInfo
                | vers == allVers = ""
                | otherwise       = foldMap (\v -> ", " <> prettyVer v) vers
        in pfx <> c <> " -- ^ @" <> licenseId l <> "@, " <> licenseName l <> versInfo

    mkLicenseList :: (Set.Set SPDXLicenseListVersion -> Bool) -> Text
    mkLicenseList p = mkList [ n | (n, _, vers) <- constructorNames, p vers ]

-------------------------------------------------------------------------------
-- JSON inputs
-------------------------------------------------------------------------------

data License = License
    { licenseId          :: !Text
    , licenseName        :: !Text
    , licenseOsiApproved :: !Bool
    , licenseFsfLibre    :: !Bool
    , licenseDeprecated  :: !Bool
    }
  deriving (Show)

newtype LicenseList = LL [License]
  deriving (Show)

instance FromJSON License where
    parseJSON = withObject "License" $ \obj -> License
        <$> obj .: "licenseId"
        <*> obj .: "name"
        <*> obj .: "isOsiApproved"
        <*> obj .:? "isFsfLibre" .!= False
        <*> obj .: "isDeprecatedLicenseId"

instance FromJSON LicenseList where
    parseJSON = withObject "License list" $ \obj ->
        LL . sortOn (OrdT . T.toLower . licenseId)
            <$> obj .: "licenses"
