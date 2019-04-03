{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens     hiding ((.=))
import Data.Aeson       (FromJSON (..), Value, eitherDecode, object, withObject, (.:), (.=))
import Data.Foldable    (for_)
import Data.List        (sortOn)
import Data.Semigroup   ((<>))
import Data.Text        (Text)
import Data.Traversable (for)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO    as TL
import qualified Options.Applicative  as O
import qualified Text.Microstache     as M

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
        <*> licenses "3.5"

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
    template <- M.compileMustacheFile tmplFile
    let (ws, rendered) = generate' lss template
    for_ ws $ putStrLn . M.displayMustacheWarning
    TL.writeFile out (header <> "\n" <> rendered)
    putStrLn $ "Generated file " ++ out

generate'
    :: PerV LicenseList
    -> M.Template
    -> ([M.MustacheWarning], TL.Text)
generate' lss template = M.renderMustacheW template $ object
    [ "licenseIds" .= licenseIds
    , "licenses"   .= licenseValues
    , "licenseList_all" .= mkLicenseList (== allVers)
    , "licenseList_3_0" .= mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_0 vers)
    , "licenseList_3_2" .= mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_2 vers)
    , "licenseList_3_5" .= mkLicenseList
        (\vers -> vers /= allVers && Set.member SPDXLicenseListVersion_3_5 vers)
    ]
  where
    PerV (LL ls_3_0) (LL ls_3_2) (LL ls_3_5) = lss

    constructorNames :: [(Text, License, Set.Set SPDXLicenseListVersion)]
    constructorNames
        = map (\(l, tags) -> (toConstructorName $ licenseId l, l, tags))
        $ combine licenseId $ \ver -> case ver of
            SPDXLicenseListVersion_3_5 -> filterDeprecated ls_3_5
            SPDXLicenseListVersion_3_2 -> filterDeprecated ls_3_2
            SPDXLicenseListVersion_3_0 -> filterDeprecated ls_3_0

    filterDeprecated = filter (not . licenseDeprecated)

    licenseValues :: [Value]
    licenseValues = flip map constructorNames $ \(c, l, _) -> object
        [ "licenseCon"      .= c
        , "licenseId"       .= textShow (licenseId l)
        , "licenseName"     .= textShow (licenseName l)
        , "isOsiApproved"   .= licenseOsiApproved l
        ]

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

newtype LicenseList = LL [License]
  deriving (Show)

instance FromJSON LicenseList where
    parseJSON = withObject "License list" $ \obj ->
        LL . sortOn (OrdT . T.toLower . licenseId)
            <$> obj .: "licenses"
