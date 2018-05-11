{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens   hiding ((.=))
import Data.Aeson     (FromJSON (..), Value, eitherDecode, object, withObject, (.:), (.=))
import Data.Char      (toUpper, isAlpha)
import Data.Foldable  (for_)
import Data.Semigroup ((<>))
import Data.Text      (Text)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO    as TL
import qualified Options.Applicative  as O
import qualified Text.Microstache     as M

data Opts = Opts FilePath FilePath FilePath

main :: IO ()
main = generate =<< O.execParser opts where
    opts = O.info (O.helper <*> parser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Generate SPDX LicenseId module"
        ]

    parser :: O.Parser Opts
    parser = Opts <$> template <*> licenses <*> output

    template = O.strArgument $ mconcat
        [ O.metavar "SPDX.LicenseId.template.hs"
        , O.help    "Module template file"
        ]

    licenses = O.strArgument $ mconcat
        [ O.metavar "licenses.json"
        , O.help    "Licenses JSON. https://github.com/spdx/license-list-data"
        ]

    output = O.strArgument $ mconcat
        [ O.metavar "Output.hs"
        , O.help    "Output file"
        ]

generate :: Opts -> IO ()
generate (Opts tmplFile fn out) = do
    contents <- LBS.readFile fn
    LicenseList ls <- either fail pure $ eitherDecode contents
    template <- M.compileMustacheFile tmplFile
    let (ws, rendered) = generate' ls template
    for_ ws $ putStrLn . M.displayMustacheWarning
    TL.writeFile out (header <> "\n" <> rendered)
    putStrLn $ "Generated file " ++ out

header :: TL.Text
header = "-- This file is generated. See Makefile's spdx rule"

generate' :: [License] -> M.Template -> ([M.MustacheWarning], TL.Text)
generate' ls template = M.renderMustacheW template $ object
    [ "licenseIds" .= licenseIds
    , "licenses"   .= licenseValues
    ]
  where
    constructorNames :: [(Text,License)]
    constructorNames
        = map (\l -> (toConstructorName $ licenseId l, l))
        $ filter (not . licenseDeprecated)
        $ ls

    licenseValues :: [Value]
    licenseValues = flip map constructorNames $ \(c, l) -> object
        [ "licenseCon"    .= c
        , "licenseId"     .= textShow (licenseId l)
        , "licenseName"   .= textShow (licenseName l)
        , "isOsiApproved" .= licenseOsiApproved l
        ]

    licenseIds :: Text
    licenseIds = T.intercalate "\n" $ flip imap constructorNames $ \i (c, l) ->
        let pfx = if i == 0 then "    = " else "    | "
        in pfx <> c <> " -- ^ @" <> licenseId l <> "@, " <> licenseName l

textShow :: Text -> Text
textShow = T.pack . show

toConstructorName :: Text -> Text
toConstructorName t = t
    & each %~ f
    & ix 0 %~ toUpper
    & special
  where
    f '.' = '_'
    f '-' = '_'
    f '+' = '\''
    f c   = c

    special :: Text -> Text
    special "0BSD" = "NullBSD"
    special t      = t

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
