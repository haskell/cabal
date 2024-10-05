{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Data.List                       (partition)
import Data.Proxy                      (Proxy (..))
import GHC.Generics                    (Generic)
import System.Environment              (getArgs)
import System.Exit                     (exitFailure)

import Distribution.PackageDescription.FieldGrammar (buildInfoFieldGrammar, packageDescriptionFieldGrammar, testSuiteFieldGrammar)

import qualified Zinza as Z

import Distribution.Described
import Distribution.Described.Extension ()
import Distribution.Utils.GrammarRegex

import Distribution.ModuleName         (ModuleName)
import Distribution.Types.Version      (Version)
import Distribution.Types.VersionRange (VersionRange)
import Cabal.Syntax.Docs.ZFields

main :: IO ()
main = do
    args <- getArgs
    case args of
        [tmpl] -> do
            let (biGhc, biCabal) = partition isGhcBuildInfo $ fromReference buildInfoFieldGrammar
            run <- Z.parseAndCompileTemplateIO tmpl
            contents <- run $ Z
                { zGhcBuildInfoFields       = biGhc
                , zCabalBuildInfoFields     = biCabal
                , zPackageDescriptionFields = fromReference packageDescriptionFieldGrammar
                , zTestSuiteFields          = fromReference $ testSuiteFieldGrammar // buildInfoFieldGrammar
                , zProductions              =
                    [ zproduction "hs-string"       reHsString
                        "String as in Haskell; it's recommended to avoid using Haskell-specific escapes."
                    , zproduction "unqual-name"     reUnqualComponent $ unwords
                        [ "Unqualified component names are used for package names, component names etc. but not flag names."
                        , "Unqualified component name consist of components separated by dash, each component is non-empty alphanumeric string, with at least one alphabetic character."
                        , "In other words, component may not look like a number."
                        ]

                    , zproduction "module-name"     (describe (Proxy :: Proxy ModuleName))
                        "Haskell module name as recognized by Cabal parser."
                    , zproduction "version"         (describe (Proxy :: Proxy Version))
                        "Version is to first approximation numbers separated by dots, where leading zero is not allowed and each version digit is consists at most of nine characters."
                    , zproduction "version-range"   (describe (Proxy :: Proxy VersionRange))
                        "Version range syntax is recursive. Also note the set syntax added in ``cabal-version: 3.0``, set cannot be empty."
                    ]
                , zSpaceList                = show $ regexDoc $
                    REMunch RESpaces1 (RENamed "element" RETodo)
                , zCommaList                = show $ regexDoc $
                    expandedCommaList (RENamed "element" RETodo)
                , zOptCommaList             = show $ regexDoc $
                    expandedOptCommaList (RENamed "element" RETodo)

                , zNull                     = null
                , zNotNull                  = not . null
                }

            putStrLn contents
        _ -> do
          putStrLn "Usage: generator <tmpl>"
          exitFailure

data Z = Z
    { zGhcBuildInfoFields       :: [ZField]
    , zCabalBuildInfoFields     :: [ZField]
    , zPackageDescriptionFields :: [ZField]
    , zTestSuiteFields          :: [ZField]
    , zProductions              :: [ZProduction]
    , zSpaceList                :: String
    , zCommaList                :: String
    , zOptCommaList             :: String
    , zNull                     :: String -> Bool
    , zNotNull                  :: String -> Bool
    }
  deriving (Generic)

instance Z.Zinza Z where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP
