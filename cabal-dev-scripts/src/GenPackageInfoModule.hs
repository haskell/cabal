{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception              (SomeException (..), catch, displayException)
import Distribution.Types.PackageName (PackageName)
import GHC.Generics                   (Generic)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)
import Zinza
       (ModuleConfig (..), Ty (..), Zinza (..), genericFromValueSFP, genericToTypeSFP,
       genericToValueSFP, parseAndCompileModuleIO)

import Capture

-------------------------------------------------------------------------------
-- Inputs
-------------------------------------------------------------------------------

$(capture "decls" [d|
    data Z = Z
        { zPackageName                :: PackageName
        , zVersionDigits              :: String
        , zLicense                    :: String
        , zCopyright                  :: String
        , zMaintainer                 :: String
        , zAuthor                     :: String
        , zStability                  :: String
        , zHomepage                   :: String
        , zPkgUrl                     :: String
        , zBugReports                 :: String
        , zSynopsis                   :: String
        , zDescription                :: String
        , zCategory                   :: String

        , zSupportsNoRebindableSyntax :: Bool

        , zManglePkgName              :: PackageName -> String
        , zShow                       :: String -> String
        }
      deriving (Generic)
    |])

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

withIO :: (FilePath -> FilePath -> IO a) -> IO a
withIO k = do
    args <- getArgs
    case args of
        [src,tgt] -> k src tgt `catch` \(SomeException e) -> do
            putStrLn $ "Exception: " ++ displayException e
            exitFailure
        _         -> do
            putStrLn "Usage cabal run ... source.temeplate.ext target.ext"
            exitFailure

main :: IO ()
main = withIO $ \src tgt -> do
    mdl <- parseAndCompileModuleIO config src
    writeFile tgt mdl

config :: ModuleConfig Z
config = ModuleConfig
    { mcRender = "render"
    , mcHeader =
        [ "{- FOURMOLU_DISABLE -}"
        , "{-# LANGUAGE DeriveGeneric #-}"
        , "module Distribution.Simple.Build.PackageInfoModule.Z (render, Z(..)) where"
        , "import Distribution.ZinzaPrelude"
        , decls
        , "render :: Z -> String"
        ]
    }

-------------------------------------------------------------------------------
-- Zinza instances
-------------------------------------------------------------------------------

instance Zinza Z where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance Zinza PackageName where
    toType _    = TyString (Just "prettyShow")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"
