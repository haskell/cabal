{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception              (SomeException (..), catch, displayException)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version     (Version)
import GHC.Generics                   (Generic)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)
import Zinza
       (ModuleConfig (..), Ty (..), Zinza (..), genericFromValueSFP, genericToTypeSFP,
       genericToValueSFP, parseAndCompileModuleIO)

import qualified Data.ByteString.Builder as BSB

import Capture

-------------------------------------------------------------------------------
-- Inputs
-------------------------------------------------------------------------------

$(capture "decls" [d|
    data Z = Z
        { zPackages        :: [ZPackage]
        , zTools           :: [ZTool]
        , zPackageKey      :: String
        , zComponentId     :: String
        , zPackageVersion  :: Version
        , zNotNull         :: String -> Bool
        , zManglePkgName   :: PackageName -> BSB.Builder
        , zMangleStr       :: String -> BSB.Builder
        , zMkBuilder       :: String -> BSB.Builder
        }
      deriving (Generic)

    data ZPackage = ZPackage
        { zpkgName    :: PackageName
        , zpkgVersion :: Version
        , zpkgX       :: !Int
        , zpkgY       :: !Int
        , zpkgZ       :: !Int
        }
      deriving (Generic)

    data ZTool = ZTool
        { ztoolName    :: String
        , ztoolVersion :: Version
        , ztoolX       :: !Int
        , ztoolY       :: !Int
        , ztoolZ       :: !Int
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
            putStrLn "Usage cabal v2-run ... source.temeplate.ext target.ext"
            exitFailure

main :: IO ()
main = withIO $ \src tgt -> do
    mdl <- parseAndCompileModuleIO config src
    writeFile tgt mdl

config :: ModuleConfig Z
config = ModuleConfig
    { mcRender = "render"
    , mcHeader =
        [ "{-# LANGUAGE DeriveGeneric     #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module Distribution.Simple.Build.Macros.Z (render, Z(..), ZPackage (..), ZTool (..)) where"
        , "import Data.ByteString.Builder (Builder)"
        , "import qualified Data.ByteString.Builder as BSB"
        , "import Distribution.ZinzaPrelude"
        , decls
        , "render :: Z -> BSB.Builder"
        ]
    }

-------------------------------------------------------------------------------
-- Zinza instances
-------------------------------------------------------------------------------

instance Zinza Z where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza ZPackage where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Zinza ZTool where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance Zinza PackageName where
    toType _    = TyString (Just "prettyShowBuilder")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"

instance Zinza Version where
    toType _    = TyString (Just "prettyShowBuilder")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"

instance Zinza Int where
    toType _    = TyString (Just "BSB.intDec")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"

instance Zinza BSB.Builder where
    toType _    = TyString (Just "id")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"

instance Zinza (Maybe BSB.Builder) where
    toType _    = TyString (Just "fromMaybe id")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"
