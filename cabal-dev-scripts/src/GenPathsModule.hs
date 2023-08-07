{-# LANGUAGE DeriveGeneric       #-}
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

import Capture

-------------------------------------------------------------------------------
-- Inputs
-------------------------------------------------------------------------------

$(capture "decls" [d|
    data Z = Z
        { zPackageName                :: PackageName
        , zVersionDigits              :: String
        , zSupportsCpp                :: Bool
        , zSupportsNoRebindableSyntax :: Bool
        , zAbsolute                   :: Bool
        , zRelocatable                :: Bool
        , zIsWindows                  :: Bool
        , zIsI386                     :: Bool
        , zIsX8664                    :: Bool

        , zPrefix     :: FilePath
        , zBindir     :: FilePath
        , zLibdir     :: FilePath
        , zDynlibdir  :: FilePath
        , zDatadir    :: FilePath
        , zLibexecdir :: FilePath
        , zSysconfdir :: FilePath

        , zNot                        :: Bool -> Bool
        , zManglePkgName              :: PackageName -> String
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
        , "module Distribution.Simple.Build.PathsModule.Z (render, Z(..)) where"
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

instance Zinza Version where
    toType _    = TyString (Just "prettyShow")
    toValue _   = error "not needed"
    fromValue _ = error "not needed"
