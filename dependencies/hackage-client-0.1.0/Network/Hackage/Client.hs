module Network.Hackage.Client where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import Distribution.Package
import Distribution.Version
import Data.Version
import Data.Maybe

import Network.Hackage.Version
import Network.Hackage.Interface

{-
data ServerResource
    = ResourceLocation URL
    | UpdateLocation ServerResource URL
    | PackageDescription PackageDescription
    | AuthorDescription { email ... }
    | ResourceBundle [ServerResource]
    deriving (Show, Read, Typeable, Ord, Eq)
-}

getPkgDescription :: String -> PackageIdentifier -> IO (Maybe String)
getPkgDescription url = remote url "getPkgDescription"

getPkgDescriptions :: String -> [PackageIdentifier] -> IO [Maybe String]
getPkgDescriptions url = remote url "getPkgDescriptions"

getDependencies :: String -> [Dependency] -> IO [(Dependency, Maybe ResolvedDependency)]
getDependencies url = remote url "getDependencies"

listPackages :: String -> IO [(PackageIdentifier,[Dependency],String)]
listPackages url = error "moose" -- remote url "listPackages"

getPkgLocation :: String -> PackageIdentifier -> IO (Maybe String)
getPkgLocation url = remote url "getPkgLocation"

getServerVersion :: String -> IO Version
getServerVersion url = remote url "getServerVersion"

isCompatible :: String -> IO Bool
isCompatible = fmap ((==) clientVersion) . getServerVersion
