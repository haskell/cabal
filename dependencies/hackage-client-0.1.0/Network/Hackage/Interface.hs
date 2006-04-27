module Network.Hackage.Interface where

import Distribution.Package(PackageIdentifier(..))
import Distribution.Version(Version(..), showVersion, VersionRange,
                            Dependency(..), withinRange)
import Network.XmlRpc.Server(ServerResult, fun, handleCall, methods)
import Network.XmlRpc.Internals(XmlRpcType(..), Type(TStruct, TArray, TString),
                                MethodCall, getField, Value(..))
import Control.Monad.State(Functor(..), MonadIO(..), mapM, gets,
                           evalStateT)
import Data.List
import Data.Maybe

-- Resolved dependency, pkg location and resolved dependencies of the dependency.
data ResolvedDependency
    = ResolvedDependency PackageIdentifier String [(Dependency,Maybe ResolvedDependency)]
      deriving (Eq,Show)

instance XmlRpcType PackageIdentifier where
    toValue p = toValue [("pkgName",toValue (pkgName p)),
                         ("pkgVersion", toValue (pkgVersion p))]
    fromValue v =
        do t <- fromValue v
           name <- getField "pkgName" t
           version <- getField "pkgVersion" t
           return (PackageIdentifier name version)
    getType _ = TStruct

instance XmlRpcType Version where
    toValue p = toValue [("branch",toValue (versionBranch p)),
                         ("tags", toValue (versionTags p))]
    fromValue v =
        do t <- fromValue v
           branch <- getField "branch" t
           tags <- getField "tags" t
           return (Version branch tags)
    getType _ = TStruct

instance XmlRpcType Dependency where
    toValue (Dependency name range)
        = toValue [("name",toValue name),
                   ("range", toValue range)]
    fromValue v =
        do t <- fromValue v
           name <- getField "name" t
           range <- getField "range" t
           return (Dependency name range)
    getType _ = TStruct

instance XmlRpcType ResolvedDependency where
    toValue (ResolvedDependency pkg location deps)
        = toValue [("pkg",toValue pkg),
                   ("deps", toValue deps),
                   ("location", toValue location)]
    fromValue v =
        do t <- fromValue v
           pkg <- getField "pkg" t
           deps <- getField "deps" t
           location <- getField "location" t
           return (ResolvedDependency pkg location deps)
    getType _ = TStruct

instance (XmlRpcType a, XmlRpcType b) => XmlRpcType (a,b) where
    toValue (a,b)
        = toValue [("fst",toValue a),
                   ("snd", toValue b)]
    fromValue v =
        do t <- fromValue v
           a <- getField "fst" t
           b <- getField "snd" t
           return (a,b)
    getType _ = TStruct

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c)
    => XmlRpcType (a,b,c) where
    toValue (a,b,c)
        = toValue [("fst", toValue a),
                   ("snd", toValue b),
                   ("trd", toValue c)]
    fromValue v =
        do t <- fromValue v
           a <- getField "fst" t
           b <- getField "snd" t
           c <- getField "trd" t
           return (a,b,c)
    getType _ = TStruct

instance XmlRpcType VersionRange where
    toValue versionRange
        = toValue (show versionRange)
    fromValue v =
        do t <- fromValue v
           return (read t)
    getType _ = TString

instance (XmlRpcType a) => XmlRpcType (Maybe a) where
    toValue Nothing = toValue ([]::[Value])
    toValue (Just a) = toValue [toValue a]
    fromValue v =
        do t <- fromValue v
           case t of
             [x] -> fmap Just (fromValue x)
             _   -> return Nothing
    getType _ = TArray

