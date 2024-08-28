{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main (main) where

import Data.Either        (partitionEithers)
import Data.Foldable      (for_)
import Data.String        (fromString)
import Data.Traversable   (for)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Cabal.Index                    as I
import qualified Cabal.Plan                     as P
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as Map
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.Version as C
import qualified Topograph                      as TG
import Control.Exception
import System.IO.Error (isDoesNotExistError)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] ->
          handleJust
            (\e -> if isDoesNotExistError e then Just e else Nothing)
              (\e -> die $ unlines ["~~~ ERROR ~~~", "", displayException e, "", cabalDirWarning])
              (main1 fp)
        _ -> die "Usage: cabal-bootstrap-gen plan.json"

cabalDirWarning :: String
cabalDirWarning =
  unlines [
    "~~~ NOTE ~~~",
    "",
    "This script will look for cabal global config file in the following locations",
    " - $CABAL_CONFIG",
    " - $CABAL_DIR/config",
    " - $HOME/.cabal/config (on Unix-like systems)",
    " - %APPDATA%/cabal (on Windows)",
    "",
    "If you are using XDG paths or a entirely different location, you can set either",
    "CABAL_CONFIG or CABAL_DIR to guide the script to the correct location.",
    "",
    "E.g.",
    "  $ CABAL_DIR=$HOME/.config/cabal cabal-bootstrap-gen"
    ]

main1 :: FilePath -> IO ()
main1 planPath = do
    meta <- getMap <$> I.cachedHackageMetadata
    plan <- P.decodePlanJson planPath
    main2 meta plan
  where
#if MIN_VERSION_cabal_install_parsers(0,4,0)
    getMap = snd
#else
    getMap = id
#endif

main2 :: Map.Map C.PackageName I.PackageInfo -> P.PlanJson -> IO ()
main2 meta plan = do
    info $ show $ Map.keys $ P.pjUnits plan

    let res = TG.runG (P.planJsonIdGraph plan) $ \g ->
          map (TG.gFromVertex g) (reverse $ TG.gVertices g)

    units <- case res of
              Left loop -> die $ "Loop in install-plan: " ++ show loop
              Right uids -> for uids $ lookupUnit (P.pjUnits plan)

    info "Unit order:"
    for_ units $ \unit -> do
        info $ " - " ++ show (P.uId unit)

    (builtin, deps) <- fmap partitionEithers $ for units $ \unit -> do
        let P.PkgId pkgname@(P.PkgName tpkgname) ver@(P.Ver verdigits) = P.uPId unit

        let cpkgname :: C.PackageName
            cpkgname = C.mkPackageName (T.unpack tpkgname)

        let cversion :: C.Version
            cversion = C.mkVersion verdigits

        let flags = [ (if fval then "+" else "-") ++ T.unpack fname
                        | (P.FlagName fname, fval) <- Map.toList (P.uFlags unit)
                        ]
        let relInfo = Map.lookup cpkgname meta >>= \pkgInfo -> Map.lookup cversion $ I.piVersions pkgInfo
        case P.uType unit of
            P.UnitTypeBuiltin ->
                return $ Left Builtin
                  { builtinPackageName = pkgname
                  , builtinVersion     = ver
                  }
            _ -> do
              let component = case Map.keys (P.uComps unit) of
                                [c] -> Just (P.dispCompNameTarget pkgname c)
                                _ -> Nothing

              source <-
                case P.uPkgSrc unit of
                      Just (P.RepoTarballPackage (P.RepoSecure _uri)) ->
                        return Hackage
                      Just (P.LocalUnpackedPackage _path) ->
                        return Local
                      pkgsrc ->
                        die $ "package source not supported: " ++ show pkgsrc

              return $ Right Dep
                      { depPackageName = pkgname
                      , depVersion     = ver
                      , depSource      = source
                      , depSrcHash     = P.uSha256 unit
                      , depRevision    = fromIntegral . I.riRevision <$> relInfo
                      , depRevHash     = relInfo >>= P.sha256FromByteString . I.getSHA256 . getHash
                      , depFlags       = flags
                      , depComponent   = component
                      }
    LBS.putStr $ A.encode Result
        { resBuiltin      = builtin
        , resDependencies = deps
        }
  where
#if MIN_VERSION_cabal_install_parsers(0,6,0)
    getHash = I.riCabalHash
#else
    getHash = I.riCabal
#endif

lookupUnit :: Map.Map P.UnitId P.Unit -> P.UnitId -> IO P.Unit
lookupUnit units uid
    = maybe (die $ "Cannot find unit " ++ show uid) return
    $ Map.lookup uid units

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Result = Result
    { resBuiltin      :: [Builtin]
    , resDependencies :: [Dep]
    }
  deriving (Show)

data Builtin = Builtin
    { builtinPackageName :: P.PkgName
    , builtinVersion     :: P.Ver
    }
  deriving (Show)

data Dep = Dep
    { depPackageName :: P.PkgName
    , depVersion     :: P.Ver
    , depSource      :: SrcType
    , depSrcHash     :: Maybe P.Sha256
    , depRevision    :: Maybe Int
    , depRevHash     :: Maybe P.Sha256
    , depFlags       :: [String]
    , depComponent   :: Maybe T.Text
    }
  deriving (Show)

data SrcType
    = Hackage
    | Local
  deriving (Show)

instance A.ToJSON Result where
    toJSON res = A.object
        [ fromString "builtin"      A..= resBuiltin res
        , fromString "dependencies" A..= resDependencies res
        ]

instance A.ToJSON Builtin where
    toJSON b = A.object
        [ fromString "package"      A..= builtinPackageName b
        , fromString "version"      A..= builtinVersion b
        ]

instance A.ToJSON Dep where
    toJSON dep = A.object
        [ fromString "package"      A..= depPackageName dep
        , fromString "version"      A..= depVersion dep
        , fromString "source"       A..= depSource dep
        , fromString "src_sha256"   A..= depSrcHash dep
        , fromString "revision"     A..= depRevision dep
        , fromString "cabal_sha256" A..= depRevHash dep
        , fromString "flags"        A..= depFlags dep
        , fromString "component"    A..= depComponent dep
        ]

instance A.ToJSON SrcType where
    toJSON Hackage     = fromString "hackage"
    toJSON Local       = fromString "local"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Log some debug information to stderr.
--
-- Disabled by default to keep the output tidy, replace by
-- the version with 'hPutStrLn' when debugging.
--
info :: String -> IO ()
info _msg = return ()
-- info msg = hPutStrLn stderr $ "INFO: " ++ msg

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitFailure
