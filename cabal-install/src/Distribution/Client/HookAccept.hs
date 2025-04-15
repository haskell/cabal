{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Client.HookAccept
  ( HookAccept (..)
  , assertHookHash
  , loadHookHasheshMap
  , parseHooks
  ) where

import Distribution.Client.Compat.Prelude

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Data.Map.Strict as Map

import Distribution.Client.Config (getConfigFilePath)
import Distribution.Client.Errors (CabalInstallException (..))
import Distribution.Client.HashValue (HashValue, hashValueFromHex, readFileHashValue, showHashValue)
import Distribution.Simple.Setup (Flag (..))
import Distribution.Simple.Utils (dieWithException)
import Distribution.Verbosity (normal)

import System.FilePath (takeDirectory, (</>))

data HookAccept
  = AcceptAlways
  | AcceptHash HashValue
  deriving (Eq, Show, Generic)

instance Monoid HookAccept where
  mempty = AcceptAlways -- Should never be needed.
  mappend = (<>)

instance Semigroup HookAccept where
  AcceptAlways <> AcceptAlways = AcceptAlways
  AcceptAlways <> AcceptHash h = AcceptHash h
  AcceptHash h <> AcceptAlways = AcceptHash h
  AcceptHash h <> _ = AcceptHash h

instance Binary HookAccept
instance Structured HookAccept

assertHookHash :: Map FilePath HookAccept -> FilePath -> IO ()
assertHookHash m fpath = do
  actualHash <- readFileHashValue fpath
  hsPath <- getHooksSecurityFilePath NoFlag
  case Map.lookup fpath m of
    Nothing ->
      dieWithException normal $
        HookAcceptUnknown hsPath fpath (showHashValue actualHash)
    Just AcceptAlways -> pure ()
    Just (AcceptHash expectedHash) ->
      when (actualHash /= expectedHash) $
        dieWithException normal $
          HookAcceptHashMismatch
            hsPath
            fpath
            (showHashValue expectedHash)
            (showHashValue actualHash)

getHooksSecurityFilePath :: Flag FilePath -> IO FilePath
getHooksSecurityFilePath configFileFlag = do
  hfpath <- getConfigFilePath configFileFlag
  pure $ takeDirectory hfpath </> "hooks-security"

loadHookHasheshMap :: Flag FilePath -> IO (Map FilePath HookAccept)
loadHookHasheshMap configFileFlag = do
  hookFilePath <- getHooksSecurityFilePath configFileFlag
  handleNotExists $ fmap parseHooks (BS.readFile hookFilePath)
  where
    handleNotExists :: IO (Map FilePath HookAccept) -> IO (Map FilePath HookAccept)
    handleNotExists action = catchIO action $ \_ -> return mempty

parseHooks :: ByteString -> Map FilePath HookAccept
parseHooks = Map.fromList . map parse . cleanUp . BS.lines
  where
    cleanUp :: [ByteString] -> [ByteString]
    cleanUp = filter (not . BS.null) . map rmComments

    rmComments :: ByteString -> ByteString
    rmComments = fst . BS.breakSubstring "--"

parse :: ByteString -> (FilePath, HookAccept)
parse bs =
  case BS.words bs of
    [fp, "AcceptAlways"] -> (BS.unpack fp, AcceptAlways)
    [fp, "AcceptHash"] -> buildAcceptHash fp "00"
    [fp, "AcceptHash", h] -> buildAcceptHash fp h
    _ -> error $ "Not able to parse:" ++ show bs
  where
    buildAcceptHash :: ByteString -> ByteString -> (FilePath, HookAccept)
    buildAcceptHash fp h =
      case hashValueFromHex h of
        Left err -> error $ "Distribution.Client.HookAccept.parse :" ++ err
        Right hv -> (BS.unpack fp, AcceptHash hv)
