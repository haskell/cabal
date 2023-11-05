{-# LANGUAGE BangPatterns #-}

module Main
  ( main
  ) where

import           Hackage.Cabal.Patches

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.ByteString.Unsafe
import           Data.List.NonEmpty (NonEmpty (..))
import           Foreign.Ptr
import           GHC.Fingerprint
import           System.Exit
import           System.Process



flatten
  :: (Package -> Version -> Revision -> Patch -> a -> a)
  -> a
  -> [(Package, [(Version, [(Revision, Patch)])])]
  -> a
flatten f =
  let revisions pkg ver (rev, patch) acc = f pkg ver rev patch acc
      versions pkg (ver, revs) acc = foldr (revisions pkg ver) acc revs
      packages (pkg, vers) acc = foldr (versions pkg) acc vers

  in foldr packages



showsLink :: Package -> Version -> Revision -> ShowS
showsLink (Package pkg) (Version (v :| ver)) (Revision rev) = do
  showString "https://hackage.haskell.org/package/"
    . (BSC.unpack pkg <>)
    . showChar '-'
    . shows v
    . (\s -> foldr (\i -> showChar '.' . shows i) s ver)
    . showString "/revision/"
    . shows rev
    . showString ".cabal"



download :: Package -> Version -> Revision -> IO BSC.ByteString
download pkg ver rev = do
  let cmd = (shell (showString "curl -s " $ showsLink pkg ver rev []))
              { std_out = CreatePipe
              }

  putStrLn $ showsLink pkg ver rev []
  withCreateProcess cmd $ \_mayIn mayOut _mayErr process -> do
    case mayOut of
      Nothing   -> fail "No output handle"
      Just outh -> do
        raw <- BSLC.hGetContents outh
        let !bs = BSLC.toStrict raw

        code <- waitForProcess process
        case code of
          ExitFailure c -> fail $ "Error code " <> showsPrec 11 c []
          ExitSuccess   -> pure bs



checkMD5 :: BSC.ByteString -> IO Fingerprint
checkMD5 bs = unsafeUseAsCStringLen bs $ \(ptr, len) ->
                fingerprintData (castPtr ptr) len



verify :: BSC.ByteString -> Patch -> IO ()
verify bs (Patch refMD5 refMD5' patch) = do
  md5 <- checkMD5 bs
  if md5 /= refMD5
    then fail "Pre-patch MD5 mismatch"
    else do
      md5' <- checkMD5 (BSLC.toStrict . patch $ BSLC.fromStrict bs)
      if md5' /= refMD5'
        then fail "Post-patch MD5 mismatch"
        else putStrLn "Correct"



main :: IO ()
main = do
  let run pkg ver rev patch acc = do
        bs <- download pkg ver rev
        verify bs patch
        acc

  flatten run (pure ()) patches
