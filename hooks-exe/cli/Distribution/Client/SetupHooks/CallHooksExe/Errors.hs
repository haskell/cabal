{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Client.SetupHooks.CallHooksExe.Errors
  ( HookInput(..)
  , SetupHooksCallExeException (..)
  , HookFailedReason(..)
  , setupHooksCallExeExceptionCode
  , setupHooksCallExeExceptionMessage
  ) where

-- Cabal
import Distribution.Compat.Binary
  ( Binary )
import Distribution.Simple.Utils

-- base
import GHC.Exception
import Data.Typeable
  ( Typeable )
import GHC.Int
  ( Int64 )

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

--------------------------------------------------------------------------------

data HookInput where
  HookInput :: (Binary input, Typeable input, Show input)
            => input -> HookInput
instance Show HookInput where
  show (HookInput input) = show input

data SetupHooksCallExeException
  = HookFailed
      String
        -- ^ hook name
      HookFailedReason
        -- ^ why did the hook fail?
  deriving Show

data HookFailedReason
  -- | The hooks executable terminated with non-zero exit code.
  = HookException
      Int -- ^ exit code
  -- | We failed to decode the output of the hooks executable.
  | CouldNotDecodeOutput
        ByteString
          -- ^ hook output that we failed to decode
        Int64
          -- ^ byte offset at which the decoding error took place
        String
          -- ^ info about the decoding error
  deriving Show

setupHooksCallExeExceptionCode :: SetupHooksCallExeException -> Int
setupHooksCallExeExceptionCode = \case
  HookFailed _ reason -> setupHooksCallExeFailedExceptionCode reason

setupHooksCallExeFailedExceptionCode :: HookFailedReason -> Int
setupHooksCallExeFailedExceptionCode = \case
  HookException {} -> 7717
  CouldNotDecodeOutput {} -> 5412

setupHooksCallExeExceptionMessage :: SetupHooksCallExeException -> String
setupHooksCallExeExceptionMessage = \case
  HookFailed hookName reason ->
   setupHooksCallExeFailedMessage hookName reason

setupHooksCallExeFailedMessage :: String -> HookFailedReason -> String
setupHooksCallExeFailedMessage hookName = \case
  HookException {} ->
    "An exception occurred when running the " ++ hookName ++ " hook."
  CouldNotDecodeOutput _bytes offset err ->
    "Failed to decode the output of the " ++ hookName ++ " hook.\n\
    \Decoding failed at position " ++ show offset ++ " with error: " ++ err ++ ".\n\
    \This could be due to a mismatch between the Cabal version of cabal-install and of the hooks executable."

instance Exception (VerboseException SetupHooksCallExeException) where
  displayException (VerboseException stack timestamp verb err) =
    withOutputMarker
      verb
      ( concat
          [ "Error: [Cabal-"
          , show (setupHooksCallExeExceptionCode err)
          , "]\n"
          ]
      )
      ++ exceptionWithMetadata stack timestamp verb (setupHooksCallExeExceptionMessage err)
