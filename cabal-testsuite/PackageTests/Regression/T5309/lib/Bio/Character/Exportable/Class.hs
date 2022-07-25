-----------------------------------------------------------------------------
-- |
-- Module      :  Bio.Character.Exportable.Class
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Class for needed operations of coded sequences and characters
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Bio.Character.Exportable.Class where


import Foreign.C.Types


-- |
-- Represents a sequence of fixed width characters packed into a bitwise form
-- consumable by lower level functions.
class Exportable c where

    toExportableBuffer     :: c -> ExportableCharacterSequence
    fromExportableBuffer   :: ExportableCharacterSequence -> c

    toExportableElements   :: c -> Maybe ExportableCharacterElements
    fromExportableElements :: ExportableCharacterElements -> c


-- |
-- A structure used for FFI calls.
--
-- 'bufferChunks' contains the bit-packed representation of the character sequence.
data ExportableCharacterSequence
   = ExportableCharacterSequence
   { exportedElementCountSequence :: Int
   , exportedElementWidthSequence :: Int
   , exportedBufferChunks :: [CULong]
   } deriving (Eq, Show)


-- |
-- A structure used for FFI calls--
-- 'characterElements' contains the integral value for each character element.
data ExportableCharacterElements
   = ExportableCharacterElements
   { exportedElementCountElements :: Int
   , exportedElementWidthElements :: Int
   , exportedCharacterElements :: [CUInt]
   } deriving (Eq, Show)
