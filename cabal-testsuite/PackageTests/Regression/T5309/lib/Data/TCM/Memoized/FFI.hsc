-----------------------------------------------------------------------------
-- |
-- TODO: Document module.
--
-- Exports C types for dynamic characters and their constructors allong with
-- an FFI binding for the memoizing TCM structure.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, ForeignFunctionInterface, TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TCM.Memoized.FFI
  ( CBufferUnit
  , CDynamicChar(..)
  , DCElement(..)
  , ForeignVoid()
  , MemoizedCostMatrix(costMatrix)
  , getMemoizedCostMatrix
  , getMedianAndCost
  -- * Utility functions
  , calculateBufferLength
  , coerceEnum
  , constructCharacterFromExportable
  , constructElementFromExportable
  , constructEmptyElement
  ) where

import Bio.Character.Exportable.Class
import Data.Bits
import Foreign         hiding (alignPtr)
import Foreign.C.Types
import GHC.Generics           (Generic)
import System.IO.Unsafe

-- import Debug.Trace

#include "costMatrixWrapper.h"
#include "dynamicCharacterOperations.h"


-- |
-- A convient type alias for improved clairity of use.
type CBufferUnit  = CULong -- This will be compatible with uint64_t


-- |
-- Type of a dynamic character to pass back and forth across the FFI interface.
data CDynamicChar
   = CDynamicChar
   { alphabetSizeChar :: CSize
   , numElements      :: CSize
   , dynCharLen       :: CSize
   , dynChar          :: Ptr CBufferUnit
   }


-- |
-- Represents a single element in a dynamic character in an exportable form.
data DCElement = DCElement
    { alphabetSizeElem :: CSize
    , characterElement :: Ptr CBufferUnit
    } deriving (Show)


-- |
-- A closed type wrapping a void pointer in C to the C++ memoized TCM.
data ForeignVoid deriving (Generic)


-- |
-- A type-safe wrapper for the mutable, memoized TCm.
newtype MemoizedCostMatrix
      = MemoizedCostMatrix
      { costMatrix :: StablePtr ForeignVoid
      } deriving (Eq, Generic)


{-
-- | (✔)
instance Show CDynamicChar where
    show (CDynamicChar alphSize dcLen numElems dChar) =
       mconcat
         ["alphabetSize:  "
         , show intAlphSize
         , "\ndynCharLen: "
         , show intLen
         , "\nbuffer length: "
         , show bufferLength
         , "\ndynChar:    "
         , show $ unsafePerformIO printedArr
         ]
        where
            bufferLength = fromEnum numElems
            intAlphSize  = fromEnum alphSize
            intLen       = fromEnum dcLen
            printedArr   = show <$> peekArray bufferLength dChar

-}


instance Storable CDynamicChar where

    sizeOf    _ = (#size struct dynChar_t) -- #size is a built-in that works with arrays, as are #peek and #poke, below

    alignment _ = alignment (undefined :: CBufferUnit)

    peek ptr    = do -- to get values from the C app
        alphLen <- (#peek struct dynChar_t, alphSize  ) ptr
        nElems  <- (#peek struct dynChar_t, numElems  ) ptr
        seqLen  <- (#peek struct dynChar_t, dynCharLen) ptr
        seqVal  <- (#peek struct dynChar_t, dynChar   ) ptr
        pure CDynamicChar
             { alphabetSizeChar = alphLen
             , numElements      = nElems
             , dynCharLen       = seqLen
             , dynChar          = seqVal
             }

    poke ptr (CDynamicChar alphLen nElems seqLen seqVal) = do -- to modify values in the C app
        (#poke struct dynChar_t, alphSize  ) ptr alphLen
        (#poke struct dynChar_t, numElems  ) ptr nElems
        (#poke struct dynChar_t, dynCharLen) ptr seqLen
        (#poke struct dynChar_t, dynChar   ) ptr seqVal


-- | (✔)
instance Storable DCElement where

    sizeOf    _ = (#size struct dcElement_t)

    alignment _ = alignment (undefined :: CBufferUnit)

    peek ptr    = do
        alphLen <- (#peek struct dcElement_t, alphSize) ptr
        element <- (#peek struct dcElement_t, element ) ptr
        pure DCElement
            { alphabetSizeElem = alphLen
            , characterElement = element
            }

    poke ptr (DCElement alphLen element) = do
        (#poke struct dcElement_t, alphSize) ptr alphLen
        (#poke struct dcElement_t, element ) ptr element



-- TODO: For now we only allocate 2d matrices. 3d will come later.
-- |
-- Create and allocate cost matrix.
-- The first argument, TCM, is only for non-ambiguous nucleotides, and it used to
-- generate the entire cost matrix, which includes ambiguous elements. TCM is
-- row-major, with each row being the left character element. It is therefore
-- indexed not by powers of two, but by cardinal integer.
foreign import ccall unsafe "costMatrixWrapper matrixInit"
    initializeMemoizedCMfn_c :: CSize
                             -> Ptr CInt
                             -> IO (StablePtr ForeignVoid)


foreign import ccall unsafe "costMatrix getCostAndMedian"
    getCostAndMedianFn_c :: Ptr DCElement
                         -> Ptr DCElement
                         -> Ptr DCElement
--                         -> CSize
                         -> StablePtr ForeignVoid
                         -> IO CInt


-- |
-- Set up and return a cost matrix.
--
-- The cost matrix is allocated strictly.
getMemoizedCostMatrix :: Word
                      -> (Word -> Word -> Word)
                      -> MemoizedCostMatrix
getMemoizedCostMatrix alphabetSize costFn = unsafePerformIO . withArray rowMajorList $ \allocedTCM -> do
    !resultPtr <- initializeMemoizedCMfn_c (coerceEnum alphabetSize) allocedTCM
    pure $ MemoizedCostMatrix resultPtr
  where
    rowMajorList = [ coerceEnum $ costFn i j | i <- range,  j <- range ]
    range = [0 .. alphabetSize - 1]


-- |
-- /O(1)/ amortized.
--
-- Calculate the median symbol set and transition cost between the two input
-- symbol sets.
--
-- *Note:* This operation is lazily evaluated and memoized for future calls.
getMedianAndCost :: Exportable s => MemoizedCostMatrix -> s -> s -> (s, Word)
getMedianAndCost memo lhs rhs = unsafePerformIO $ do
    medianPtr     <- constructEmptyElement alphabetSize
    lhs'          <- constructElementFromExportable lhs
    rhs'          <- constructElementFromExportable rhs
    !cost         <- getCostAndMedianFn_c lhs' rhs' medianPtr (costMatrix memo)
    medianElement <- peek medianPtr
    medianValue   <- fmap buildExportable . peekArray bufferLength $ characterElement medianElement
    pure (medianValue, coerceEnum cost)
  where
    alphabetSize    = exportedElementWidthSequence $ toExportableBuffer lhs
    buildExportable = fromExportableBuffer . ExportableCharacterSequence 1 alphabetSize
    bufferLength    = calculateBufferLength alphabetSize 1


-- |
-- /O(1)/
--
-- Calculate the buffer length based on the element count and element bit width.
calculateBufferLength :: Enum b
                      => Int -- ^ Element count
                      -> Int -- ^ Element bit width
                      -> b
calculateBufferLength count width = coerceEnum $ q + if r == 0 then 0 else 1
   where
    (q,r)  = (count * width) `divMod` finiteBitSize (undefined :: CULong)


-- |
-- Coerce one 'Enum' value to another through the type's corresponding 'Int'
-- values.
coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum


-- |
-- /O(n)/ where @n@ is the length of the dynamic character.
--
-- Malloc and populate a pointer to an exportable representation of the
-- 'Exportable' value. The supplied value is assumed to be a dynamic character
-- and the result is a pointer to a C representation of a dynamic character.
constructCharacterFromExportable :: Exportable s => s -> IO (Ptr CDynamicChar)
constructCharacterFromExportable exChar = do
    valueBuffer <- newArray $ exportedBufferChunks exportableBuffer
    charPointer <- malloc :: IO (Ptr CDynamicChar)
    let charValue = CDynamicChar (coerceEnum width) (coerceEnum count) bufLen valueBuffer
    !_ <- poke charPointer charValue
    pure charPointer
  where
    count  = exportedElementCountSequence exportableBuffer
    width  = exportedElementWidthSequence exportableBuffer
    bufLen = calculateBufferLength count width
    exportableBuffer = toExportableBuffer exChar


-- |
-- /O(1)/
--
-- Malloc and populate a pointer to an exportable representation of the
-- 'Exportable' value. The supplied value is assumed to be a dynamic character
-- element and the result is a pointer to a C representation of a dynamic
-- character element.
constructElementFromExportable :: Exportable s => s -> IO (Ptr DCElement)
constructElementFromExportable exChar = do
    valueBuffer    <- newArray $ exportedBufferChunks exportableBuffer
    elementPointer <- malloc :: IO (Ptr DCElement)
    let elementValue = DCElement (coerceEnum width) valueBuffer
    !_ <- poke elementPointer elementValue
    pure elementPointer
  where
    width  = exportedElementWidthSequence exportableBuffer
    exportableBuffer = toExportableBuffer exChar


-- |
-- /O(1)/
--
-- Malloc and populate a pointer to a C representation of a dynamic character.
-- The buffer of the resulting value is intentially zeroed out.
constructEmptyElement :: Int -- ^ Bit width of a dynamic character element.
                      -> IO (Ptr DCElement)
constructEmptyElement alphabetSize = do
    elementPointer <- malloc :: IO (Ptr DCElement)
    valueBuffer    <- mallocArray bufferLength
    let elementValue = DCElement (coerceEnum alphabetSize) valueBuffer
    !_ <- poke elementPointer elementValue
    pure elementPointer
  where
    bufferLength = calculateBufferLength alphabetSize 1
