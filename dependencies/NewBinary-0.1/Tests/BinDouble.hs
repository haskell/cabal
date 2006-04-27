module NewBinary.BinDouble() where

import NewBinary.Binary
import PrelByteArr
import GHC.Float
import ST

instance Binary Double where
    put_ bh d = put_ bh (doubleToInts d)
    get  bh   = do { s <- get bh ; return (intsToDouble s) }

instance Binary Float where
    put_ bh d = put_ bh (floatToInt d)
    get  bh   = do { s <- get bh ; return (intToFloat s) }

doubleToInts d = runST (
    do arr <- newDoubleArray (1,2)
       writeDoubleArray arr 1 d
       i1 <- readIntArray arr 1
       i2 <- readIntArray arr 2
       return (i1,i2))

intsToDouble (i1,i2) = runST (
    do arr <- newDoubleArray (1,2)
       writeIntArray arr 1 i1
       writeIntArray arr 2 i2
       readDoubleArray arr 1)


doublesToInts dl = runST (
    do arr <- newDoubleArray (1, 2)
       mapM (\ d -> do { writeDoubleArray arr 1 d ;
			 i1 <- readIntArray arr 1 ;
			 i2 <- readIntArray arr 2 ;
			 return (i1,i2) } ) dl)

intsToDoubles il = runST (
    do arr <- newDoubleArray (1, 2)
       mapM (\ (i1,i2) -> writeIntArray arr 1 i1 >> writeIntArray arr 2 i2 >> readDoubleArray arr 1) il)

floatToInt d = runST (
    do arr <- newFloatArray (1,1)
       writeFloatArray arr 1 d
       i1 <- readIntArray arr 1
       return (i1))

intToFloat (i1) = runST (
    do arr <- newFloatArray (1,1)
       writeIntArray arr 1 i1
       readFloatArray arr 1)
