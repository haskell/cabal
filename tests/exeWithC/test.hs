{-# CFILES a.c #-}
foreign import ccall unsafe "foo" foo :: Int -> Int

main = print $ foo 6
