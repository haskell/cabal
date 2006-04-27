module Data.Digest.SHA1Aux (sha1, sha1_size) where

import Char
import Bits
import Word

type ABCDE = (Word32, Word32, Word32, Word32, Word32)
type XYZ = (Word32, Word32, Word32)
type Rotation = Int

sha1 :: String -> String
sha1 s = s5
 where s1_2 = sha1_step_1_2_pad_length s
       abcde = sha1_step_3_init
       abcde' = sha1_step_4_main abcde s1_2
       s5 = sha1_step_5_display abcde'

sha1_size :: (Integral a) => a -> String -> String
sha1_size l s = s5
 where s1_2 = s ++ sha1_step_1_2_work (fromIntegral ((toInteger l) `mod` (2^64))) ""
       abcde = sha1_step_3_init
       abcde' = sha1_step_4_main abcde s1_2
       s5 = sha1_step_5_display abcde'

sha1_step_1_2_pad_length :: String -> String
sha1_step_1_2_pad_length s = sha1_step_1_2_work 0 s

sha1_step_1_2_work :: Integer -> String -> String
sha1_step_1_2_work c64 "" = padding ++ len
 where padding = '\128':replicate' (shiftR (fromIntegral $ (440 - c64) `mod` 512) 3) '\000'
       len = map chr $ size_split 8 c64
sha1_step_1_2_work c64 (c:cs) = c:sha1_step_1_2_work ((c64 + 8) `mod` (2^64)) cs

replicate' :: Word16 -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

size_split :: Int -> Integer -> [Int]
size_split 0 _ = []
size_split p n = size_split (p-1) n' ++ [fromIntegral d]
 where (n', d) = divMod n 256

sha1_step_3_init :: ABCDE
sha1_step_3_init = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0)

sha1_step_4_main :: ABCDE -> String -> ABCDE
sha1_step_4_main abcde "" = {- abcde -} abcde
sha1_step_4_main abcde0 s = sha1_step_4_main abcde5 s'
 where (s64, s') = takeDrop 64 s
       s16 = get_word_32s s64
       s80 = s16 ++ sha1_add_ws 16 (drop 13 s16, drop 8 s16, drop 2 s16, s16)
       (s20_0, s60) = takeDrop 20 s80
       (s20_1, s40) = takeDrop 20 s60
       (s20_2, s20_3) = takeDrop 20 s40
       abcde1 = foldl (doit f1 0x5a827999) abcde0 s20_0
       abcde2 = foldl (doit f2 0x6ed9eba1) abcde1 s20_1
       abcde3 = foldl (doit f3 0x8f1bbcdc) abcde2 s20_2
       abcde4 = foldl (doit f2 0xca62c1d6) abcde3 s20_3
       f1 (x, y, z) = (x .&. y) .|. ((complement x) .&. z)
       f2 (x, y, z) = x `xor` y `xor` z
       f3 (x, y, z) = (x .&. y) .|. (x .&. z) .|. (y .&. z)
       (a,  b,  c,  d,  e ) = abcde0
       (a', b', c', d', e') = abcde4
       abcde5 = (a + a', b + b', c + c', d + d', e + e')

doit :: (XYZ -> Word32) -> Word32 -> ABCDE -> Word32 -> ABCDE
doit f k (a, b, c, d, e) w = (a', a, rotL b 30, c, d)
 where a' = rotL a 5 + f(b, c, d) + e + w + k

sha1_add_ws :: Int -> ([Word32], [Word32], [Word32], [Word32]) -> [Word32]
sha1_add_ws 80 _ = []
sha1_add_ws n (w1:w1s, w2:w2s, w3:w3s, w4:w4s)
 = w:sha1_add_ws (n + 1) (w1s ++ [w], w2s ++ [w], w3s ++ [w], w4s ++ [w])
 where w = rotL (foldr1 xor [w1, w2, w3, w4]) 1

get_word_32s :: String -> [Word32]
get_word_32s "" = []
get_word_32s ss = this:rest
 where (s, ss') = takeDrop 4 ss
       this = sum $ zipWith shiftL (map (fromIntegral.ord) s) [24, 16, 8, 0]
       rest = get_word_32s ss'

takeDrop :: Int -> [a] -> ([a], [a])
takeDrop _ [] = ([], [])
takeDrop 0 xs = ([], xs)
takeDrop n (x:xs) = (x:ys, zs)
 where (ys, zs) = takeDrop (n-1) xs

sha1_step_5_display :: ABCDE -> String
sha1_step_5_display (a, b, c, d, e)
 = foldr (\x y -> display_32bits_as_hex x ++ y) "" [a, b, c, d, e]

display_32bits_as_hex :: Word32 -> String
display_32bits_as_hex x0 = map getc [y8,y7,y6,y5,y4,y3,y2,y1]
 where (x1, y1) = divMod x0 16
       (x2, y2) = divMod x1 16
       (x3, y3) = divMod x2 16
       (x4, y4) = divMod x3 16
       (x5, y5) = divMod x4 16
       (x6, y6) = divMod x5 16
       (y8, y7) = divMod x6 16
       getc n = (['0'..'9'] ++ ['a'..'f']) !! (fromIntegral n)

rotL :: Word32 -> Rotation -> Word32
rotL a s = shiftL a s .|. shiftL a (s-32)

