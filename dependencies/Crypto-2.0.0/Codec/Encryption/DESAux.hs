module Codec.Encryption.DESAux (des_enc, des_dec) where

import Word
import Bits

type Rotation = Int
type Key     = Word64
type Message = Word64
type Enc     = Word64

type BitsX  = [Bool]
type Bits4  = [Bool]
type Bits6  = [Bool]
type Bits32 = [Bool]
type Bits48 = [Bool]
type Bits56 = [Bool]
type Bits64 = [Bool]

instance Num [Bool]

instance Bits [Bool] where
 a `xor` b = (zipWith (\x y -> (not x && y) || (x && not y)) a b)
 rotate bits rot = drop rot' bits ++ take rot' bits
  where rot' = rot `mod` (length bits)

bitify :: Word64 -> Bits64
bitify w = map (\b -> w .&. (shiftL 1 b) /= 0) [63,62..0]

unbitify :: Bits64 -> Word64
unbitify bs = foldl (\i b -> if b then 1 + shiftL i 1 else shiftL i 1) 0 bs

initial_permutation :: Bits64 -> Bits64
initial_permutation mb = map ((!!) mb) i
 where i = [57, 49, 41, 33, 25, 17,  9, 1, 59, 51, 43, 35, 27, 19, 11, 3,
            61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7,
            56, 48, 40, 32, 24, 16,  8, 0, 58, 50, 42, 34, 26, 18, 10, 2,
            60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22, 14, 6]

key_transformation :: Bits64 -> Bits56
key_transformation kb = map ((!!) kb) i
 where i = [56, 48, 40, 32, 24, 16,  8,  0, 57, 49, 41, 33, 25, 17,
             9,  1, 58, 50, 42, 34, 26, 18, 10,  2, 59, 51, 43, 35,
            62, 54, 46, 38, 30, 22, 14,  6, 61, 53, 45, 37, 29, 21,
            13,  5, 60, 52, 44, 36, 28, 20, 12,  4, 27, 19, 11,  3]

des_enc :: Message -> Key -> Enc
des_enc = do_des [1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28]

des_dec :: Message -> Key -> Enc
des_dec = do_des [28,27,25,23,21,19,17,15,14,12,10,8,6,4,2,1]

do_des :: [Rotation] -> Message -> Key -> Enc
do_des rots m k = des_work rots (takeDrop 32 mb) kb
 where kb = key_transformation $ bitify k
       mb = initial_permutation $ bitify m

des_work :: [Rotation] -> (Bits32, Bits32) -> Bits56 -> Enc
des_work [] (ml, mr) _ = unbitify $ final_perm $ (mr ++ ml)
des_work (r:rs) mb kb = des_work rs mb' kb
 where mb' = do_round r mb kb

do_round :: Rotation -> (Bits32, Bits32) -> Bits56 -> (Bits32, Bits32)
do_round r (ml, mr) kb = (mr, m')
 where kb' = get_key kb r
       comp_kb = compression_permutation kb'
       expa_mr = expansion_permutation mr
       res = comp_kb `xor` expa_mr
       res' = tail $ iterate (trans 6) ([], res)
       trans n (_, b) = (take n b, drop n b)
       res_s = concat $ zipWith (\f (x,_) -> f x) [s_box_1, s_box_2,
                                                   s_box_3, s_box_4,
                                                   s_box_5, s_box_6,
                                                   s_box_7, s_box_8] res'
       res_p = p_box res_s
       m' = res_p `xor` ml

get_key :: Bits56 -> Rotation -> Bits56
get_key kb r = kb'
 where (kl, kr) = takeDrop 28 kb
       kb' = rotateL kl r ++ rotateL kr r

compression_permutation :: Bits56 -> Bits48
compression_permutation kb = map ((!!) kb) i
 where i = [13, 16, 10, 23,  0,  4,  2, 27, 14,  5, 20,  9,
            22, 18, 11,  3, 25,  7, 15,  6, 26, 19, 12,  1,
            40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
            43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31]

expansion_permutation :: Bits32 -> Bits48
expansion_permutation mb = map ((!!) mb) i
 where i = [31,  0,  1,  2,  3,  4,  3,  4,  5,  6,  7,  8,
             7,  8,  9, 10, 11, 12, 11, 12, 13, 14, 15, 16,
            15, 16, 17, 18, 19, 20, 19, 20, 21, 22, 23, 24,
            23, 24, 25, 26, 27, 28, 27, 28, 29, 30, 31,  0]

s_box :: [[Word8]] -> Bits6 -> Bits4
s_box s [a,b,c,d,e,f] = to_bool 4 $ (s !! row) !! col
 where row = sum $ zipWith numericise [a,f]     [1, 0]
       col = sum $ zipWith numericise [b,c,d,e] [3, 2, 1, 0]
       numericise = (\x y -> if x then 2^y else 0)
       to_bool 0 _ = []
       to_bool n i = ((i .&. 8) == 8):to_bool (n-1) (shiftL i 1)

s_box_1 :: Bits6 -> Bits4
s_box_1 = s_box i
 where i = [[14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7],
            [ 0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8],
            [ 4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0],
            [15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13]]

s_box_2 :: Bits6 -> Bits4
s_box_2 = s_box i
 where i = [[15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10],
            [3,  13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9,  11, 5],
            [0,  14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15],
            [13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5,  14, 9]]

s_box_3 :: Bits6 -> Bits4
s_box_3 = s_box i
 where i = [[10,  0,  9, 14 , 6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8],
            [13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1],
            [13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7],
            [1,  10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12]]

s_box_4 :: Bits6 -> Bits4
s_box_4 = s_box i
 where i = [[7,  13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15],
            [13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9],
            [10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4],
            [3,  15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14]]

s_box_5 :: Bits6 -> Bits4
s_box_5 = s_box i
 where i = [[2,  12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9],
            [14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6],
            [4,   2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14],
            [11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3]]

s_box_6 :: Bits6 -> Bits4
s_box_6 = s_box i
 where i = [[12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11],
            [10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8],
            [9,  14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6],
            [4,  3,   2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13]]

s_box_7 :: Bits6 -> Bits4
s_box_7 = s_box i
 where i = [[4,  11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1],
            [13, 0,  11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6],
            [1,  4,  11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2],
            [6,  11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12]]

s_box_8 :: Bits6 -> Bits4
s_box_8 = s_box i
 where i = [[13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7],
            [1,  15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2],
            [7,  11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8],
            [2,   1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11]]

p_box :: Bits32 -> Bits32
p_box kb = map ((!!) kb) i
 where i = [15, 6, 19, 20, 28, 11, 27, 16,  0, 14, 22, 25,  4, 17, 30,  9,
             1, 7, 23, 13, 31, 26,  2,  8, 18, 12, 29,  5, 21, 10,  3, 24]

final_perm :: Bits64 -> Bits64
final_perm kb = map ((!!) kb) i
 where i = [39, 7, 47, 15, 55, 23, 63, 31, 38, 6, 46, 14, 54, 22, 62, 30,
            37, 5, 45, 13, 53, 21, 61, 29, 36, 4, 44, 12, 52, 20, 60, 28,
            35, 3, 43, 11, 51, 19, 59, 27, 34, 2, 42, 10, 50, 18, 58, 26,
            33, 1, 41,  9, 49, 17, 57, 25, 32, 0, 40 , 8, 48, 16, 56, 24]

takeDrop :: Int -> [a] -> ([a], [a])
takeDrop _ [] = ([], [])
takeDrop 0 xs = ([], xs)
takeDrop n (x:xs) = (x:ys, zs)
 where (ys, zs) = takeDrop (n-1) xs

