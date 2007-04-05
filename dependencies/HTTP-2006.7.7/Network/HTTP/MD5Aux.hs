module Network.HTTP.MD5Aux 
   (md5,  md5s,  md5i,
    MD5(..), ABCD(..), 
    Zord64, Str(..), BoolList(..), WordList(..)) where

import Data.Char
import Data.Bits
import Data.Word

{-
Nasty kludge to create a type Zord64 which is really a Word64 but works
how we want in hugs ands nhc98 too...
Also need a rotate left function that actually works.

#ifdef __GLASGOW_HASKELL__
#define rotL rotateL
#include "Zord64_EASY.hs"
#else

> import Zord64_HARD
 
> rotL :: Word32 -> Rotation -> Word32
> rotL a s = shiftL a s .|. shiftL a (s-32)

#endif
-}

rotL x = rotateL x
type Zord64 = Word64

-- ===================== TYPES AND CLASS DEFINTIONS ========================


type XYZ = (Word32, Word32, Word32)
type Rotation = Int
newtype ABCD = ABCD (Word32, Word32, Word32, Word32) deriving (Eq, Show)
newtype Str = Str String
newtype BoolList = BoolList [Bool]
newtype WordList = WordList ([Word32], Zord64)

-- Anything we want to work out the MD5 of must be an instance of class MD5

class MD5 a where
 get_next :: a -> ([Word32], Int, a) -- get the next blocks worth
 --                     \      \   \------ the rest of the input
 --                      \      \--------- the number of bits returned
 --                       \--------------- the bits returned in 32bit words
 len_pad :: Zord64 -> a -> a         -- append the padding and length
 finished :: a -> Bool               -- Have we run out of input yet?


-- Mainly exists because it's fairly easy to do MD5s on input where the
-- length is not a multiple of 8

instance MD5 BoolList where
 get_next (BoolList s) = (bools_to_word32s ys, length ys, BoolList zs)
  where (ys, zs) = splitAt 512 s
 len_pad l (BoolList bs)
  = BoolList (bs ++ [True]
                 ++ replicate (fromIntegral $ (447 - l) .&. 511) False
                 ++ [l .&. (shiftL 1 x) > 0 | x <- (mangle [0..63])]
             )
  where mangle [] = []
        mangle xs = reverse ys ++ mangle zs
         where (ys, zs) = splitAt 8 xs
 finished (BoolList s) = s == []


-- The string instance is fairly straightforward

instance MD5 Str where
 get_next (Str s) = (string_to_word32s ys, 8 * length ys, Str zs)
  where (ys, zs) = splitAt 64 s
 len_pad c64 (Str s) = Str (s ++ padding ++ l)
  where padding = '\128':replicate (fromIntegral zeros) '\000'
        zeros = shiftR ((440 - c64) .&. 511) 3
        l = length_to_chars 8 c64
 finished (Str s) = s == ""


-- YA instance that is believed will be useful

instance MD5 WordList where
 get_next (WordList (ws, l)) = (xs, fromIntegral taken, WordList (ys, l - taken))
  where (xs, ys) = splitAt 16 ws
        taken = if l > 511 then 512 else l .&. 511
 len_pad c64 (WordList (ws, l)) = WordList (beginning ++ nextish ++ blanks ++ size, newlen)
  where beginning = if length ws > 0 then start ++ lastone' else []
        start = init ws
        lastone = last ws
        offset = c64 .&. 31
        lastone' = [if offset > 0 then lastone + theone else lastone]
        theone = shiftL (shiftR 128 (fromIntegral $ offset .&. 7))
                        (fromIntegral $ offset .&. (31 - 7))
        nextish = if offset == 0 then [128] else []
        c64' = c64 + (32 - offset)
        num_blanks = (fromIntegral $ shiftR ((448 - c64') .&. 511) 5)
        blanks = replicate num_blanks 0
        lowsize = fromIntegral $ c64 .&. (shiftL 1 32 - 1)
        topsize = fromIntegral $ shiftR c64 32
        size = [lowsize, topsize]
        newlen = l .&. (complement 511)
               + if c64 .&. 511 >= 448 then 1024 else 512
 finished (WordList (_, z)) = z == 0


instance Num ABCD where
 ABCD (a1, b1, c1, d1) + ABCD (a2, b2, c2, d2) = ABCD (a1 + a2, b1 + b2, c1 + c2, d1 + d2)


-- ===================== EXPORTED FUNCTIONS ========================


-- The simplest function, gives you the MD5 of a string as 4-tuple of
-- 32bit words.

md5 :: (MD5 a) => a -> ABCD
md5 m = md5_main False 0 magic_numbers m


-- Returns a hex number ala the md5sum program

md5s :: (MD5 a) => a -> String
md5s = abcd_to_string . md5


-- Returns an integer equivalent to the above hex number

md5i :: (MD5 a) => a -> Integer
md5i = abcd_to_integer . md5


-- ===================== THE CORE ALGORITHM ========================


-- Decides what to do. The first argument indicates if padding has been
-- added. The second is the length mod 2^64 so far. Then we have the
-- starting state, the rest of the string and the final state.

md5_main :: (MD5 a) =>
            Bool   -- Have we added padding yet?
         -> Zord64 -- The length so far mod 2^64
         -> ABCD   -- The initial state
         -> a      -- The non-processed portion of the message
         -> ABCD   -- The resulting state
md5_main padded ilen abcd m
 = if finished m && padded
   then abcd
   else md5_main padded' (ilen + 512) (abcd + abcd') m''
 where (m16, l, m') = get_next m
       len' = ilen + fromIntegral l
       ((m16', _, m''), padded') = if not padded && l < 512
                                   then (get_next $ len_pad len' m, True)
                                   else ((m16, l, m'), padded)
       abcd' = md5_do_block abcd m16'


-- md5_do_block processes a 512 bit block by calling md5_round 4 times to
-- apply each round with the correct constants and permutations of the
-- block

md5_do_block :: ABCD     -- Initial state
             -> [Word32] -- The block to be processed - 16 32bit words
             -> ABCD     -- Resulting state
md5_do_block abcd0 w = abcd4
 where (r1, r2, r3, r4) = rounds
       {-
       map (\x -> w !! x) [1,6,11,0,5,10,15,4,9,14,3,8,13,2,7,12]
                       -- [(5 * x + 1) `mod` 16 | x <- [0..15]]
       map (\x -> w !! x) [5,8,11,14,1,4,7,10,13,0,3,6,9,12,15,2]
                       -- [(3 * x + 5) `mod` 16 | x <- [0..15]]
       map (\x -> w !! x) [0,7,14,5,12,3,10,1,8,15,6,13,4,11,2,9]
                       -- [(7 * x) `mod` 16 | x <- [0..15]]
       -}
       perm5 [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
        = [c1,c6,c11,c0,c5,c10,c15,c4,c9,c14,c3,c8,c13,c2,c7,c12]
       perm5 _ = error "broke at perm5"
       perm3 [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
        = [c5,c8,c11,c14,c1,c4,c7,c10,c13,c0,c3,c6,c9,c12,c15,c2]
       perm3 _ = error "broke at perm3"
       perm7 [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15]
        = [c0,c7,c14,c5,c12,c3,c10,c1,c8,c15,c6,c13,c4,c11,c2,c9]
       perm7 _ = error "broke at perm7"
       abcd1 = md5_round md5_f abcd0        w  r1
       abcd2 = md5_round md5_g abcd1 (perm5 w) r2
       abcd3 = md5_round md5_h abcd2 (perm3 w) r3
       abcd4 = md5_round md5_i abcd3 (perm7 w) r4


-- md5_round does one of the rounds. It takes an auxiliary function and foldls
-- (md5_inner_function f) to repeatedly apply it to the initial state with the
-- correct constants

md5_round :: (XYZ -> Word32)      -- Auxiliary function (F, G, H or I
                                  -- for those of you with a copy of
                                  -- the prayer book^W^WRFC)
          -> ABCD                 -- Initial state
          -> [Word32]             -- The 16 32bit words of input
          -> [(Rotation, Word32)] -- The list of 16 rotations and
                                  -- additive constants
          -> ABCD                 -- Resulting state
md5_round f abcd s ns = foldl (md5_inner_function f) abcd ns'
 where ns' = zipWith (\x (y, z) -> (y, x + z)) s ns


-- Apply one of the functions md5_[fghi] and put the new ABCD together

md5_inner_function :: (XYZ -> Word32)    -- Auxiliary function
                   -> ABCD               -- Initial state
                   -> (Rotation, Word32) -- The rotation and additive
                                         -- constant (X[i] + T[j])
                   -> ABCD               -- Resulting state
md5_inner_function f (ABCD (a, b, c, d)) (s, ki) = ABCD (d, a', b, c)
 where mid_a = a + f(b,c,d) + ki
       rot_a = rotL mid_a s
       a' = b + rot_a


-- The 4 auxiliary functions

md5_f :: XYZ -> Word32
md5_f (x, y, z) = z `xor` (x .&. (y `xor` z))
{- optimised version of: (x .&. y) .|. ((complement x) .&. z) -}

md5_g :: XYZ -> Word32
md5_g (x, y, z) = md5_f (z, x, y)
{- was: (x .&. z) .|. (y .&. (complement z)) -}

md5_h :: XYZ -> Word32
md5_h (x, y, z) = x `xor` y `xor` z

md5_i :: XYZ -> Word32
md5_i (x, y, z) = y `xor` (x .|. (complement z))


-- The magic numbers from the RFC.

magic_numbers :: ABCD
magic_numbers = ABCD (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)


-- The 4 lists of (rotation, additive constant) tuples, one for each round

rounds :: ([(Rotation, Word32)],
           [(Rotation, Word32)],
           [(Rotation, Word32)],
           [(Rotation, Word32)])
rounds = (r1, r2, r3, r4)
 where r1 = [(s11, 0xd76aa478), (s12, 0xe8c7b756), (s13, 0x242070db),
             (s14, 0xc1bdceee), (s11, 0xf57c0faf), (s12, 0x4787c62a),
             (s13, 0xa8304613), (s14, 0xfd469501), (s11, 0x698098d8),
             (s12, 0x8b44f7af), (s13, 0xffff5bb1), (s14, 0x895cd7be),
             (s11, 0x6b901122), (s12, 0xfd987193), (s13, 0xa679438e),
             (s14, 0x49b40821)]
       r2 = [(s21, 0xf61e2562), (s22, 0xc040b340), (s23, 0x265e5a51),
             (s24, 0xe9b6c7aa), (s21, 0xd62f105d), (s22,  0x2441453),
             (s23, 0xd8a1e681), (s24, 0xe7d3fbc8), (s21, 0x21e1cde6),
             (s22, 0xc33707d6), (s23, 0xf4d50d87), (s24, 0x455a14ed),
             (s21, 0xa9e3e905), (s22, 0xfcefa3f8), (s23, 0x676f02d9),
             (s24, 0x8d2a4c8a)]
       r3 = [(s31, 0xfffa3942), (s32, 0x8771f681), (s33, 0x6d9d6122),
             (s34, 0xfde5380c), (s31, 0xa4beea44), (s32, 0x4bdecfa9),
             (s33, 0xf6bb4b60), (s34, 0xbebfbc70), (s31, 0x289b7ec6),
             (s32, 0xeaa127fa), (s33, 0xd4ef3085), (s34,  0x4881d05),
             (s31, 0xd9d4d039), (s32, 0xe6db99e5), (s33, 0x1fa27cf8),
             (s34, 0xc4ac5665)]
       r4 = [(s41, 0xf4292244), (s42, 0x432aff97), (s43, 0xab9423a7),
             (s44, 0xfc93a039), (s41, 0x655b59c3), (s42, 0x8f0ccc92),
             (s43, 0xffeff47d), (s44, 0x85845dd1), (s41, 0x6fa87e4f),
             (s42, 0xfe2ce6e0), (s43, 0xa3014314), (s44, 0x4e0811a1),
             (s41, 0xf7537e82), (s42, 0xbd3af235), (s43, 0x2ad7d2bb),
             (s44, 0xeb86d391)]
       s11 = 7
       s12 = 12
       s13 = 17
       s14 = 22
       s21 = 5
       s22 = 9
       s23 = 14
       s24 = 20
       s31 = 4
       s32 = 11
       s33 = 16
       s34 = 23
       s41 = 6
       s42 = 10
       s43 = 15
       s44 = 21


-- ===================== CONVERSION FUNCTIONS ========================


-- Turn the 4 32 bit words into a string representing the hex number they
-- represent.

abcd_to_string :: ABCD -> String
abcd_to_string (ABCD (a,b,c,d)) = concat $ map display_32bits_as_hex [a,b,c,d]


-- Split the 32 bit word up, swap the chunks over and convert the numbers
-- to their hex equivalents.

display_32bits_as_hex :: Word32 -> String
display_32bits_as_hex w = swap_pairs cs
 where cs = map (\x -> getc $ (shiftR w (4*x)) .&. 15) [0..7]
       getc n = (['0'..'9'] ++ ['a'..'f']) !! (fromIntegral n)
       swap_pairs (x1:x2:xs) = x2:x1:swap_pairs xs
       swap_pairs _ = []

-- Convert to an integer, performing endianness magic as we go

abcd_to_integer :: ABCD -> Integer
abcd_to_integer (ABCD (a,b,c,d)) = rev_num a * 2^(96 :: Int)
                                 + rev_num b * 2^(64 :: Int)
                                 + rev_num c * 2^(32 :: Int)
                                 + rev_num d

rev_num :: Word32 -> Integer
rev_num i = toInteger j `mod` (2^(32 :: Int))
 --         NHC's fault ~~~~~~~~~~~~~~~~~~~~~
 where j = foldl (\so_far next -> shiftL so_far 8 + (shiftR i next .&. 255))
                 0 [0,8,16,24]

-- Used to convert a 64 byte string to 16 32bit words

string_to_word32s :: String -> [Word32]
string_to_word32s "" = []
string_to_word32s ss = this:string_to_word32s ss'
 where (s, ss') = splitAt 4 ss
       this = foldr (\c w -> shiftL w 8 + (fromIntegral.ord) c) 0 s


-- Used to convert a list of 512 bools to 16 32bit words

bools_to_word32s :: [Bool] -> [Word32]
bools_to_word32s [] = []
bools_to_word32s bs = this:bools_to_word32s rest
 where (bs1, bs1') = splitAt 8 bs
       (bs2, bs2') = splitAt 8 bs1'
       (bs3, bs3') = splitAt 8 bs2'
       (bs4, rest) = splitAt 8 bs3'
       this = boolss_to_word32 [bs1, bs2, bs3, bs4]
       bools_to_word8 = foldl (\w b -> shiftL w 1 + if b then 1 else 0) 0
       boolss_to_word32 = foldr (\w8 w -> shiftL w 8 + bools_to_word8 w8) 0


-- Convert the size into a list of characters used by the len_pad function
-- for strings

length_to_chars :: Int -> Zord64 -> String
length_to_chars 0 _ = []
length_to_chars p n = this:length_to_chars (p-1) (shiftR n 8)
         where this = chr $ fromIntegral $ n .&. 255

