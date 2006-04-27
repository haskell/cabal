--Copyright 2001, 2002, 2003 David J. Sankel
--
--This file is part of rsa-haskell.
--rsa-haskell is free software; you can redistribute it and/or modify
--it under the terms of the GNU General Public License as published by
--the Free Software Foundation; either version 2 of the License, or
--(at your option) any later version.
--
--rsa-haskell is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.
--
--You should have received a copy of the GNU General Public License
--along with rsa-haskell; if not, write to the Free Software
--Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

module Codec.Encryption.RSA.NumberTheory( 
inverse, extEuclGcd, simplePrimalityTest, getPrime, pg, isPrime, 
rabinMillerPrimalityTest, expmod, factor, testInverse, primes, (/|),
randomOctet
)    where


import Random(getStdRandom,randomR)
--The following line is required for ghc optomized implementation
--  (see comments beginning with GHC):
-- import Bits(setBit)
import List(elemIndex)
import Maybe(fromJust)
import Char(chr,ord)
import Bits(xor)

--Precondition: the integer is >= 0
randomOctet :: Int -> IO( String )
randomOctet n
  | n < 0 = error "randomOctet argument doesn't meet preconditions"
  | otherwise = (sequence $ take n $ repeat $ getStdRandom (randomR( 0,255) )) 
                  >>= (return . (map chr) )

--Returns a list [r_1,r_2,r_3,r_4, . . ., r_n ] where
--  a = p_1^r_1 * p_2^r_2 * p_3^r_3 * . . . * p_n^r_n
factor = factor_1

--An implimentation of factor
factor_1 :: Integer -> [Int]
factor_1 a = reverse . dropWhile (== 0) . reverse 
  . map (\x -> largestPower x a) . takeWhile (<= a ) $ primes

--Another implimentation of factor
factor_2 :: Integer -> [Integer]
factor_2 a = 
  let 
    p = map (fromIntegral) . reverse . dropWhile (== 0) 
      . reverse . map (\x -> largestPower x a) 
	  . takeWhile (<= a `div` 2) $ primes
  in
    if (length p == 0)
    then (take ((fromIntegral . fromJust $ elemIndex a primes)-1) (repeat 0)) 
	  ++ [1]
    else p
 
--Find the inverse of x (mod n)
inverse :: Integer -> Integer -> Integer
inverse x n = (fst (extEuclGcd x n)) `mod` n

testInverse :: Integer ->Integer -> Bool
testInverse a b = ((inverse a b)*a) `mod` b == 1 

--Extended Eucildean algorithm
--Returns (x,y) where gcd(a,b) = xa + yb
extEuclGcd :: Integer -> Integer -> (Integer,Integer)
extEuclGcd a b = extEuclGcd_iter a b (1,0) (0,1)

extEuclGcd_iter :: Integer -> Integer 
  -> (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
extEuclGcd_iter a b (c1,c2) (d1,d2)
  |  (a > b) && (r1 == 0)  = (d1,d2)
  |  (a > b) && (r1 /= 0)  = extEuclGcd_iter 
    (a - (q1*b)) b (c1 - (q1*d1), c2 - (q1*d2)) (d1,d2)
  |  (a <= b) && (r2 == 0) = (c1,c2)
  |  (a <= b) && (r2 /= 0) = extEuclGcd_iter 
    a (b - (q2*a)) (c1,c2) ( d1 - (q2*c1), d2- (q2*c2))
      where
        q1 = a `div` b
        q2 = b `div` a
        r1 = a `mod` b
        r2 = b `mod` a

-- This will return a random Integer of n bits.  The highest order bit
-- will always be 1.

-- GHC optomized implementation
-- getNumber :: Int -> IO Integer
-- getNumber n = do 
--                  i <- getStdRandom ( randomR (0, a-1 ) )
--                  return (setBit i (n-1))
--               where
--                   a = (2^n) ::Integer

--This is the portable version
getNumber :: Int -> IO Integer
getNumber n = do 
                 i <- getStdRandom ( randomR (0, a-1 ) )
                 return (i+(2^(n-1)))
              where
                  a = (2^(n-1)) ::Integer

--Returns a probable prime number of nBits bits

-- GHC optomized implementation
-- getPrime  :: Int -> IO Integer
-- getPrime nBits = do
--                 r <- getNumber nBits
--                 let p = (setBit r 0) --Make it odd for speed
--                 pIsPrime <- isPrime p
--                 if( pIsPrime )
--                    then return p
--                    else getPrime nBits

--This is the portable version
getPrime  :: Int -> IO Integer
getPrime nBits = do
                r <- getNumber nBits
                let p = if( 2 /| r ) then r else r+1
                pIsPrime <- isPrime p
                if( pIsPrime )
                   then return p
                   else getPrime nBits

--Prime Generate:
--Generates a prime p | minimum <= p <= maximum and gcd p e  == 1
pg :: Integer -> Integer -> Integer -> IO(Integer)
pg minimum maximum e = do
  p <- getStdRandom( randomR( minimum, maximum ) )
  pIsPrime <- isPrime p
  if( pIsPrime && (gcd p e) == 1 )
    then return p
    else pg minimum maximum e

isPrime :: Integer -> IO Bool
isPrime a
  | (a <= 1)    = return False
  | (a <= 2000) = return (simplePrimalityTest a)
  | otherwise   = if (simplePrimalityTest a)
                    then do --Do this 5 times for saftey
                      test <- mapM rabinMillerPrimalityTest $ take 5 $ repeat a
                      return (and test)
                    else return False

simplePrimalityTest :: Integer -> Bool
simplePrimalityTest a = foldr (&&) True (map (/| a)(takeWhile (<it) primes))
  where it = min 2000 a

--returns greatest z where x^z | y
largestPower :: Integer -> Integer -> Int
largestPower x y = fromJust . elemIndex False 
  . map (\b -> (y `mod` x^b) == 0) $ [1..]

rabinMillerPrimalityTest :: Integer -> IO Bool
rabinMillerPrimalityTest p = rabinMillerPrimalityTest_iter_1 p b m
                                 where
                                   b = fromIntegral $ largestPower 2 (p-1)
                                   m = (p-1) `div` (2^b)

--The ?prime? Number -> The amount of iterations -> b -> m
rabinMillerPrimalityTest_iter_1 :: Integer -> Integer -> Integer -> IO Bool
rabinMillerPrimalityTest_iter_1 p b m =
              do
                a <- getStdRandom ( randomR (0, 2000 ) )
                return (rabinMillerPrimalityTest_iter_2 p b 0 (expmod a m p))

rabinMillerPrimalityTest_iter_2 :: Integer -> Integer -> Integer -> Integer 
  -> Bool
rabinMillerPrimalityTest_iter_2 p b j z 
  | (z == 1)   || (z == p-1)       = True
  | (j > 0)    && (z == 1)         = False
  | (j+1 < b)  && (z /= p-1)       = 
    (rabinMillerPrimalityTest_iter_2 p b (j+1) ((z^2) `mod` p ))
  | z == p - 1                     = True
  | (j+1 == b) && (z /= p-1)       = False

--a^x (mod m)
expmod :: Integer -> Integer -> Integer -> Integer
expmod a x m |  x == 0    = 1
             |  x == 1    = a `mod` m
             |  even x    = let p = (expmod a (x `div` 2) m) `mod` m
                            in  (p^2) `mod` m
             |  otherwise = (a * expmod a (x-1) m) `mod` m

--Largest x where x^2 < i
intSqrt :: Integer -> Integer
intSqrt i = floor (sqrt (fromIntegral i ) )

--The doesn't divide function
(/|) :: Integer -> Integer -> Bool
a /| b = b `mod` a /= 0

--List of primes
primes :: [Integer]
primes = 2:[x | x <- [3,5..], foldr (&&) True 
          ( map ( /| x ) (takeWhile (<=(intSqrt x)) primes ) ) ]
