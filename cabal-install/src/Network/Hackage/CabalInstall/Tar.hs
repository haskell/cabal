-- | Simplistic TAR archive reading. Only gets the file names and file contents.
module Network.Hackage.CabalInstall.Tar (readTarArchive) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (ord)
import Data.Int (Int8, Int64)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Numeric (readOct)


readTarArchive :: ByteString -> [(FilePath,ByteString)]
readTarArchive = catMaybes . unfoldr getTarEntry

getTarEntry :: ByteString -> Maybe (Maybe (FilePath,ByteString), ByteString)
getTarEntry bs | endBlock = Nothing
               | BS.length hdr < 512 = error "Truncated TAR archive."
               | not (checkChkSum hdr chkSum) = error "TAR checksum error."
               | not normalFile = Just (Nothing, bs''')
               | otherwise = Just (Just (path, cnt), bs''')

   where (hdr,bs') = BS.splitAt 512 bs

         endBlock  = getByte 0 hdr == '\0'

         fileSuffix = getString   0 100 hdr
         chkSum     = getOct    148   8 hdr
         typ        = getByte   156     hdr
         size       = getOct    124  12 hdr
         filePrefix = getString 345 155 hdr

         normalFile = typ == '0' || typ == '\0'
         path       = filePrefix ++ fileSuffix

         padding    = (512 - size) `mod` 512
         (cnt,bs'') = BS.splitAt size bs'
         bs'''      = BS.drop padding bs''

checkChkSum :: ByteString -> Int -> Bool
checkChkSum hdr s = s == chkSum hdr' || s == signedChkSum hdr'
  where 
    -- replace the checksum with spaces
    hdr' = BS.concat [BS.take 148 hdr, BS.replicate 8 ' ', BS.drop 156 hdr]
    -- tar.info says that Sun tar is buggy and 
    -- calculates the checksum using signed chars
    chkSum = BS.foldl' (\x y -> x + ord y) 0
    signedChkSum = BS.foldl' (\x y -> x + (ordSigned y)) 0

ordSigned :: Char -> Int
ordSigned c = fromIntegral (fromIntegral (ord c) :: Int8)

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> a
getOct off len = parseOct . getString off len
  where parseOct "" = 0
        parseOct s = case readOct s of
                       [(x,_)] -> x
                       _       -> error $ "Number format error: " ++ show s

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.index bs off

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.unpack . BS.takeWhile (/='\0') . getBytes off len
