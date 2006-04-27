-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Binary.Base64
-- Copyright   :  (c) Dominic Steinitz 2005, Warrick Gray 2002
-- License     :  BSD-style (see the file ReadMe.tex)
--
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Base64 encoding and decoding functions provided by Warwick Gray. 
-- See <http://homepages.paradise.net.nz/warrickg/haskell/http/#base64> 
-- and <http://www.faqs.org/rfcs/rfc2045.html>.
--
-----------------------------------------------------------------------------

module Codec.Binary.Base64 (
    encode,
    decode,
    chop72
) where



{------------------------------------------------------------------------
This is what RFC2045 had to say:

6.8.  Base64 Content-Transfer-Encoding

   The Base64 Content-Transfer-Encoding is designed to represent
   arbitrary sequences of octets in a form that need not be humanly
   readable.  The encoding and decoding algorithms are simple, but the
   encoded data are consistently only about 33 percent larger than the
   unencoded data.  This encoding is virtually identical to the one used
   in Privacy Enhanced Mail (PEM) applications, as defined in RFC 1421.

   A 65-character subset of US-ASCII is used, enabling 6 bits to be
   represented per printable character. (The extra 65th character, "=",
   is used to signify a special processing function.)

   NOTE:  This subset has the important property that it is represented
   identically in all versions of ISO 646, including US-ASCII, and all
   characters in the subset are also represented identically in all
   versions of EBCDIC. Other popular encodings, such as the encoding
   used by the uuencode utility, Macintosh binhex 4.0 [RFC-1741], and
   the base85 encoding specified as part of Level 2 PostScript, do not
   share these properties, and thus do not fulfill the portability
   requirements a binary transport encoding for mail must meet.

   The encoding process represents 24-bit groups of input bits as output
   strings of 4 encoded characters.  Proceeding from left to right, a
   24-bit input group is formed by concatenating 3 8bit input groups.
   These 24 bits are then treated as 4 concatenated 6-bit groups, each
   of which is translated into a single digit in the base64 alphabet.
   When encoding a bit stream via the base64 encoding, the bit stream
   must be presumed to be ordered with the most-significant-bit first.
   That is, the first bit in the stream will be the high-order bit in
   the first 8bit byte, and the eighth bit will be the low-order bit in
   the first 8bit byte, and so on.

   Each 6-bit group is used as an index into an array of 64 printable
   characters.  The character referenced by the index is placed in the
   output string.  These characters, identified in Table 1, below, are
   selected so as to be universally representable, and the set excludes
   characters with particular significance to SMTP (e.g., ".", CR, LF)
   and to the multipart boundary delimiters defined in RFC 2046 (e.g.,
   "-").



                    Table 1: The Base64 Alphabet

     Value Encoding  Value Encoding  Value Encoding  Value Encoding
         0 A            17 R            34 i            51 z
         1 B            18 S            35 j            52 0
         2 C            19 T            36 k            53 1
         3 D            20 U            37 l            54 2
         4 E            21 V            38 m            55 3
         5 F            22 W            39 n            56 4
         6 G            23 X            40 o            57 5
         7 H            24 Y            41 p            58 6
         8 I            25 Z            42 q            59 7
         9 J            26 a            43 r            60 8
        10 K            27 b            44 s            61 9
        11 L            28 c            45 t            62 +
        12 M            29 d            46 u            63 /
        13 N            30 e            47 v
        14 O            31 f            48 w         (pad) =
        15 P            32 g            49 x
        16 Q            33 h            50 y

   The encoded output stream must be represented in lines of no more
   than 76 characters each.  All line breaks or other characters not
   found in Table 1 must be ignored by decoding software.  In base64
   data, characters other than those in Table 1, line breaks, and other
   white space probably indicate a transmission error, about which a
   warning message or even a message rejection might be appropriate
   under some circumstances.

   Special processing is performed if fewer than 24 bits are available
   at the end of the data being encoded.  A full encoding quantum is
   always completed at the end of a body.  When fewer than 24 input bits
   are available in an input group, zero bits are added (on the right)
   to form an integral number of 6-bit groups.  Padding at the end of
   the data is performed using the "=" character.  Since all base64
   input is an integral number of octets, only the following cases can
   arise: (1) the final quantum of encoding input is an integral
   multiple of 24 bits; here, the final unit of encoded output will be
   an integral multiple of 4 characters with no "=" padding, (2) the
   final quantum of encoding input is exactly 8 bits; here, the final
   unit of encoded output will be two characters followed by two "="
   padding characters, or (3) the final quantum of encoding input is
   exactly 16 bits; here, the final unit of encoded output will be three
   characters followed by one "=" padding character.

   Because it is used only for padding at the end of the data, the
   occurrence of any "=" characters may be taken as evidence that the
   end of the data has been reached (without truncation in transit).  No
   such assurance is possible, however, when the number of octets
   transmitted was a multiple of three and no "=" characters are
   present.

   Any characters outside of the base64 alphabet are to be ignored in
   base64-encoded data.

   Care must be taken to use the proper octets for line breaks if base64
   encoding is applied directly to text material that has not been
   converted to canonical form.  In particular, text line breaks must be
   converted into CRLF sequences prior to base64 encoding.  The
   important thing to note is that this may be done directly by the
   encoder rather than in a prior canonicalization step in some
   implementations.

   NOTE: There is no need to worry about quoting potential boundary
   delimiters within base64-encoded bodies within multipart entities
   because no hyphen characters are used in the base64 encoding.


----------------------------------------------------------------------------}


{-

The following properties should hold:

  decode . encode = id
  decode . chop72 . encode = id

I.E. Both "encode" and "chop72 . encode" are valid methods of encoding input,
the second variation corresponds better with the RFC above, but outside of
MIME applications might be undesireable.



But: The Haskell98 Char type is at least 16bits (and often 32), these implementations assume only 
     8 significant bits, which is more than enough for US-ASCII.  
-}




import Data.Array
import Data.Bits
import Data.Int
import Data.Char (chr,ord)
import Codec.Utils (Octet)

encodeArray :: Array Int Char
encodeArray = array (0,64) 
          [ (0,'A'),  (1,'B'),  (2,'C'),  (3,'D'),  (4,'E'),  (5,'F')                    
          , (6,'G'),  (7,'H'),  (8,'I'),  (9,'J'),  (10,'K'), (11,'L')                    
          , (12,'M'), (13,'N'), (14,'O'), (15,'P'), (16,'Q'), (17,'R')
          , (18,'S'), (19,'T'), (20,'U'), (21,'V'), (22,'W'), (23,'X')
          , (24,'Y'), (25,'Z'), (26,'a'), (27,'b'), (28,'c'), (29,'d')
          , (30,'e'), (31,'f'), (32,'g'), (33,'h'), (34,'i'), (35,'j')
          , (36,'k'), (37,'l'), (38,'m'), (39,'n'), (40,'o'), (41,'p')
          , (42,'q'), (43,'r'), (44,'s'), (45,'t'), (46,'u'), (47,'v')
          , (48,'w'), (49,'x'), (50,'y'), (51,'z'), (52,'0'), (53,'1')
          , (54,'2'), (55,'3'), (56,'4'), (57,'5'), (58,'6'), (59,'7')
          , (60,'8'), (61,'9'), (62,'+'), (63,'/') ]


-- Convert between 4 base64 (6bits ea) integers and 1 ordinary integer (32 bits)
-- clearly the upmost/leftmost 8 bits of the answer are 0.
-- Hack Alert: In the last entry of the answer, the upper 8 bits encode 
-- the integer number of 6bit groups encoded in that integer, ie 1, 2, 3.
-- 0 represents a 4 :(
int4_char3 :: [Int] -> [Char]
int4_char3 (a:b:c:d:t) = 
    let n = (a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6 .|. d)
    in (chr (n `shiftR` 16 .&. 0xff))
     : (chr (n `shiftR` 8 .&. 0xff))
     : (chr (n .&. 0xff)) : int4_char3 t

int4_char3 [a,b,c] =
    let n = (a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6)
    in [ (chr (n `shiftR` 16 .&. 0xff))
       , (chr (n `shiftR` 8 .&. 0xff)) ]

int4_char3 [a,b] = 
    let n = (a `shiftL` 18 .|. b `shiftL` 12)
    in [ (chr (n `shiftR` 16 .&. 0xff)) ]

int4_char3 [] = []     




-- Convert triplets of characters to
-- 4 base64 integers.  The last entries
-- in the list may not produce 4 integers,
-- a trailing 2 character group gives 3 integers,
-- while a trailing single character gives 2 integers.
char3_int4 :: [Char] -> [Int]
char3_int4 (a:b:c:t) 
    = let n = (ord a `shiftL` 16 .|. ord b `shiftL` 8 .|. ord c)
      in (n `shiftR` 18 .&. 0x3f) : (n `shiftR` 12 .&. 0x3f) : (n `shiftR` 6  .&. 0x3f) : (n .&. 0x3f) : char3_int4 t

char3_int4 [a,b]
    = let n = (ord a `shiftL` 16 .|. ord b `shiftL` 8)
      in [ (n `shiftR` 18 .&. 0x3f)
         , (n `shiftR` 12 .&. 0x3f)
         , (n `shiftR` 6  .&. 0x3f) ]
    
char3_int4 [a]
    = let n = (ord a `shiftL` 16)
      in [(n `shiftR` 18 .&. 0x3f),(n `shiftR` 12 .&. 0x3f)]

char3_int4 [] = []


-- Retrieve base64 char, given an array index integer in the range [0..63]
enc1 :: Int -> Char
enc1 ch = encodeArray!ch


-- | Cut up a string into 72 char lines, each line terminated by CRLF.

chop72 :: String -> String
chop72 str =  let (bgn,end) = splitAt 70 str
              in if null end then bgn else "\r\n" ++ chop72 end


-- Pads a base64 code to a multiple of 4 characters, using the special
-- '=' character.
quadruplets (a:b:c:d:t) = a:b:c:d:quadruplets t
quadruplets [a,b,c]     = [a,b,c,'=']      -- 16bit tail unit
quadruplets [a,b]       = [a,b,'=','=']    -- 8bit tail unit
quadruplets []          = []               -- 24bit tail unit


enc :: [Int] -> [Char]
enc = quadruplets . map enc1


dcd [] = []
dcd (h:t)
    | h <= 'Z' && h >= 'A'  =  ord h - ord 'A'      : dcd t
    | h >= '0' && h <= '9'  =  ord h - ord '0' + 52 : dcd t
    | h >= 'a' && h <= 'z'  =  ord h - ord 'a' + 26 : dcd t
    | h == '+'  = 62 : dcd t
    | h == '/'  = 63 : dcd t
    | h == '='  = []  -- terminate data stream
    | otherwise = dcd t


-- Principal encoding and decoding functions.

encode :: [Octet] -> String
encode = enc . char3_int4 . (map (chr .fromIntegral))

{-
prop_base64 os =
   os == (f . g . h) os
      where types = (os :: [Word8])
            f = map (fromIntegral. ord)
            g = decode . encode
            h = map (chr . fromIntegral)
-}

decode :: String -> [Octet]
decode = (map (fromIntegral . ord)) . int4_char3 . dcd