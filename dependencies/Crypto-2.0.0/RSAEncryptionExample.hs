module Main(main) where

import System.IO
import Data.Char
import Data.List
import System.Environment
import System.Console.GetOpt
import Data.Maybe
import Codec.ASN1
import Codec.Utils
import Data.Digest.SHA1
import Codec.Encryption.RSA.MGF
import qualified Codec.Encryption.RSA.EMEOAEP as E
import Codec.Encryption.RSA
import Codec.ASN1.TLV
import Codec.ASN1.X509
import Codec.ASN1.BER
import NewBinary.Binary

randomSeed :: [Octet]
randomSeed = hash [3]

ciphertext :: [Octet] -> [Octet] -> String -> [Octet]
ciphertext n d x =
   encrypt (n,d) $
   E.encode mgf hash [] randomSeed n $
   map (fromIntegral . ord) x

encryptWith certFile plainTextFile cipherTextFile = 
{-
certFile should contain an X.509 certificate.
-}
   do hcert   <- openFile certFile ReadMode
      hplain  <- openFile plainTextFile ReadMode
      ptext   <- hGetContents hplain
      bin <- openBinIO_ hcert
      (_,x) <- tlvIO bin
{-
Typecheck this is really a signed certificate.
-}
      sc <- tc signedCertificate x
{-
If it is then decode it and extract the bitstring containing the RSA
public key.
-}
      let (_ ::= c) = signedCertificate
          d  = (decode c (Just sc))::(Maybe SignedCertificate)
          d1 = certificate1 (fromJust d)
          d2 = subjectPublicKeyInfo2 d1
          d3 = subjectPublicKeyInfo1 d2
          (BitString e) = d3
          (_,x') = tlv e
{-
Typecheck this really is an RSA public key.
-}
      rpk <- tc rsaPublicKey x'
{-
If it is then decode it and extract the modulus and the public
exponent.
-}
      let (_ ::= r) = rsaPublicKey
          s  = (decode r (Just rpk))
          ct = ciphertext (toOctets 256 $ modulus1 $ fromJust s) 
                          (toOctets 256 $ publicExponent1 $ fromJust s)
                          ptext
{-
Write out the encrypted text.
-}
      ofh <- openFile cipherTextFile WriteMode
      hPutStr ofh (map (chr . fromIntegral) ct)
      hClose ofh

main = do pn <- getProgName
          args <- getArgs
          (fs,ss) <- opts pn args
          let sfs        = sort fs
              (Cert e)   = sfs!!0
              (Input i)  = sfs!!1
              (Output o) = sfs!!2
          encryptWith e i o

data Flag = Cert String | Input String | Output String 
   deriving (Show,Eq,Ord)

options = [
   Option ['e'] ["cert","certificate"]  (ReqArg Cert "CERT")
          "Certificate File",
   Option ['p'] ["plain","plaintext"]   (ReqArg Input "INPUT")
          "Plaintext File",
   Option ['c'] ["cipher","ciphertext"] (ReqArg Output "OUTPUT")
          "Ciphertext Fileoutput"
   ]
    
opts :: String -> [String] -> IO ([Flag], [String])
opts progName argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> 
         if length o == 3
            then return (o,n)
            else ioError (userError (usageInfo header options))
      (_,_,errs) -> 
         ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: " ++ progName ++ " [OPTION...] files..."