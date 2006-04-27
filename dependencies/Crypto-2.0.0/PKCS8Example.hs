module Main(main) where

import System.IO
import System.Environment
import Control.Monad.Error
import Data.Maybe
import Data.List (
   unfoldr,
   intersperse
   )
import Numeric (
   showHex
   )
import NewBinary.Binary
import Codec.Utils (
   toOctets
   )
import Codec.ASN1.BER
import Codec.ASN1
import Codec.ASN1.TLV
import Codec.ASN1.PKCS8
import Codec.ASN1.X509 (
   algorithm1, 
   parameters1
   )
import Text.PrettyPrint

pp pki rsapk =
   render (
      ppLabelString "Version" (show (version2 pki))
      $$
      ppLabelDoc "Private Key Algorithm" algid
      $$
      ppLabelDoc "Private Key" rs
   )
   where
      algid =
          ppLabelString "Algorithm" (show (algorithm1 al))
          $$
          ppLabelString "Parameters" (show (parameters1 al))
      al = privateKeyAlgorithm pki
      rs = ppLabelString "Version" (show (version1 rsapk))
           $$
           mod
           $$
           ppLabelDoc "Public Exponent" puE
           $$
           ppLabelDoc "Private Exponent" prE
           $$
           ppLabelDoc "Prime 1" p1
           $$
           ppLabelDoc "Prime 2" p2
           $$
           ppLabelDoc "Exponent 1" e1
           $$
           ppLabelDoc "Exponent 2" e2
           $$
           ppLabelDoc "Coefficient" co
      bar  = map (map sh) (split 16 (toOctets 256 (modulus rsapk)))
      sh x | x < 16    = showHex x "0"
           | otherwise = showHex x ""
      split :: Int -> [a] -> [[a]]
      split n xs = unfoldr (g n) xs
      g :: Int -> [a] -> Maybe ([a],[a])
      g n y
         | length y == 0 = Nothing
         | otherwise     = Just (splitAt n y)
      mods1 :: [[Doc]]
      mods1 = map (intersperse colon) (map (map text) bar)
      mods2 :: [Doc]
      mods2 = map hcat mods1
      mod = ppLabelDoc "Modulus" (vcat mods2)
      puE  = hexify (publicExponent rsapk)
      prE  = hexify (privateExponent rsapk)
      p1   = hexify (prime1 rsapk)
      p2   = hexify (prime2 rsapk)
      e1   = hexify (exponent1 rsapk)
      e2   = hexify (exponent2 rsapk)
      co   = hexify (coefficient rsapk)
      hexify :: Integral a => a -> Doc
      hexify n =
         let bar = map (map sh) (split 16 (toOctets 256 n))
             foo = map (intersperse colon) (map (map text) bar)
             baz = vcat (map hcat foo)
         in baz

ppLabelString :: String -> String -> Doc
ppLabelString l x =
   text l <> colon <> space <> (text x)

ppLabelDoc :: String -> Doc -> Doc
ppLabelDoc l d =
   text l <> colon
   $$
   nest 3 d

test fileName = 
   do h   <- openFile fileName ReadMode
      bin <- openBinIO_ h
      (l,x) <- tlvIO bin
      foo <- tc privateKeyInfo x
      let (_ ::= t) = privateKeyInfo
          pk = (decode t (Just foo))::(Maybe PrivateKeyInfo)
      let (OctetString xs) = privateKey1 $ fromJust pk
          (l',x') = tlv xs
      bar <- tc rsaPrivateKey x'
      let (_ ::= t') = rsaPrivateKey
          rsapk = (decode t' (Just bar))::(Maybe RSAPrivateKey)
      putStrLn (pp (fromJust pk) (fromJust rsapk))

main = 
   do progName <- getProgName
      args <- getArgs
      if length args /= 1
         then putStrLn ("Usage: " ++ progName ++ " <fileName>")
         else test (args!!0)
