{-# LANGUAGE OverloadedStrings #-}

module Hackage.Cabal.Patches
  ( Package (..)
  , Version (..)
  , Revision (..)
  , Patch (..)

  , patches
  ) where

import           Data.Bits
import           Data.Char
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.ByteString.Unsafe
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Word
import           GHC.Fingerprint
import           Foreign.Ptr



showsHexFingerprint :: Fingerprint -> ShowS
showsHexFingerprint (Fingerprint l r) =
  showString "Fingerprint 0x" . (hex16 16 l <>) . showString " 0x" . (hex16 16 r <>)
  where
    offset :: Int -> Int
    offset i
      | i < 10    = 48
      | otherwise = 55

    hex16 :: Int -> Word64 -> String
    hex16 c n
      | c <= 0    = []
      | otherwise =
          let n' = n `unsafeShiftL` 4
              i  = fromIntegral (n `unsafeShiftR` 60)

          in chr (i + offset i) : hex16 (c - 1) n'

checkMD5 :: BSC.ByteString -> IO Fingerprint
checkMD5 bs = unsafeUseAsCStringLen bs $ \(ptr, len) ->
                fingerprintData (castPtr ptr) len

_fingerprint :: FilePath -> (BSLC.ByteString -> BSLC.ByteString) -> IO ()
_fingerprint path patch = do
  file <- BSC.readFile path
  fingerprint <- checkMD5 file

  putStrLn $ showsHexFingerprint fingerprint ""

  let file' = BSLC.toStrict . patch $ BSLC.fromStrict file
  fingerprint' <- checkMD5 file'

  putStrLn $ showsHexFingerprint fingerprint' ""



newtype Package = Package BSC.ByteString
                  deriving Show

newtype Version = Version (NonEmpty Int)
                  deriving Show

newtype Revision = Revision Int
                   deriving Show

data Patch = Patch
               Fingerprint -- ^ Original file MD5 hash
               Fingerprint -- ^ Modified file MD5 hash
               (BSLC.ByteString -> BSLC.ByteString)

instance Show Patch where
  showsPrec d (Patch l r _) =
    showParen (d > 10) $ showString "Patch "
                       . showsPrec 11 l . showChar ' '
                       . showsPrec 11 r . showString " _"



patches :: [(Package, [(Version, [(Revision, Patch)])])]
patches =
  [

  -- single dot in "other-modules" misinterpreted as an empty line

    (,) (Package "unicode-transforms")
      [ (,) (Version (0 :| [3,3]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xDD76C65DFDDAAA73 0x8F336B568E127831)
                (Fingerprint 0x98C5ED065CA1964A 0xBFBAFA37F2CFBFAB)
                punicode_transformsv0_3_3
          ]
      ]

  -- fields with spaces in names

  , (,) (Package "DSTM")
      [ (,) (Version (0 :| [1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x91C0BABCAC04DD3A 0xE3EE2BB1FA0BD64B)
                (Fingerprint 0xBC35F486FEF9CF2B 0xF867D73E12DF13BE)
                pDSTMv0_1
          ]

      , (,) (Version (0 :| [1,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xF04481FC8441AB51 0x855F1AA0F8550458)
                (Fingerprint 0xEF67AC6FB76CA2FA 0xF4AA20697ED66889)
                pDSTMv0_1_1
          ]

      , (,) (Version (0 :| [1,2]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x6006291A8F214ECE 0x7D9AB5DCB637A5C3)
                (Fingerprint 0xEC220DCC8A00F27C 0xA5BEA530FD6C1910)
                pDSTMv0_1_2
          ]
      ]

  , (,) (Package "control-monad-exception-mtl")
      [ (,) (Version (0 :| [10,3]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xFD9CF2DDD63BCFD4 0x381D89165A98FCA3)
                (Fingerprint 0x9DA6D0E329F2E1CF 0x071A20916FFEF82A)
                pcontrol_monad_exception_mtlv0_10_3
          ]
      ]

  -- '\DEL' character

  , (,) (Package "vacuum-opengl")
      [ (,) (Version (0 :| [0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x5287232FFBD7B691 0xEAFF62B1B5455C2E)
                (Fingerprint 0xC2C56E4F7E19A973 0xC29FE1BAF78DD450)
                pvacuum_openglv0_0
          ]

      , (,) (Version (0 :| [0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x95C127F3D330654F 0x122C7E1BE7946D5C)
                (Fingerprint 0x15BAA3782DE374A0 0xBD5E90117A02DDF9)
                pvacuum_openglv0_0_1
          ]

      ]

  -- {- comments -}

  , (,) (Package "ixset")
      [ (,) (Version (1 :| [0,4]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xA4F3E2029EA6CBE9 0x39999AFD1B2F28AF)
                (Fingerprint 0x4F89E823849C0607 0xF27FDA365EC0835D)
                pixsetv1_0_4r0
          ]
      ]

  -- : after section

  , (,) (Package "ds-kanren")
      [ (,) (Version (0 :| [2,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x26E9D54B4B97A36B 0x864E315EB592755E)
                (Fingerprint 0x886CF947F5364109 0xB1CDBFB920AD3589)
                pds_kanrenv0_2_0_0
          ]

      , (,) (Version (0 :| [2,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x7EB532C6DADD44A1 0x1DEA7B7C0D9CB674)
                (Fingerprint 0x19A55F8211A75AB0 0x3FCEA17BED62D029)
                pds_kanrenv0_2_0_1
          ]

      ]

  , (,) (Package "metric")
      [ (,) (Version (0 :| [1,4]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x555941F32E58FE2A 0x2A8F78C4D42CD502)
                (Fingerprint 0x7EA1E4EB98FD4E64 0xD8E7DD349B7A8884)
                pmetricv0_1_4
          ]

      , (,) (Version (0 :| [2,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x4063E6F1F50FFE2E 0x6D11E73784D96987)
                (Fingerprint 0x4D3F31AEA567A1EF 0x0C1E383D1EC6C7AF)
                pmetricv0_2_0
          ]
      ]

  , (,) (Package "phasechange")
      [ (,) (Version (0 :| [1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x925CBAC26035876E 0x0368389BFD517347)
                (Fingerprint 0x47BF0259F1C240E8 0x6492D76BB19EC369)
                pphasechangev0_1
          ]
      ]

  , (,) (Package "smartword")
      [ (,) (Version (0 :| [0,0,5]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x6C4BC2A2BB5DAC5F 0x95FB69A1B1BC3C86)
                (Fingerprint 0x171D867EE25DD359 0xE0FDF038404B17BD)
                psmartwordv0_0_0_5
          ]
      ]

  , (,) (Package "shelltestrunner")
      [ (,) (Version (1 :| [3]))
          [ (,) (Revision 0) $
              Patch
               (Fingerprint 0x3D1B70D2AA75C16D 0xD59D97C209D5681A)
               (Fingerprint 0x8DD0C7E21870D4C7 0x4073889A7AE8794D)
               pshelltestrunnerv1_3
          ]
      ]

  -- &&!

  , (,) (Package "hblas")
      [ (,) (Version (0 :| [2,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x76EF2EDEBB7C9E61 0xFE2DD056ED3AFC19)
                (Fingerprint 0x9668562A268F8E05 0xDE695A48CB9F33DF)
                phblasv0_2_0_0
          ]

      , (,) (Version (0 :| [3,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x49097DB512A0C9F3 0x9686F520DD3723F8)
                (Fingerprint 0x29F3B0E8185D4255 0xB9E811142A0B803C)
                phblasv0_3_0_0
          ]

      , (,) (Version (0 :| [3,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x00C0A3319837991D 0x4CB220094064DDB1)
                (Fingerprint 0x1847EFF2EC949169 0x77727A7B02EB420D)
                phblasv0_3_0_1
          ]

      , (,) (Version (0 :| [3,1,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x5E9BBD387B54CA5C 0xD40963973A5205E9)
                (Fingerprint 0xDAAEA83295B653C0 0xD7B23C92C6595D9F)
                phblasv0_3_1_0
          ]

      , (,) (Version (0 :| [3,1,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x73533ACF82684D48 0x02CB399C5C4E09B7)
                (Fingerprint 0x8EB5E8B5B2D1BB18 0x3ABAE5F0FB807316)
                phblasv0_3_1_1
          ]

      , (,) (Version (0 :| [3,2,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x614C08B4E3171EC3 0xA0DEA58890933104)
                (Fingerprint 0x5577FFF442D39D1D 0x591782F1C222DC27)
                phblasv0_3_2_1r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0x1CD274CDDACAB6D9 0xE694E8A64C1E6B2B)
                (Fingerprint 0x939611048906E89A 0x495AFA5A8E871738)
                phblasv0_3_2_1r1

          , (,) (Revision 2) $
              Patch
                (Fingerprint 0xA473BAFAF8ED08AB 0xA5EDF500BB60EA2E)
                (Fingerprint 0xBE2A5E8E07D28EE9 0xB9BBD6526BFE8B96)
                phblasv0_3_2_1r2
          ]

      , (,) (Version (0 :| [4,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xBDFDCC06038F74E4 0x004600BCCD095F02)
                (Fingerprint 0x9B48A51E81B0BB48 0x7411388255F2A566)
                phblasv0_4_0_0
          ]
      ]

  -- flag used, but not defined

  , (,) (Package "brainheck")
      [ (,) (Version (0 :| [1,0,2]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x5FE7D5B2A340EF48 0xD5BD98F4F4E2778D)
                (Fingerprint 0xE5B26DA09ABA5F20 0xE1A8771590BE95AD)
                pbrainheckv0_1_0_2r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0xC6BE5EA37ABFE415 0x8B359EB49B205421)
                (Fingerprint 0x6E75742134B1E8E8 0xB84BD42D0C9C19BA)
                pbrainheckv0_1_0_2r1

          , (,) (Revision 2) $
              Patch
                (Fingerprint 0x34DC909D52E06238 0x95D33074FE2B7E6D)
                (Fingerprint 0x0FA4BACB6375393C 0xA7C282376A3FC705)
                pbrainheckv0_1_0_2r1

          , (,) (Revision 3) $
              Patch
                (Fingerprint 0xC058A86CB59A1986 0xAD313DF7C594203B)
                (Fingerprint 0x410D4A84DF32C967 0x6F354C7201CBA589)
                pbrainheckv0_1_0_2r3
          ]
      ]

  , (,) (Package "wordchoice")
      [ (,) (Version (0 :| [1,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xE10A7D801867688B 0xD86C50D43C82657B)
                (Fingerprint 0xD1D773B8881CBCA7 0x250037DC8B71CC6F)
                pwordchoicev0_1_0_1r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0xE646AC35EE3DC624 0x383EDC5EC98842A1)
                (Fingerprint 0x31A5692403466649 0x9F57E6A8D36F3881)
                pwordchoicev0_1_0_1r1
          ]

      , (,) (Version (0 :| [1,0,2]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x815BEA1C3E893F6E 0x1244C43323724B5F)
                (Fingerprint 0x3485B3B51896D6B5 0xAFBABC8808D4879B)
                pwordchoicev0_1_0_2r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0x235869B28A5C92B9 0x1C95A3FDDD0D5298)
                (Fingerprint 0x702946A4E678DA64 0x0888F313490A70FB)
                pwordchoicev0_1_0_2r1
          ]

      , (,) (Version (0 :| [1,0,3]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x1FACA5302AF8DFDF 0xACE2200B88C6A658)
                (Fingerprint 0xF06DE6E5F6D18500 0xEE527055DEAC380D)
                pwordchoicev0_1_0_2r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0xB3226381A5C8D219 0x99BEA513FBF8FE1E)
                (Fingerprint 0xB7D3E761206EB8C8 0x40B6850797D82996)
                pwordchoicev0_1_0_2r1
          ]
      ]

  , (,) (Package "hw-prim-bits")
      [ (,) (Version (0 :| [1,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xABE6ACB775B44A54 0xF1AB834894A2EB1F)
                (Fingerprint 0x2FE8FFC90F9E8542 0xC3B7851BC6FC5278)
                phw_prim_bitsv0_1_0_0
          ]

      , (,) (Version (0 :| [1,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x5F58FE271203A97C 0xC9F8445A7DCF4BFC)
                (Fingerprint 0xAD357E86BFE9F6C7 0xCC28494F12A6D59A)
                phw_prim_bitsv0_1_0_1
          ]
      ]

  -- leading zeros in version digits

  , (,) (Package "Sit")
      [ (,) (Version (0 :| [2017,2,26]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x7562BD095EBB7296 0x2CCE12E1BEA2BA25)
                (Fingerprint 0xC8D18D5E8A7BF819 0xF30F7C37B959F9AB)
                pSitv0_2017_2_26r0
          ]

      , (,) (Version (0 :| [2017,5,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x141FE653145FFA81 0xA2F451615E61A39C)
                (Fingerprint 0xE77A6C0635F39A45 0x3B0843C3A4CAFC96)
                pSitv0_2017_5_1r0
          ]

      , (,) (Version (0 :| [2017,5,2]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x042009F71F9E4B19 0xF06653CF9B9D9C00)
                (Fingerprint 0x08CF4E3110EA71A0 0x185D1467F9317E4E)
                pSitv0_2017_5_2r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0x335178C5ED248D35 0x36B8497E6845E530)
                (Fingerprint 0xC711FE80329F135A 0xE368304D783836D8)
                pSitv0_2017_5_2r1
          ]
      ]

  , (,) (Package "MiniAgda")
      [ (,) (Version (0 :| [2017,2,18]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xEE3DE4D3EBC68B27 0x3BADEA07A42CAC39)
                (Fingerprint 0xAC1D98C61052D989 0x6B117CD15B4AFD7E)
                pMiniAgdav0_2017_2_18r0
          ]
      ]

  , (,) (Package "fast-downward")
      [ (,) (Version (0 :| [1,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x9C359D852F0B0D03 0x5F4FB1C2EB822810)
                (Fingerprint 0xA8C058E0E3813C6A 0x493F470A9D17364D)
                pfast_downwardv0_1_0_0r0

          , (,) (Revision 1) $
              Patch
                (Fingerprint 0x7FE67F96CEAEC267 0x0C6628B7437A2C1C)
                (Fingerprint 0x8B0ECB1CC737517B 0x195F8E966829AB93)
                pfast_downwardv0_1_0_0r1
          ]

      , (,) (Version (0 :| [1,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x89637669C71F6278 0x52FEC4B6E40D8FA6)
                (Fingerprint 0xA6A319AB7ECC3AE6 0x737BC9BCAA4B63DF)
                pfast_downwardv0_1_0_1
          ]

      , (,) (Version (0 :| [1,1,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xB02C7AF973EAA227 0x1A661FE3FEFE65FF)
                (Fingerprint 0xD62D9218636B49C9 0x1FB4EE2DD01F92F0)
                pfast_downwardv0_1_1_0
          ]
      ]

  -- version length exceeds nine digits

  , (,) (Package "SGplus")
      [ (,) (Version (1 :| [1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xF621AF4C72ED375D 0x9F8215632E481FBA)
                (Fingerprint 0x84BF56197F02764C 0xDD6E4BA447FCE21C)
                pSGplusv1_1
          ]
      ]

  , (,) (Package "control-dotdotdot")
      [ (,) (Version (0 :| [1,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x1503B8E06D892BF6 0x6BA307216B485FB5)
                (Fingerprint 0xC36DA4A5D743B1CF 0xFF927C718CA487CB)
                pcontrol_dotdotdotv0_1_0_1
          ]
      ]

  , (,) (Package "data-foldapp")
      [ (,) (Version (0 :| [1,1,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x3E9B1F90498A69F3 0xA262D8AB2B943DAC)
                (Fingerprint 0xA40B00F5C25DF6DD 0x4408398749AFBBC4)
                pdata_foldappv0_1_1_0
          ]
      ]

  , (,) (Package "data-list-zigzag")
      [ (,) (Version (0 :| [1,1,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xAD2314015488BB4B 0xFA8C1A9B73E5A5E1)
                (Fingerprint 0xE1EDF596316CF59E 0xDA9F23F125C46345)
                pdata_list_zigzagv0_1_1_1
          ]
      ]

  -- Not UTF-8 encoding

  , (,) (Package "nat")
      [ (,) (Version (0 :| [1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x7FFCF20BA0E0B894 0xB5985D5FA7FC4227)
                (Fingerprint 0xF26E135671D11FBE 0xB77B8D1D49D20634)
                pnatv0_1
          ]
      ]

  -- cabal-version: 2

  , (,) (Package "streaming-bracketed")
      [ (,) (Version (0 :| [1,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xCB9677E55A5C6BF7 0x13CF7D7DD4CA3B55)
                (Fingerprint 0x80223BA2D7D55031 0x5B346DC4F94858D6)
                pstreaming_bracketedv0_1_0_0
          ]

      , (,) (Version (0 :| [1,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x654A5453500AA567 0x8CBE884D34B36822)
                (Fingerprint 0x12BBFC2295577719 0x31DF53F9B9DFA62D)
                pstreaming_bracketedv0_1_0_1
          ]
      ]

  , (,) (Package "zsyntax")
      [ (,) (Version (0 :| [2,0,0]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xF7321CE9FD54C553 0x29B4F2B864C5F9D7)
                (Fingerprint 0x2FD27FDF9480E134 0xAD16C19E721A1B5A)
                pzsyntaxv0_2_0_0
          ]
      ]

  -- empty hs-source-dirs field

  , (,) (Package "wai-middleware-hmac-client")
      [ (,) (Version (0 :| [1,0,1]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x2B323327DD3EF4BB 0xA651E107CCAC410B)
                (Fingerprint 0x5FFC1B28D862BCA4 0x5BE3E6C65561AA50)
                pwai_middleware_hmac_clientv0_1_0_1
          ]

      , (,) (Version (0 :| [1,0,2]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0xAE662EDE54B9AFAA 0xF3B913407DB727DD)
                (Fingerprint 0xDA83E7AD0A344FEA 0x9280370898F79C0C)
                pwai_middleware_hmac_clientv0_1_0_2
          ]
      ]

  -- absolute license path

  , (,) (Package "reheat")
      [ (,) (Version (0 :| [1,4]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x7F0E8419E7B61A45 0xCD922EE0B34FE702)
                (Fingerprint 0x6AADE64A5BB9C363 0xD580A03AB7034580)
                preheatv0_1_4
          ]

      , (,) (Version (0 :| [1,5]))
          [ (,) (Revision 0) $
              Patch
                (Fingerprint 0x296AAFEF8A70E42D 0xA2C30F7C82EE1D19)
                (Fingerprint 0xA75852D61D776BF5 0xC289B81E96F9E9C2)
                preheatv0_1_5
          ]
      ]
    ]



-- line 193, column 1: "  other-modules:\n      .\n" -> ""
punicode_transformsv0_3_3 :: BSLC.ByteString -> BSLC.ByteString
punicode_transformsv0_3_3 bsl =
  let (before, after) = BSLC.splitAt 5652 bsl
  in before <> BSLC.drop 25 after



-- line 60, column 3: "Other modules:" -> "Other-modules:"
pDSTMv0_1 :: BSLC.ByteString -> BSLC.ByteString
pDSTMv0_1 bsl =
  let (before, after) = BSLC.splitAt 1999 bsl
  in before <> "-- " <> BSLC.drop 14 after

-- line 60, column 3: "Other modules:" -> "-- "
pDSTMv0_1_1 :: BSLC.ByteString -> BSLC.ByteString
pDSTMv0_1_1 bsl =
  let (before, after) = BSLC.splitAt 2001 bsl
  in before <> "-- " <> BSLC.drop 14 after

-- line 69, column 3: "Other modules:" -> "Other-modules:"
pDSTMv0_1_2 :: BSLC.ByteString -> BSLC.ByteString
pDSTMv0_1_2 bsl =
  let (before, after) = BSLC.splitAt 2225 bsl
  in before <> "-- " <> BSLC.drop 14 after



-- line 26, column 2: "default- extensions:" -> " default-extensions:"
pcontrol_monad_exception_mtlv0_10_3 :: BSLC.ByteString -> BSLC.ByteString
pcontrol_monad_exception_mtlv0_10_3 bsl =
  let (before, after) = BSLC.splitAt 704 bsl
  in before <> " default-" <> BSLC.drop 9 after

-- line 26, column 2: " default- extensions:" -> "unknown-section"
--
-- Fingerprint 0x9E24163ECAD97FFB 0x3BB8750A5516E854
_pcontrol_monad_exception_mtlv0_10_3' :: BSLC.ByteString -> BSLC.ByteString
_pcontrol_monad_exception_mtlv0_10_3' bsl =
  let (before, after) = BSLC.splitAt 703 bsl
  in before <> "unknown-section" <> BSLC.drop 21 after



-- line 4, column 22: "\DEL" -> ""
pvacuum_openglv0_0 :: BSLC.ByteString -> BSLC.ByteString
pvacuum_openglv0_0 bsl =
  let (before, after) = BSLC.splitAt 176 bsl
  in before <> BSLC.drop 1 after


-- line 4, column 22: "\DEL" -> ""
pvacuum_openglv0_0_1 :: BSLC.ByteString -> BSLC.ByteString
pvacuum_openglv0_0_1 bsl =
  let (before, after) = BSLC.splitAt 178 bsl
  in before <> BSLC.drop 1 after



-- line 49, column 1: "{-...eof" -> ""
pixsetv1_0_4r0 :: BSLC.ByteString -> BSLC.ByteString
pixsetv1_0_4r0 = BSLC.take 1268



-- line 80, column 1: "Test-Suite test-unify:" -> "Test-Suite \"test-unify:\""
-- line 91, column 1: "Test-Suite test-list-ops:" -> "Test-Suite \"test-list-ops:\""
pds_kanrenv0_2_0_0 :: BSLC.ByteString -> BSLC.ByteString
pds_kanrenv0_2_0_0 bsl =
  let (before, middle_) = BSLC.splitAt 2509 bsl
      (middle, after)   = BSLC.splitAt 349 middle_
  in before <> "\"test-unify:\""    <> BSLC.drop 11 middle
            <> "\"test-list-ops:\"" <> BSLC.drop 14 after

-- line 27, column 1: "Test-Suite test-unify:" -> "Test-Suite \"test-unify:\""
-- line 38, column 1: "Test-Suite test-list-ops:" -> "Test-Suite \"test-list-ops:\""
pds_kanrenv0_2_0_1 :: BSLC.ByteString -> BSLC.ByteString
pds_kanrenv0_2_0_1 bsl =
  let (before, middle_) = BSLC.splitAt 839 bsl
      (middle, after)   = BSLC.splitAt 349 middle_
  in before <> "\"test-unify:\""    <> BSLC.drop 11 middle
            <> "\"test-list-ops:\"" <> BSLC.drop 14 after



-- line 28, column 1: "Test-Suite metric-tests:" -> "Test-Suite \"metric-tests:\""
pmetricv0_1_4 :: BSLC.ByteString -> BSLC.ByteString
pmetricv0_1_4 bsl =
  let (before, after) = BSLC.splitAt 927 bsl
  in before <> "\"metric-tests:\"" <> BSLC.drop 13 after

-- line 28, column 1: "Test-Suite metric-tests:" -> "Test-Suite \"metric-tests:\""
pmetricv0_2_0 :: BSLC.ByteString -> BSLC.ByteString
pmetricv0_2_0 = pmetricv0_1_4



-- line 49, column 5: "impl(ghc >= 7.4):" -> "erroneous-section"
-- line 54, column 5: "impl(ghc >= 7.6):" -> "erroneous-section"
pphasechangev0_1 :: BSLC.ByteString -> BSLC.ByteString
pphasechangev0_1 bsl =
  let (before, middle_) = BSLC.splitAt 2104 bsl
      (middle, after)   = BSLC.splitAt 102 middle_
  in before <> "erroneous-section" <> BSLC.drop 17 middle
            <> "erroneous-section" <> BSLC.drop 17 after



-- line 3438, column 3: "build depends:" -> "--"
psmartwordv0_0_0_5 :: BSLC.ByteString -> BSLC.ByteString
psmartwordv0_0_0_5 bsl =
  let (before, after) = BSLC.splitAt 65767 bsl
  in before <> "--" <> BSLC.drop 14 after



-- line 28, column 3: "other modules:" -> "--"
pshelltestrunnerv1_3 :: BSLC.ByteString -> BSLC.ByteString
pshelltestrunnerv1_3 bsl =
  let (before, after) = BSLC.splitAt 956 bsl
  in before <> "--" <> BSLC.drop 14 after



-- line 97, column 13: "&&!" -> "&& !"
phblasv0_2_0_0 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_2_0_0 bsl =
  let (before, after) = BSLC.splitAt 2714 bsl
  in before <> " " <> after

-- line 98, column 13: "&&!" -> "&& !"
phblasv0_3_0_0 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_0_0 bsl =
  let (before, after) = BSLC.splitAt 2749 bsl
  in before <> " " <> after

-- line 125, column 13: "&&!" -> "&& !"
phblasv0_3_0_1 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_0_1 bsl =
  let (before, after) = BSLC.splitAt 4066 bsl
  in before <> " " <> after

-- line 132, column 13: "&&!" -> "&& !"
phblasv0_3_1_0 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_1_0 bsl =
  let (before, after) = BSLC.splitAt 4167 bsl
  in before <> " " <> after

-- line 132, column 13: "&&!" -> "&& !"
phblasv0_3_1_1 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_1_1 = phblasv0_3_1_0

-- line 132, column 13: "&&!" -> "&& !"
phblasv0_3_2_1r0 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_2_1r0 bsl =
  let (before, after) = BSLC.splitAt 4167 bsl
  in before <> " " <> after

-- line 133, column 13: "&&!" -> "&& !"
phblasv0_3_2_1r1 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_2_1r1 bsl =
  let (before, after) = BSLC.splitAt 4313 bsl
  in before <> " " <> after

-- line 133, column 13: "&&!" -> "&& !"
phblasv0_3_2_1r2 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_3_2_1r2 bsl =
  let (before, after) = BSLC.splitAt 4314 bsl
  in before <> " " <> after

-- line 148, column 13: "&&!" -> "&& !"
phblasv0_4_0_0 :: BSLC.ByteString -> BSLC.ByteString
phblasv0_4_0_0 bsl =
  let (before, after) = BSLC.splitAt 4693 bsl
  in before <> " " <> after



-- line 55, column 6: "flag(llvm-fast)" -> "False"
pbrainheckv0_1_0_2r0 :: BSLC.ByteString -> BSLC.ByteString
pbrainheckv0_1_0_2r0 bsl =
  let (before, after) = BSLC.splitAt 1786 bsl
  in before <> "False" <> BSLC.drop 15 after

-- line 56, column 6: "flag(llvm-fast)" -> "False"
pbrainheckv0_1_0_2r1 :: BSLC.ByteString -> BSLC.ByteString
pbrainheckv0_1_0_2r1 bsl =
  let (before, after) = BSLC.splitAt 1863 bsl
  in before <> "False" <> BSLC.drop 15 after

-- line 56, column 6: "flag(llvm-fast)" -> "False"
pbrainheckv0_1_0_2r3 :: BSLC.ByteString -> BSLC.ByteString
pbrainheckv0_1_0_2r3 bsl =
  let (before, after) = BSLC.splitAt 1874 bsl
  in before <> "False" <> BSLC.drop 15 after



-- line 62, column 6: "flag(llvm-fast)" -> "False"
pwordchoicev0_1_0_1r0 :: BSLC.ByteString -> BSLC.ByteString
pwordchoicev0_1_0_1r0 bsl =
  let (before, after) = BSLC.splitAt 2079 bsl
  in before <> "False" <> BSLC.drop 15 after

-- line 63, column 6: "flag(llvm-fast)" -> "False"
pwordchoicev0_1_0_1r1 :: BSLC.ByteString -> BSLC.ByteString
pwordchoicev0_1_0_1r1 bsl =
  let (before, after) = BSLC.splitAt 2161 bsl
  in before <> "False" <> BSLC.drop 15 after

-- line 63, column 6: "flag(llvm-fast)" -> "False"
pwordchoicev0_1_0_2r0 :: BSLC.ByteString -> BSLC.ByteString
pwordchoicev0_1_0_2r0 bsl =
  let (before, after) = BSLC.splitAt 2107 bsl
  in before <> "False" <> BSLC.drop 15 after

-- line 64, column 6: "flag(llvm-fast)" -> "False"
pwordchoicev0_1_0_2r1 :: BSLC.ByteString -> BSLC.ByteString
pwordchoicev0_1_0_2r1 bsl =
  let (before, after) = BSLC.splitAt 2190 bsl
  in before <> "False" <> BSLC.drop 15 after



-- line 46, column 8: "flag(sse42)" -> "False"
phw_prim_bitsv0_1_0_0 :: BSLC.ByteString -> BSLC.ByteString
phw_prim_bitsv0_1_0_0 bsl =
  let (before, after) = BSLC.splitAt 1421 bsl
  in before <> "False" <> BSLC.drop 11 after

-- line 46, column 6: "flag(sse42)" -> "False"
phw_prim_bitsv0_1_0_1 :: BSLC.ByteString -> BSLC.ByteString
phw_prim_bitsv0_1_0_1 bsl =
  let (before, after) = BSLC.splitAt 1458 bsl
  in before <> "False" <> BSLC.drop 11 after



-- line 2, column 18: "0.2017.02.26" -> "0.2017.2.26"
pSitv0_2017_2_26r0 :: BSLC.ByteString -> BSLC.ByteString
pSitv0_2017_2_26r0 bsl =
  let (before, after) = BSLC.splitAt 45 bsl
  in before <> BSLC.drop 1 after

-- line 2, column 18: "0.2017.05.01" -> "0.2017.5.1"
pSitv0_2017_5_1r0 :: BSLC.ByteString -> BSLC.ByteString
pSitv0_2017_5_1r0 bsl =
  let (before, after) = BSLC.splitAt 45 bsl
  in before <> "5." <> BSLC.drop 4 after

-- line 2, column 18: "0.2017.05.02" -> "0.2017.5.2"
pSitv0_2017_5_2r0 :: BSLC.ByteString -> BSLC.ByteString
pSitv0_2017_5_2r0 bsl =
  let (before, after) = BSLC.splitAt 45 bsl
  in before <> "5." <> BSLC.drop 4 after

-- line 2, column 18: "0.2017.5.02" -> "0.2017.5.2"
pSitv0_2017_5_2r1 :: BSLC.ByteString -> BSLC.ByteString
pSitv0_2017_5_2r1 bsl =
  let (before, after) = BSLC.splitAt 47 bsl
  in before <> BSLC.drop 1 after



-- line 2, column 18: "0.2017.02.18" -> "0.2017.2.18"
pMiniAgdav0_2017_2_18r0 :: BSLC.ByteString -> BSLC.ByteString
pMiniAgdav0_2017_2_18r0 bsl =
  let (before, after) = BSLC.splitAt 50 bsl
  in before <> BSLC.drop 1 after



-- line 56, column 5: "1.2.03.0" -> "1.2.3.0"
pfast_downwardv0_1_0_0r0 :: BSLC.ByteString -> BSLC.ByteString
pfast_downwardv0_1_0_0r0 bsl =
  let (before, after) = BSLC.splitAt 1486 bsl
  in before <> BSLC.drop 1 after

-- line 65, column 5: "1.2.03.0" -> "1.2.3.0"
pfast_downwardv0_1_0_0r1 :: BSLC.ByteString -> BSLC.ByteString
pfast_downwardv0_1_0_0r1 bsl =
  let (before, after) = BSLC.splitAt 1792 bsl
  in before <> BSLC.drop 1 after

-- line 66, column 5: "1.2.03.0" -> "1.2.3.0"
pfast_downwardv0_1_0_1 :: BSLC.ByteString -> BSLC.ByteString
pfast_downwardv0_1_0_1 bsl =
  let (before, after) = BSLC.splitAt 1708 bsl
  in before <> BSLC.drop 1 after

-- line 66, column 5: "1.2.03.0" -> "1.2.3.0"
pfast_downwardv0_1_1_0 :: BSLC.ByteString -> BSLC.ByteString
pfast_downwardv0_1_1_0 bsl =
  let (before, after) = BSLC.splitAt 1735 bsl
  in before <> BSLC.drop 1 after



-- line 24, column 38: "1000000000.0" -> "100000000.0"
pSGplusv1_1 :: BSLC.ByteString -> BSLC.ByteString
pSGplusv1_1 bsl =
  let (before, after) = BSLC.splitAt 1010 bsl
  in before <> BSLC.drop 1 after



-- line 41, column 39: "9223372036854775807" -> "5"
pcontrol_dotdotdotv0_1_0_1 :: BSLC.ByteString -> BSLC.ByteString
pcontrol_dotdotdotv0_1_0_1 bsl =
  let (before, after) = BSLC.splitAt 1363 bsl
  in before <> "5" <> BSLC.drop 19 after



-- line 37, column 38: "9223372036854775807" -> "999"
-- line 38, column 44: "9223372036854775807" -> "999"
pdata_foldappv0_1_1_0 :: BSLC.ByteString -> BSLC.ByteString
pdata_foldappv0_1_1_0 bsl =
  let (before, middle_) = BSLC.splitAt 1051 bsl
      (middle, after)   = BSLC.splitAt 64 middle_
  in before <> "999" <> BSLC.drop 19 middle
            <> "999" <> BSLC.drop 19 after



-- line 23, column 40: "9223372036854775807" -> "999"
pdata_list_zigzagv0_1_1_1 :: BSLC.ByteString -> BSLC.ByteString
pdata_list_zigzagv0_1_1_1 bsl =
  let (before, after) = BSLC.splitAt 845 bsl
  in before <> "999" <> BSLC.drop 19 after



-- line 10, column 36: "\xF6" (Latin-1) -> "ö"
pnatv0_1 :: BSLC.ByteString -> BSLC.ByteString
pnatv0_1 bsl =
  let (before, after) = BSLC.splitAt 615 bsl
  in before <> "\xC3\xB6" <> BSLC.drop 1 after



-- line 25, column 1: "cabal-version:       2" "cabal-version: 2.0"
pstreaming_bracketedv0_1_0_0 :: BSLC.ByteString -> BSLC.ByteString
pstreaming_bracketedv0_1_0_0 bsl =
  let (before, after) = BSLC.splitAt 1043 bsl
  in before <> "2.0" <> BSLC.drop 7 after

-- line 25, column 1: "cabal-version:       2" "cabal-version: 2.0"
pstreaming_bracketedv0_1_0_1 :: BSLC.ByteString -> BSLC.ByteString
pstreaming_bracketedv0_1_0_1 bsl =
  let (before, after) = BSLC.splitAt 1031 bsl
  in before <> "2.0" <> BSLC.drop 7 after



-- line 31, column 1: "cabal-version:  2" "cabal-version: 2.0"
pzsyntaxv0_2_0_0 :: BSLC.ByteString -> BSLC.ByteString
pzsyntaxv0_2_0_0 bsl =
  let (before, after) = BSLC.splitAt 1460 bsl
  in before <> "2.0" <> BSLC.drop 2 after



-- line 55, column 21: "\"\"" -> "."
pwai_middleware_hmac_clientv0_1_0_1 :: BSLC.ByteString -> BSLC.ByteString
pwai_middleware_hmac_clientv0_1_0_1 bsl =
  let (before, after) = BSLC.splitAt 1802 bsl
  in before <> "." <> BSLC.drop 2 after

-- line 55, column 21: "\"\"" -> "."
pwai_middleware_hmac_clientv0_1_0_2 :: BSLC.ByteString -> BSLC.ByteString
pwai_middleware_hmac_clientv0_1_0_2 bsl =
  let (before, after) = BSLC.splitAt 1798 bsl
  in before <> "." <> BSLC.drop 2 after



-- line 6, column 15: "/home/palo/dev/haskell-workspace/playground/reheat/gpl-3.0.txt" -> ""
preheatv0_1_4 :: BSLC.ByteString -> BSLC.ByteString
preheatv0_1_4 bsl =
  let (before, after) = BSLC.splitAt 95 bsl
  in before <> BSLC.drop 62 after

-- line 6, column 15: "/home/palo/dev/haskell-workspace/playground/reheat/gpl-3.0.txt" -> ""
preheatv0_1_5 :: BSLC.ByteString -> BSLC.ByteString
preheatv0_1_5 = preheatv0_1_4
