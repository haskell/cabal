-- | Minimal JSON / RFC 7159 support
--
-- The API is heavily inspired by @aeson@'s API but puts emphasis on
-- simplicity rather than performance. The 'ToJSON' instances are
-- intended to have an encoding compatible with @aeson@'s encoding.
--
-- Currently, only encoding to JSON is implemented.
--
-- In future we may want to encode via ByteString builders once
-- we can depend on recent enough @byteststring@ versions.
--
module Distribution.Client.Utils.Json
    ( Value(..)
    , object, (.=)
    , encodeToString
    , ToJSON(toJSON)
    )
    where

import Data.Char
import Data.Int
import Data.String
import Data.Word

data Value = Object [(String,Value)]
           | Array  [Value]
           | String  String
           | Number !Double
           | Bool   !Bool
           | Null
           deriving (Eq, Read, Show)

infixr 8 .=

(.=) :: ToJSON v => String -> v -> (String,Value)
k .= v  = (k, toJSON v)

object :: [(String,Value)] -> Value
object = Object

instance IsString Value where
  fromString = String

class ToJSON a where
  toJSON :: a -> Value

instance ToJSON () where
  toJSON () = Array []

instance ToJSON Value where
  toJSON = id

instance ToJSON Bool where
  toJSON = Bool

instance ToJSON a => ToJSON [a] where
  toJSON = Array . map toJSON

instance ToJSON a => ToJSON (Maybe a) where
  toJSON Nothing  = Null
  toJSON (Just a) = toJSON a

instance (ToJSON a,ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = Array [toJSON a, toJSON b]

instance (ToJSON a,ToJSON b,ToJSON c) => ToJSON (a,b,c) where
  toJSON (a,b,c) = Array [toJSON a, toJSON b, toJSON c]

instance (ToJSON a,ToJSON b,ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
  toJSON (a,b,c,d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]

instance ToJSON Float where
  toJSON = Number . realToFrac

instance ToJSON Double where
  toJSON = Number

instance ToJSON Int    where  toJSON = Number . realToFrac
instance ToJSON Int8   where  toJSON = Number . realToFrac
instance ToJSON Int16  where  toJSON = Number . realToFrac
instance ToJSON Int32  where  toJSON = Number . realToFrac

instance ToJSON Word   where  toJSON = Number . realToFrac
instance ToJSON Word8  where  toJSON = Number . realToFrac
instance ToJSON Word16 where  toJSON = Number . realToFrac
instance ToJSON Word32 where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Int64  where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Word64 where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Integer where toJSON = Number . fromInteger

encodeToString :: ToJSON a => a -> String
encodeToString jv = encodeToShowS (toJSON jv) []

encodeToShowS :: Value -> ShowS
encodeToShowS jv = case jv of
  Bool b   -> showString (if b then "true" else "false")
  Null     -> showString "null"
  Number n
    | isNaN n || isInfinite n    -> encodeToShowS Null
    | Just i <- doubleToInt64 n -> shows i
    | otherwise                 -> shows n
  Array a -> encodeArray a
  String s -> encodeString s
  Object o -> encodeObject o

encodeArray :: [Value] -> ShowS
encodeArray [] = showString "[]"
encodeArray jvs = ('[':) . go jvs . (']':)
  where
    go []     = id
    go [x]    = encodeToShowS x
    go (x:xs) = encodeToShowS x . (',':) . go xs

encodeObject :: [(String,Value)] -> ShowS
encodeObject [] = showString "{}"
encodeObject jvs = ('{':) . go jvs . ('}':)
  where
    go []          = id
    go [(l,x)]     = encodeString l . (':':) . encodeToShowS x
    go ((l,x):lxs) = encodeString l . (':':) . encodeToShowS x . (',':) . go lxs

encodeString :: String -> ShowS
encodeString s = ('"':) . showString (escape0 s) . ('"':)
  where
    escape0 s
      | not (any needsEscape s) = s
      | otherwise               = escape s

    escape [] = []
    escape (x:xs) = case x of
      '\\' -> '\\':'\\':escape xs
      '"'  -> '\\':'"':escape xs
      '\b' -> '\\':'b':escape xs
      '\f' -> '\\':'f':escape xs
      '\n' -> '\\':'n':escape xs
      '\r' -> '\\':'r':escape xs
      '\t' -> '\\':'t':escape xs
      c | ord c < 0x10 -> '\\':'u':'0':'0':'0':intToDigit (ord c):escape xs
        | ord c < 0x20 -> '\\':'u':'0':'0':'1':intToDigit (ord c - 0x10):escape xs
        | otherwise    -> c : escape xs

    -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    needsEscape c = ord c < 0x20 || c `elem` ['\\','"']

doubleToInt64 :: Double -> Maybe Int64
doubleToInt64 x
  | fromInteger x' == x
  , x' <= toInteger (maxBound :: Int64)
  , x' >= toInteger (minBound :: Int64)
    = Just (fromIntegral x')
  | otherwise = Nothing
  where
    x' = round x
