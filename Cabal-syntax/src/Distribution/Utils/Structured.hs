{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
--
-- Copyright: (c) 2019 Oleg Grenrus
--
-- Structurally tag binary serialisation stream.
-- Useful when most 'Binary' instances are 'Generic' derived.
--
-- Say you have a data type
--
-- @
-- data Record = Record
--   { _recordFields  :: HM.HashMap Text (Integer, ByteString)
--   , _recordEnabled :: Bool
--   }
--   deriving (Eq, Show, Generic)
--
-- instance 'Binary' Record
-- instance 'Structured' Record
-- @
--
-- then you can serialise and deserialise @Record@ values with a structure tag by simply
--
-- @
-- 'structuredEncode' record :: 'LBS.ByteString'
-- 'structuredDecode' lbs :: IO Record
-- @
--
-- If structure of @Record@ changes in between, deserialisation will fail early.
--
-- Technically, 'Structured' is not related to 'Binary', and may
-- be useful in other uses.
module Distribution.Utils.Structured
  ( -- * Encoding and decoding

    -- | These functions operate like @binary@'s counterparts,
    -- but the serialised version has a structure hash in front.
    structuredEncode
  , structuredEncodeFile
  , structuredDecode
  , structuredDecodeOrFailIO
  , structuredDecodeFileOrFail

    -- * Structured class
  , Structured (structure)
  , MD5
  , structureHash
  , structureBuilder
  , genericStructure
  , GStructured
  , nominalStructure
  , containerStructure

    -- * Structure type
  , Structure (..)
  , Tag (..)
  , TypeName
  , ConstructorName
  , TypeVersion
  , SopStructure
  , hashStructure
  , typeVersion
  , typeName
  ) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio)
import Data.Word (Word, Word16, Word32, Word64, Word8)

import qualified Control.Monad.Trans.State.Strict as State

import Control.Exception (ErrorCall (..), catch, evaluate)

import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Builder      as Builder
#else
import qualified Data.ByteString.Lazy.Builder as Builder
#endif
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import qualified Distribution.Compat.Binary as Binary

#ifdef MIN_VERSION_aeson
import qualified Data.Aeson as Aeson
#endif

import Data.Kind (Type)

import Distribution.Compat.Typeable (TypeRep, Typeable, typeRep)
import Distribution.Utils.MD5

import Data.Monoid (mconcat)

import qualified Data.Foldable
import qualified Data.Semigroup

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TypeName = String
type ConstructorName = String

-- | A semantic version of a data type. Usually 0.
type TypeVersion = Word32

-- | Structure of a datatype.
--
-- It can be infinite, as far as 'TypeRep's involved are finite.
-- (e.g. polymorphic recursion might cause troubles).
data Structure
  = -- | nominal, yet can be parametrised by other structures.
    Nominal !TypeRep !TypeVersion TypeName [Structure]
  | -- | a newtype wrapper
    Newtype !TypeRep !TypeVersion TypeName Structure
  | -- | sum-of-products structure
    Structure !TypeRep !TypeVersion TypeName SopStructure
  deriving (Eq, Ord, Show, Generic)

type SopStructure = [(ConstructorName, [Structure])]

-- | A MD5 hash digest of 'Structure'.
hashStructure :: Structure -> MD5
hashStructure = md5 . LBS.toStrict . Builder.toLazyByteString . structureBuilder

-- | A van-Laarhoven lens into 'TypeVersion' of 'Structure'
--
-- @
-- 'typeVersion' :: Lens' 'Structure' 'TypeVersion'
-- @
typeVersion :: Functor f => (TypeVersion -> f TypeVersion) -> Structure -> f Structure
typeVersion f (Nominal t v n s) = fmap (\v' -> Nominal t v' n s) (f v)
typeVersion f (Newtype t v n s) = fmap (\v' -> Newtype t v' n s) (f v)
typeVersion f (Structure t v n s) = fmap (\v' -> Structure t v' n s) (f v)

-- | A van-Laarhoven lens into 'TypeName' of 'Structure'
--
-- @
-- 'typeName' :: Lens' 'Structure' 'TypeName'
-- @
typeName :: Functor f => (TypeName -> f TypeName) -> Structure -> f Structure
typeName f (Nominal t v n s) = fmap (\n' -> Nominal t v n' s) (f n)
typeName f (Newtype t v n s) = fmap (\n' -> Newtype t v n' s) (f n)
typeName f (Structure t v n s) = fmap (\n' -> Structure t v n' s) (f n)

-------------------------------------------------------------------------------
-- Builder
-------------------------------------------------------------------------------

-- | Flatten 'Structure' into something we can calculate hash of.
--
-- As 'Structure' can be potentially infinite. For mutually recursive types,
-- we keep track of 'TypeRep's, and put just 'TypeRep' name when it's occurred
-- another time.
structureBuilder :: Structure -> Builder.Builder
structureBuilder s0 = State.evalState (go s0) Map.empty
  where
    go :: Structure -> State.State (Map.Map String (NonEmpty TypeRep)) Builder.Builder
    go (Nominal t v n s) = withTypeRep t $ do
      s' <- traverse go s
      return $ mconcat $ Builder.word8 1 : Builder.word32LE v : Builder.stringUtf8 n : s'
    go (Newtype t v n s) = withTypeRep t $ do
      s' <- go s
      return $ mconcat [Builder.word8 2, Builder.word32LE v, Builder.stringUtf8 n, s']
    go (Structure t v n s) = withTypeRep t $ do
      s' <- goSop s
      return $ mconcat [Builder.word8 3, Builder.word32LE v, Builder.stringUtf8 n, s']

    withTypeRep t k = do
      acc <- State.get
      case insert t acc of
        Nothing -> return $ mconcat [Builder.word8 0, Builder.stringUtf8 (show t)]
        Just acc' -> do
          State.put acc'
          k

    goSop :: SopStructure -> State.State (Map.Map String (NonEmpty TypeRep)) Builder.Builder
    goSop sop = do
      parts <- traverse part sop
      return $ mconcat parts

    part (cn, s) = do
      s' <- traverse go s
      return $ Data.Monoid.mconcat [Builder.stringUtf8 cn, mconcat s']

    insert :: TypeRep -> Map.Map String (NonEmpty TypeRep) -> Maybe (Map.Map String (NonEmpty TypeRep))
    insert tr m = case Map.lookup trShown m of
      Nothing -> inserted
      Just ne
        | tr `Data.Foldable.elem` ne -> Nothing
        | otherwise -> inserted
      where
        inserted = Just (Map.insertWith (Data.Semigroup.<>) trShown (pure tr) m)
        trShown = show tr

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Class of types with a known 'Structure'.
--
-- For regular data types 'Structured' can be derived generically.
--
-- @
-- data Record = Record { a :: Int, b :: Bool, c :: [Char] } deriving ('Generic')
-- instance 'Structured' Record
-- @
--
-- @since 3.2.0.0
class Typeable a => Structured a where
  structure :: Proxy a -> Structure
  default structure :: (Generic a, GStructured (Rep a)) => Proxy a -> Structure
  structure = genericStructure

  -- This member is hidden. It's there to precalc
  structureHash' :: Tagged a MD5
  structureHash' = Tagged (hashStructure (structure (Proxy :: Proxy a)))

-- private Tagged
newtype Tagged a b = Tagged {untag :: b}

-- | Semantically @'hashStructure' . 'structure'@.
structureHash :: forall a. Structured a => Proxy a -> MD5
structureHash _ = untag (structureHash' :: Tagged a MD5)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Structured 'Binary.encode'.
-- Encode a value to using binary serialisation to a lazy 'LBS.ByteString'.
-- Encoding starts with 16 byte large structure hash.
structuredEncode
  :: forall a
   . (Binary.Binary a, Structured a)
  => a
  -> LBS.ByteString
structuredEncode x = Binary.encode (Tag :: Tag a, x)

-- | Lazily serialise a value to a file
structuredEncodeFile :: (Binary.Binary a, Structured a) => FilePath -> a -> IO ()
structuredEncodeFile f = LBS.writeFile f . structuredEncode

-- | Structured 'Binary.decode'.
-- Decode a value from a lazy 'LBS.ByteString', reconstructing the original structure.
-- Throws pure exception on invalid inputs.
structuredDecode
  :: forall a
   . (Binary.Binary a, Structured a)
  => LBS.ByteString
  -> a
structuredDecode lbs = snd (Binary.decode lbs :: (Tag a, a))

structuredDecodeOrFailIO :: (Binary.Binary a, Structured a) => LBS.ByteString -> IO (Either String a)
structuredDecodeOrFailIO bs =
  catch (evaluate (structuredDecode bs) >>= return . Right) handler
  where
    handler (ErrorCallWithLocation str _) = return $ Left str

-- | Lazily reconstruct a value previously written to a file.
structuredDecodeFileOrFail :: (Binary.Binary a, Structured a) => FilePath -> IO (Either String a)
structuredDecodeFileOrFail f = structuredDecodeOrFailIO =<< LBS.readFile f

-------------------------------------------------------------------------------
-- Helper data
-------------------------------------------------------------------------------

data Tag a = Tag

instance Structured a => Binary.Binary (Tag a) where
  get = do
    actual <- binaryGetMD5
    if actual == expected
      then return Tag
      else
        fail $
          concat
            [ "Non-matching structured hashes: "
            , showMD5 actual
            , "; expected: "
            , showMD5 expected
            ]
    where
      expected = untag (structureHash' :: Tagged a MD5)

  put _ = binaryPutMD5 expected
    where
      expected = untag (structureHash' :: Tagged a MD5)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

-- | Use 'Typeable' to infer name
nominalStructure :: Typeable a => Proxy a -> Structure
nominalStructure p = Nominal tr 0 (show tr) []
  where
    tr = typeRep p

containerStructure :: forall f a. (Typeable f, Structured a) => Proxy (f a) -> Structure
containerStructure _ =
  Nominal
    faTypeRep
    0
    (show fTypeRep)
    [ structure (Proxy :: Proxy a)
    ]
  where
    fTypeRep = typeRep (Proxy :: Proxy f)
    faTypeRep = typeRep (Proxy :: Proxy (f a))

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

-- | Derive 'structure' generically.
genericStructure :: forall a. (Typeable a, Generic a, GStructured (Rep a)) => Proxy a -> Structure
genericStructure _ = gstructured (typeRep (Proxy :: Proxy a)) (Proxy :: Proxy (Rep a)) 0

-- | Used to implement 'genericStructure'.
class GStructured (f :: Type -> Type) where
  gstructured :: TypeRep -> Proxy f -> TypeVersion -> Structure

instance (i ~ D, Datatype c, GStructuredSum f) => GStructured (M1 i c f) where
  gstructured tr _ v = case sop of
    [(_, [s])] | isNewtype p -> Newtype tr v name s
    _ -> Structure tr v name sop
    where
      p = undefined :: M1 i c f ()
      name = datatypeName p
      sop = gstructuredSum (Proxy :: Proxy f) []

class GStructuredSum (f :: Type -> Type) where
  gstructuredSum :: Proxy f -> SopStructure -> SopStructure

instance (i ~ C, Constructor c, GStructuredProd f) => GStructuredSum (M1 i c f) where
  gstructuredSum _ xs = (name, prod) : xs
    where
      name = conName (undefined :: M1 i c f ())
      prod = gstructuredProd (Proxy :: Proxy f) []

instance (GStructuredSum f, GStructuredSum g) => GStructuredSum (f :+: g) where
  gstructuredSum _ xs =
    gstructuredSum (Proxy :: Proxy f) $
      gstructuredSum (Proxy :: Proxy g) xs

instance GStructuredSum V1 where
  gstructuredSum _ = id

class GStructuredProd (f :: Type -> Type) where
  gstructuredProd :: Proxy f -> [Structure] -> [Structure]

instance (i ~ S, GStructuredProd f) => GStructuredProd (M1 i c f) where
  gstructuredProd _ = gstructuredProd (Proxy :: Proxy f)

instance Structured c => GStructuredProd (K1 i c) where
  gstructuredProd _ xs = structure (Proxy :: Proxy c) : xs

instance GStructuredProd U1 where
  gstructuredProd _ = id

instance (GStructuredProd f, GStructuredProd g) => GStructuredProd (f :*: g) where
  gstructuredProd _ xs =
    gstructuredProd (Proxy :: Proxy f) $
      gstructuredProd (Proxy :: Proxy g) xs

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Structured ()
instance Structured Bool
instance Structured Ordering

instance Structured Char where structure = nominalStructure
instance Structured Int where structure = nominalStructure
instance Structured Integer where structure = nominalStructure

instance Structured Data.Word.Word where structure = nominalStructure

instance Structured Int8 where structure = nominalStructure
instance Structured Int16 where structure = nominalStructure
instance Structured Int32 where structure = nominalStructure
instance Structured Int64 where structure = nominalStructure

instance Structured Word8 where structure = nominalStructure
instance Structured Word16 where structure = nominalStructure
instance Structured Word32 where structure = nominalStructure
instance Structured Word64 where structure = nominalStructure

instance Structured Float where structure = nominalStructure
instance Structured Double where structure = nominalStructure

instance Structured a => Structured (Maybe a)
instance (Structured a, Structured b) => Structured (Either a b)
instance Structured a => Structured (Ratio a) where structure = containerStructure
instance Structured a => Structured [a] where structure = containerStructure
instance Structured a => Structured (NonEmpty a) where structure = containerStructure

instance (Structured a1, Structured a2) => Structured (a1, a2)
instance (Structured a1, Structured a2, Structured a3) => Structured (a1, a2, a3)
instance (Structured a1, Structured a2, Structured a3, Structured a4) => Structured (a1, a2, a3, a4)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5) => Structured (a1, a2, a3, a4, a5)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5, Structured a6) => Structured (a1, a2, a3, a4, a5, a6)
instance (Structured a1, Structured a2, Structured a3, Structured a4, Structured a5, Structured a6, Structured a7) => Structured (a1, a2, a3, a4, a5, a6, a7)

instance Structured BS.ByteString where structure = nominalStructure
instance Structured LBS.ByteString where structure = nominalStructure

instance Structured T.Text where structure = nominalStructure
instance Structured LT.Text where structure = nominalStructure

instance (Structured k, Structured v) => Structured (Map.Map k v) where structure _ = Nominal (typeRep (Proxy :: Proxy (Map.Map k v))) 0 "Map" [structure (Proxy :: Proxy k), structure (Proxy :: Proxy v)]
instance Structured k => Structured (Set.Set k) where structure = containerStructure
instance Structured v => Structured (IM.IntMap v) where structure = containerStructure
instance Structured IS.IntSet where structure = nominalStructure
instance Structured v => Structured (Seq.Seq v) where structure = containerStructure

instance Structured Time.UTCTime where structure = nominalStructure
instance Structured Time.DiffTime where structure = nominalStructure
instance Structured Time.UniversalTime where structure = nominalStructure
instance Structured Time.NominalDiffTime where structure = nominalStructure
instance Structured Time.Day where structure = nominalStructure
instance Structured Time.TimeZone where structure = nominalStructure
instance Structured Time.TimeOfDay where structure = nominalStructure
instance Structured Time.LocalTime where structure = nominalStructure
