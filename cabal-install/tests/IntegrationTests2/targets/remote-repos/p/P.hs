module P where

import Codec.Serialise
import Codec.CBOR.Decoding (peekByteOffset)

p = serialise "hello world"
