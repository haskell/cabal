module Codec.Manifest.Cabal.Layout
  ( Codec.Manifest.Cabal.Layout.parse

  , render

  , -- * Types
    Offset (..)
  , Whitespace (..)
  , Comment (..)
  , Heading (..)
  , Inline (..)
  , Line (..)
  , Filler (..)
  , Section (..)
  , Contents (..)
  , Field (..)
  , Node (..)
  , Layout (..)
  ) where

import           Codec.Manifest.Cabal.Internal.Layout
import           Codec.Manifest.Cabal.Internal.Parse
import           Codec.Manifest.Cabal.Internal.Render

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.ByteString.Builder
import qualified Data.Text.Lazy as Lazy (Text)
import           Text.Parsec hiding (Line)



parse :: Lazy.Text -> Either ParseError Layout
parse = Text.Parsec.parse layoutP ""



render :: Layout -> BSLC.ByteString
render = toLazyByteString . layoutB
