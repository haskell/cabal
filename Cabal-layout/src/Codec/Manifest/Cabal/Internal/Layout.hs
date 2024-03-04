{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , QuantifiedConstraints
           , StandaloneDeriving
           , UndecidableInstances #-}

module Codec.Manifest.Cabal.Internal.Layout where

import           Data.Text (Text)



-- | Context-dependent whitespace.
newtype Offset = Offset Int
                 deriving newtype Show

-- | Context-independent whitespace.
newtype Whitespace = Whitespace Int
                     deriving newtype Show

-- | Anything that follows two consecutive hyphens. Lasts until the end of the line.
data Comment = Comment
                 Whitespace -- ^ Before double hyphens
                 Whitespace -- ^ Between double hyphens and text
                 Text
                 Whitespace -- ^ Between end of comment and end of line
               deriving Show



-- | Any Unicode characters, excluding '\x00'..'\x1F'
--   ('\r' and '\t' are allowed), '\DEL', '{', '}', ':'.
newtype Heading = Heading Text
                  deriving newtype Show

-- | Any Unicode characters, excluding '\x00'..'\x1F', '\DEL', '{', '}', ':' and spaces.
newtype Name = Name Text
               deriving newtype Show




-- | Field contents at the declaration line.
data Inline = Inline
                Whitespace -- ^ Between colon and start of text
                Text
                Whitespace -- ^ Between end of text and end of line

            | EmptyI Whitespace

              deriving Show

-- | Field contents at the lines following the declaration.
data Line = Line
              Offset
              Text
              Whitespace -- ^ Between end of text and end of line

          | CommentL Comment

          | EmptyL Whitespace

            deriving Show



-- | Non-meaningful information.
data Filler = CommentF Comment
            | EmptyF Whitespace
              deriving Show



-- | Section contents with the curly bracket alternative.
data Section = CurlS
                 [Filler] -- ^ Between heading and left curly
                 [Node]

             | NormalS
                 Filler   -- ^ Inline comment
                 [Node]
               deriving Show



-- | Field contents.
data Contents = Contents Inline [Line]
                deriving Show

-- | Field contents with the curly bracket alternative.
data Field = CurlF
               [Filler] -- ^ Between colon and left curly
               Contents

           | NormalF Contents
             deriving Show



data Node = Section
              Offset
              Heading
              Section

          | Field
              Offset
              Name
              Whitespace -- ^ Between field name and colon
              Field

          | CommentN Comment

          | EmptyN Whitespace

            deriving Show



newtype Layout = Layout [Node]
                 deriving newtype Show
