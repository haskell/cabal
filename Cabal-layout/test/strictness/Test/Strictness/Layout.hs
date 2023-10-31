{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Strictness.Layout () where

import           Codec.Manifest.Cabal.Internal.Layout

import           NoThunks.Class



instance NoThunks Offset where
  wNoThunks ctx (Offset off) = wNoThunks ctx off
  showTypeOf _ = "Offset"

instance NoThunks Whitespace where
  wNoThunks ctx (Whitespace space) = wNoThunks ctx space
  showTypeOf _ = "Whitespace"


instance NoThunks Comment where
  wNoThunks ctx (Comment space0 space1 comment) =
    allNoThunks
      [ noThunks ("(0)":ctx) space0
      , noThunks ("(1)":ctx) space1
      , noThunks ctx comment
      ]

  showTypeOf _ = "Comment"


instance NoThunks Heading where
  wNoThunks ctx (Heading heading) = wNoThunks ctx heading
  showTypeOf _ = "Heading"


instance NoThunks Inline where
  wNoThunks ctx inline =
    case inline of
      Inline space text ->
        allNoThunks
          [ noThunks ("{I}":ctx) space
          , noThunks ("{I}":ctx) text
          ]

      EmptyI space      -> noThunks ("{E}":ctx) space

  showTypeOf _ = "Inline"


instance NoThunks Line where
  wNoThunks ctx line =
    case line of
      Line off text    ->
        allNoThunks
          [ noThunks ("{L}":ctx) off
          , noThunks ("{L}":ctx) text
          ]

      CommentL comment -> wNoThunks ("{C}":ctx) comment

      EmptyL space     -> noThunks ("{E}":ctx) space

  showTypeOf _ = "Line"


instance NoThunks Filler where
  wNoThunks ctx filler =
    case filler of
      CommentF comment -> wNoThunks ("{C}":ctx) comment

      EmptyF space     -> noThunks ("{E}":ctx) space

  showTypeOf _ = "Filler"


newtype List a = List [a]

instance NoThunks a => NoThunks (List a) where
  wNoThunks ctx (List xs) =
    let brackets :: Int -> String
        brackets n = showChar '[' . shows n $ showChar ']' []

    in allNoThunks . fmap (\(n, x) -> wNoThunks (brackets n : ctx) x) $ zip [0 :: Int ..] xs

  showTypeOf _ = "List"


instance NoThunks Section where
  wNoThunks ctx section =
    case section of
      CurlS fillers nodes  ->
        allNoThunks
          [ wNoThunks ("{C}":ctx) (List fillers)
          , wNoThunks ("{C}":ctx) (List nodes)
          ]

      NormalS inline nodes ->
        allNoThunks
          [ wNoThunks ("{N}":ctx) inline
          , wNoThunks ("{N}":ctx) (List nodes)
          ]

  showTypeOf _ = "Section"


instance NoThunks Contents where
  wNoThunks ctx (Contents inline lines_) =
    allNoThunks
     [ wNoThunks ("<0>":ctx) inline
     , wNoThunks ("<1>":ctx) (List lines_)
     ]

  showTypeOf _ = "Contents"

instance NoThunks Field where
  wNoThunks ctx field =
    case field of
      CurlF fillers contents ->
        allNoThunks
          [ wNoThunks ("{C}":ctx) (List fillers)
          , wNoThunks ("{C}":ctx) contents
          ]

      NormalF contents       -> wNoThunks ("{N}":ctx) contents

  showTypeOf _ = "Field"


instance NoThunks Node where
  wNoThunks ctx node =
    case node of
      Section off heading section   ->
        allNoThunks
          [ noThunks ("{S}":ctx) off
          , noThunks ("{S}":ctx) heading
          , wNoThunks ("{S}":ctx) section
          ]

      Field off heading space field ->
        allNoThunks
          [ noThunks ("{F}":ctx) off
          , noThunks ("{F}":ctx) heading
          , noThunks ("{F}":ctx) space
          , wNoThunks ("{F}":ctx) field
          ]

      CommentN comment              -> wNoThunks ("{C}":ctx) comment
      EmptyN space                  -> noThunks ("{E}":ctx) space

  showTypeOf _ = "Node"


instance NoThunks Layout where
  wNoThunks ctx (Layout nodes) = wNoThunks ctx (List nodes)
  showTypeOf _ = "Layout"
