module Codec.Manifest.Cabal.Internal.Render
  ( layoutB
  ) where

import           Codec.Manifest.Cabal.Internal.Layout

import           Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim
import           Data.Text.Encoding



data Anchor = Bottom
            | Anchor Int
              deriving Show

mintercalate :: Monoid m => m -> [m] -> m
mintercalate x ys =
  let go a bs =
        case bs of
          b:cs -> a <> x <> go b cs
          []   -> a

  in case ys of
       a:bs   -> go a bs
       []     -> mempty



offset :: Offset -> Anchor -> Anchor
offset (Offset n) anchor =
  case anchor of
    Bottom   | n > 0     -> Anchor n
             | otherwise -> Bottom
    Anchor m -> Anchor (m + n)

anchorB :: Anchor -> Builder
anchorB anchor =
  case anchor of
    Bottom   -> mempty
    Anchor n -> spaceB (Whitespace $ max n 1)



spaceB :: Whitespace -> Builder
spaceB (Whitespace n) =
  Prim.primMapListBounded (Prim.liftFixedToBounded Prim.char8) (replicate n ' ')

commentB :: Comment -> Builder
commentB (Comment space0 space1 comment space2) =
  spaceB space0 <> string8 "--" <> spaceB space1 <> byteString (encodeUtf8 comment)
                                                 <> spaceB space2



fillerB :: Filler -> Builder
fillerB filler =
  case filler of
    CommentF comment -> commentB comment
    EmptyF space     -> spaceB space

lineB :: Anchor -> Line -> Builder
lineB anchor l =
  case l of
    Line off txt space -> anchorB (offset off anchor) <> byteString (encodeUtf8 txt)
                                                      <> spaceB space
    CommentL comment   -> commentB comment
    EmptyL space       -> spaceB space



contentsB :: Anchor -> Contents -> Builder
contentsB anchor (Contents inline lines_) =
  let fit inl =
        case inl of
          Inline (Whitespace space0) txt space1 -> Line (Offset space0) txt space1
          EmptyI space                          -> EmptyL space

  in mintercalate (char8 '\n') $
       lineB Bottom (fit inline) : (fmap (lineB anchor) lines_)



data Trail = Contextual
           | Trailing

sectionB :: Anchor -> Trail -> Section -> (Builder, Trail)
sectionB anchor trail section =
  case section of
    CurlS fillers nodes ->
      (    mintercalate (char8 '\n') (fmap fillerB fillers)
        <> char8 '{'
        <> fst (nodesB Bottom Contextual nodes)
        <> char8 '}'
      , Trailing
      )

    NormalS filler nodes ->
      case nodes of
        [] -> (fillerB filler, trail)
        _  -> let ~(rendered, trail') = nodesB anchor trail nodes
              in (    fillerB filler
                   <> char8 '\n'
                   <> rendered
                 , trail'
                 )



fieldB :: Anchor -> Field -> (Builder, Trail)
fieldB anchor field =
  case field of
    CurlF fillers contents ->
      (    mintercalate (char8 '\n') (fmap fillerB fillers)
        <> char8 '{'
        <> contentsB Bottom contents
        <> char8 '}'
      , Trailing
      )

    NormalF contents ->
      (contentsB anchor contents, Contextual)



layoutB :: Layout -> Builder
layoutB (Layout nodes) = fst $ nodesB Bottom Contextual nodes

nodesB :: Anchor -> Trail -> [Node] -> (Builder, Trail)
nodesB anchor trail xs =
  case xs of
    y:zs -> go y zs
    []   -> (mempty, Contextual)
  where
    go a bs =
      let ~(rendered, trail') = nodeB anchor trail a
      in case bs of
           b:cs -> let (more, trail'') = go b cs
                   in ( rendered <> case trail' of
                                      Contextual -> char8 '\n' <> more
                                      Trailing   -> more
                      , trail''
                      )

           []   -> (rendered, trail')

nodeB :: Anchor -> Trail -> Node -> (Builder, Trail)
nodeB anchor trail node =
  case node of
    Section off (Heading heading) section ->
      let anchor' = offset off anchor

          ~(rendered, trail') = sectionB anchor' trail section

      in (    anchorB anchor'
           <> byteString (encodeUtf8 heading)
           <> rendered
         , trail'
         )

    Field off (Name name) space field ->
      let anchor' = offset off anchor

          ~(rendered, trail') = fieldB anchor' field

      in (    anchorB anchor'
           <> byteString (encodeUtf8 name)
           <> spaceB space
           <> char8 ':'
           <> rendered
         , trail'
         )

    CommentN comment -> (commentB comment, Contextual)

    EmptyN space     -> (spaceB space, Contextual)
