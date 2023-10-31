{-# LANGUAGE BangPatterns
           , OverloadedStrings
           , RankNTypes #-}

module Codec.Manifest.Cabal.Internal.Parse
  ( layoutP
  ) where

import           Codec.Manifest.Cabal.Internal.Layout

import           Control.Monad
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Builder
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.Internal.StrictBuilder as StrictBuilder
import           Text.Parsec hiding (Line)
import           Text.Parsec.Text.Lazy



isControlC0 :: Char -> Bool
isControlC0 c = (c <= '\x1F' && c /= '\t' && c /= '\r') || c == '\DEL'



data Curliness = Normal | Curled

commentP :: Parser (Whitespace -> Comment)
commentP = do
  space1_ <- many $ satisfy (\c -> c /= '\n' && isSpace c)
  let !space1 = length space1_

      sift = do
        c <- anyChar
        if isControlC0 c
          then fail "C0 control codes and '\\DEL' are not allowed"
          else pure c

  txt_ <- manyTill sift $ eof <|> lookAhead (() <$ satisfy (== '\n'))
  let !txt = Text.pack txt_

  pure $ \space0 -> Comment space0 (Whitespace space1) txt

uncomment :: Filler -> Inline
uncomment filler =
  case filler of
    CommentF (Comment space0 (Whitespace space1) comment) ->
      let !txt = StrictBuilder.toText . StrictBuilder.unsafeFromByteString
                   . BSL.toStrict . toLazyByteString
                       $ string8 "--"
                      <> foldMap id (replicate space1 $ char8 ' ')
                      <> byteString (encodeUtf8 comment)

      in Inline space0 txt

    EmptyF space_ -> EmptyI space_



rightCurlyP :: SourcePos -> Parser ()
rightCurlyP curlyPos = do
  let sourcePosS pos = showString "line " . shows (sourceLine pos)
                     . showString ", column " . shows (sourceColumn pos)

  mayc <- optionMaybe $ lookAhead anyChar
  case mayc of
    Just c
      | c == '}'  -> void anyChar

      | otherwise ->
          fail $
            showString "Curly section started on " . sourcePosS curlyPos
              $ showString
                  " was not consumed correctly. This is not supposed to\
                  \ happen, please report it as a Cabal bug." []

    Nothing  ->
      fail $
        showString "Reached end of file without finding a closing bracket\
                  \ for curly section started on " $ sourcePosS curlyPos []



data Lined = Newline
           | Trailing
             deriving Show

data Stop = LineEnd Lined
          | MidLine Whitespace
            deriving Show

data Next = NextLine Whitespace
          | NextComment Comment Lined
          | NextEmpty Whitespace Lined
          | NextFin
            deriving Show

nextP :: Curliness -> (Next -> Parser a) -> Parser a
nextP curliness f = go 0
  where
    go !n = do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just c
          | c == '\n' -> do
              _ <- anyChar
              f $ NextEmpty (Whitespace n) Newline

          | isSpace c     -> do
              _ <- anyChar
              go (n + 1)

          | c == '-'      -> do
              mayd <- lookAhead $ do
                        _ <- anyChar
                        optionMaybe anyChar

              case mayd of
                Just '-' -> do
                  _ <- anyChar
                  _ <- anyChar
                  comment_ <- commentP
                  let !comment = comment_ (Whitespace n)

                  maye <- optionMaybe $ lookAhead anyChar
                  lined <- case maye of
                             Just '\n' -> do _ <- anyChar
                                             pure Newline

                             _         -> pure Trailing

                  f $ NextComment comment lined

                _        -> f $ NextLine (Whitespace n)

          | c == '}' ->
              f $ case curliness of
                    Curled
                      | n <= 0    -> NextFin
                      | otherwise -> NextEmpty (Whitespace n) Trailing

                    _      -> NextLine (Whitespace n)

          | otherwise -> f $ NextLine (Whitespace n)

        Nothing ->
          f $ if n > 0
                then NextEmpty (Whitespace n) Trailing
                else NextFin



data Anchor = Bottom
            | Anchor Int

data Belonging = Belongs Offset
               | Above

relative :: Anchor -> Whitespace -> Belonging
relative anchor (Whitespace space_) =
  case anchor of
    Bottom       -> Belongs (Offset space_)
    Anchor pivot -> let off = space_ - pivot
                    in if off > 0
                         then Belongs (Offset off)
                         else Above



-- | Comments and newlines whose position within the layout has not yet been figured out.
data Overflow = NoOverflow
              | Overflow
                  (forall a. (Whitespace -> a) -> (Comment -> a) -> [a] -> [a])

instance Show Overflow where
  showsPrec d flow =
    case flow of
      NoOverflow -> showString "NoOverflow"
      Overflow _ -> showParen (d > 10) $ showString "Overflow _"

flowComment :: Comment -> Overflow -> Overflow
flowComment comment flow =
  case flow of
    NoOverflow -> Overflow $ \_   comm -> (:) (comm comment)
    Overflow f -> Overflow $ \new comm -> f new comm . (:) (comm comment)

flowEmpty :: Whitespace -> Overflow -> Overflow
flowEmpty space_ flow =
  case flow of
    NoOverflow -> Overflow $ \new _    -> (:) (new space_)
    Overflow f -> Overflow $ \new comm -> f new comm . (:) (new space_)

runOverflow :: (Whitespace -> a) -> (Comment -> a) -> Overflow -> [a] -> [a]
runOverflow new comm flow =
  case flow of
    NoOverflow -> id
    Overflow f -> f new comm

nodeOverflow :: Overflow -> [Node] -> [Node]
nodeOverflow = runOverflow EmptyN CommentN

lineOverflow :: Overflow -> [Line] -> [Line]
lineOverflow = runOverflow EmptyL CommentL

fillerOverflow :: Overflow -> [Filler] -> [Filler]
fillerOverflow = runOverflow EmptyF CommentF



data Result = Proper Node Overflow Stop
            | Fin (Maybe Whitespace)
              deriving Show

data NodeLine = JustSection
                  Int -- ^ Length of the section name
                  Int -- ^ Whitespace between the name and line end

              | CurlySection
                  Int -- ^ Length of the section name
                  Int -- ^ Whitespace between the name and left curly brace

              | CommentSection
                  Int -- ^ Length of the section name
                  Int -- ^ Whitespace between the name and the comment start

              | JustField
                  Int -- ^ Length of the field name
                  Int -- ^ Whitespace between the name and the colon

                deriving Show

layoutP :: Parser Layout
layoutP = do
  (acc, flow, stop) <- nodesP Normal Bottom (LineEnd Trailing)

  atEnd <- option False $ True <$ eof
  unless atEnd $
    fail "Input was not consumed in its entirety. This is not supposed to\
         \ happen, please report it as a Cabal bug."

  let base = case stop of
               LineEnd Newline  -> [EmptyN (Whitespace 0)]
               LineEnd Trailing -> []
               MidLine _        -> []

  pure . Layout . acc $ nodeOverflow flow base


nodesP :: Curliness -> Anchor -> Stop -> Parser ([Node] -> [Node], Overflow, Stop)
nodesP curliness anchor = go id NoOverflow
  where
    go acc flow stop = do
      result <- case stop of
                  LineEnd _      -> nodeP curliness anchor
                  MidLine lines_ -> spaceNodeP curliness anchor lines_

      case result of
        Proper node flow' stop' ->
          let acc' xs = acc $ nodeOverflow flow (node : xs)
          in case node of
               Section _ _ _    -> go acc' flow' stop'
               Field _ _ _ _    -> go acc' flow' stop'

               CommentN comment -> go acc (flowComment comment flow) stop'
               EmptyN space_    -> go acc (flowEmpty space_ flow) stop'

        Fin mayOff ->
          pure
            ( acc
            , flow
            , case mayOff of
                Just space' -> MidLine space'
                Nothing     -> stop
            )



nodeP :: Curliness -> Anchor -> Parser Result
nodeP curliness anchor =
  nextP curliness $ \next -> do
    case next of
      NextLine space_            ->
        spaceNodeP curliness anchor space_

      NextComment comment lines_ ->
        pure $ Proper (CommentN comment) NoOverflow (LineEnd lines_)

      NextEmpty space_ lines_    ->
        pure $ Proper (EmptyN space_) NoOverflow (LineEnd lines_)

      NextFin                    -> pure $ Fin Nothing



spaceNodeP :: Curliness -> Anchor -> Whitespace -> Parser Result
spaceNodeP curliness anchor (Whitespace space_) = do
  case relative anchor (Whitespace space_) of
    Above       -> pure $ Fin (Just (Whitespace space_))
    Belongs off -> do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just ':' -> fail "Colon"
        Just '{' -> fail "Curly bracket"
        _        -> pure ()

      future <- lookAhead $ firstWordP 0
      case future of
        JustSection len space' -> do
          heading_ <- count len anyChar
          let !heading = Text.pack heading_

          _ <- count space' anyChar
          mc <- optionMaybe $ lookAhead anyChar
          lined <- case mc of
                     Just '\n' -> do void anyChar
                                     pure Newline
                     _         -> pure Trailing

          (section, flow', stop) <-
            ambiguousSectionP curliness (Anchor space_) (EmptyF (Whitespace space')) lined

          pure $
            Proper
              (Section off (Heading heading) section)
              flow'
              stop

        CurlySection len space' -> do
          heading_ <- count len anyChar
          let !heading = Text.pack heading_

          _ <- count space' anyChar

          curlyPos <- getPosition
          _ <- anyChar -- '{'

          (acc, flow', stop) <- nodesP Curled Bottom (LineEnd Trailing)

          rightCurlyP curlyPos

          let base = case stop of
                       LineEnd Newline  -> [EmptyN (Whitespace 0)]
                       LineEnd Trailing -> []
                       MidLine _        -> []

          pure $
            Proper
              (Section off (Heading heading) $ CurlS [EmptyF (Whitespace space')] (acc $ nodeOverflow flow' base))
              NoOverflow
              (LineEnd Trailing)

        CommentSection len space' -> do
          heading_ <- count len anyChar
          let !heading = Text.pack heading_

          _ <- count space' anyChar
          _ <- anyChar -- '-'
          _ <- anyChar -- '-'
          comment <- commentP

          mc <- optionMaybe $ lookAhead anyChar
          lined <- case mc of
                     Just '\n' -> do void anyChar
                                     pure Newline
                     _         -> pure Trailing

          (section, flow', stop) <-
            ambiguousSectionP curliness (Anchor space_) (CommentF $ comment (Whitespace space')) lined

          pure $
            Proper
              (Section off (Heading heading) section)
              flow'
              stop

        JustField len space' -> do
          let !_ = off

          heading_ <- count len anyChar
          let !heading = Text.pack heading_

          _ <- count space' anyChar
          _ <- anyChar -- ':'

          (field, flow', stop) <- fieldP curliness (Anchor space_)

          pure $
            Proper
              (Field off (Heading heading) (Whitespace space') field)
              flow'
              stop

  where
    firstWordP :: Int -> Parser NodeLine
    firstWordP !n = do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just c
          | c == '\n'     -> pure $ JustSection n 0

          | isSpace c     -> do
              _ <- anyChar
              firstSpaceP n 1

          | c == '-'      -> do
              mayd <- lookAhead $ do
                        _ <- anyChar
                        optionMaybe anyChar

              case mayd of
                Just '-' -> pure $ CommentSection n 0
                Just _   -> do
                  _ <- anyChar
                  firstWordP (n + 1)

                Nothing  -> pure $ JustSection n 0

          | c == ':'      -> pure $ JustField n 0
          | c == '{'      -> pure $ CurlySection n 0
          | c == '}'      ->
              case curliness of
                Curled -> pure $ JustSection n 0
                Normal -> fail "Closing curly bracket found, but no opening one"

          | isControlC0 c ->
              fail "C0 control codes and '\\DEL' are not allowed"

          | otherwise     -> do
              _ <- anyChar
              firstWordP (n + 1)

        Nothing -> pure $ JustSection n 0

    firstSpaceP :: Int -> Int -> Parser NodeLine
    firstSpaceP n !m = do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just c
          | c == '\n'     -> pure $ JustSection n m

          | isSpace c     -> do
              _ <- anyChar
              firstSpaceP n (m + 1)

          | c == '-'      -> do
              mayd <- lookAhead $ do
                        _ <- anyChar
                        optionMaybe anyChar

              case mayd of
                Just '-' -> pure $ CommentSection n m
                Just _   -> do
                  _ <- anyChar
                  moreWordsP (n + m + 1) 0

                Nothing  -> pure $ JustSection n m

          | c == ':'      -> pure $ JustField n m
          | c == '{'      -> pure $ CurlySection n m
          | c == '}'      ->
              case curliness of
                Curled -> pure $ JustSection n m
                Normal -> fail "Closing curly bracket found, but no opening one"

          | isControlC0 c ->
              fail "C0 control codes and '\\DEL' are not allowed"

          | otherwise     -> do
              _ <- anyChar
              moreWordsP (n + m + 1) 0

        Nothing -> pure $ JustSection n m

    moreWordsP :: Int -> Int -> Parser NodeLine
    moreWordsP !n !m = do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just c
          | c == '\n'     -> pure $ JustSection n m

          | isSpace c     -> do
              _ <- anyChar
              moreWordsP n (m + 1)

          | c == '-'      -> do
              mayd <- lookAhead $ do
                        _ <- anyChar
                        optionMaybe anyChar

              case mayd of
                Just '-' -> pure $ CommentSection n m
                Just _   -> do
                  _ <- anyChar
                  moreWordsP (n + m + 1) 0

                Nothing  -> pure $ JustSection n m

          | c == '{'      -> pure $ CurlySection n m
          | c == '}'      ->
              case curliness of
                Curled -> pure $ JustSection n m
                Normal -> fail "Closing curly bracket found, but no opening one"

          | c == ':'      -> pure $ JustField n m

          | isControlC0 c ->
              fail "C0 control codes and '\\DEL' are not allowed"

          | otherwise     -> do
              _ <- anyChar
              moreWordsP (n + m + 1) 0

        Nothing -> pure $ JustSection n 0



ambiguousSectionP
  :: Curliness
  -> Anchor
  -> Filler
  -> Lined
  -> Parser (Section, Overflow, Stop)
ambiguousSectionP curliness anchor inline = go NoOverflow
  where
    go flow lined =
      nextP curliness $ \next ->
        case next of
          NextLine space_ -> do
            c <- lookAhead anyChar
            case c of
              '{' -> do
                curlyPos <- getPosition
                _ <- anyChar

                (acc, flow', stop) <- nodesP Curled Bottom (LineEnd Trailing)
                rightCurlyP curlyPos

                let lines_ = inline : fillerOverflow flow [EmptyF space_]

                    base = case stop of
                             LineEnd Newline  -> [EmptyN (Whitespace 0)]
                             LineEnd Trailing -> []
                             MidLine _        -> []

                pure
                  ( CurlS lines_ (acc $ nodeOverflow flow' base)
                  , NoOverflow
                  , LineEnd Trailing
                  )

              _   ->
                case relative anchor space_ of
                  Belongs _ -> do
                    (acc, flow', stop) <- nodesP curliness anchor (MidLine space_)

                    let nodes = nodeOverflow flow $ acc []

                    pure (NormalS inline nodes, flow', stop)

                  Above -> pure (NormalS inline [], flow, MidLine space_)

          NextComment comment lined' -> go (flowComment comment flow) lined'

          NextEmpty space1 lined'    -> go (flowEmpty space1 flow) lined'

          NextFin               ->
            pure
              ( NormalS inline []
              , flow
              , LineEnd lined
              )



lineP :: Curliness -> Parser (Text, Lined)
lineP curliness = do
  let predicate = case curliness of
                    Normal -> (== '\n')
                    Curled -> \c -> c == '\n' || c == '}'

  txt_ <- manyTill anyChar $ eof <|> lookAhead (() <$ satisfy predicate)
  let !txt = Text.pack txt_

  maye <- optionMaybe $ lookAhead anyChar
  lined <- case maye of
             Just '\n' -> do void anyChar
                             pure Newline

             _         -> pure Trailing

  pure (txt, lined)



fieldP :: Curliness -> Anchor -> Parser (Field, Overflow, Stop)
fieldP curliness0 anchor0 =
  nextP curliness0 $ \next ->
    case next of
      NextLine space_           -> do
        c <- lookAhead anyChar
        case c of
          '{' -> do
            curlyPos <- getPosition
            _ <- anyChar
            contents <- curledInlineP
            rightCurlyP curlyPos
            pure
              ( CurlF [EmptyF space_] contents
              , NoOverflow
              , LineEnd Trailing
              )

          _   -> do
            (txt, lined) <- lineP curliness0
            normalP curliness0 anchor0 (Inline space_ txt) id NoOverflow lined

      NextComment comment lined ->
        ambiguousP curliness0 anchor0 (CommentF comment) NoOverflow lined

      NextEmpty space1 lined  ->
        ambiguousP curliness0 anchor0 (EmptyF space1) NoOverflow lined

      NextFin                   ->
        pure
          ( NormalF (Contents (EmptyI (Whitespace 0)) [])
          , NoOverflow
          , LineEnd Trailing
          )

  where
    ambiguousP curliness anchor inline flow lined0 =
      nextP curliness $ \next ->
        case next of
          NextLine space_ -> do
            c <- lookAhead anyChar
            case c of
              '{' -> do
                curlyPos <- getPosition
                _ <- anyChar
                contents <- curledInlineP
                let lines_ = inline : fillerOverflow flow [EmptyF space_]

                rightCurlyP curlyPos
                pure
                  ( CurlF lines_ contents
                  , NoOverflow
                  , LineEnd Trailing
                  )

              _   ->
                let !inline' = uncomment inline
                in case relative anchor space_ of
                     Belongs off -> do
                       (txt, lined1) <- lineP curliness

                       normalP curliness anchor inline'
                               (lineOverflow flow . (:) (Line off txt))
                               NoOverflow
                               lined1

                     Above ->
                       pure
                         ( NormalF (Contents inline' [])
                         , flow
                         , MidLine space_
                         )

          NextComment comment lined1 ->
            ambiguousP curliness anchor inline (flowComment comment flow) lined1

          NextEmpty space1 lined1  ->
            ambiguousP curliness anchor inline (flowEmpty space1 flow) lined1

          NextFin               ->
            let !inline' = uncomment inline
            in pure
                 ( NormalF (Contents inline' [])
                 , flow
                 , LineEnd lined0
                 )


    curledInlineP =
      nextP Curled $ \next ->
        case next of
          NextLine (Whitespace space_) -> do
            (txt, lined) <- lineP Curled
            let !inline = Inline (Whitespace space_) txt
            curledP inline id lined

          NextComment comment lined ->
            let !inline = uncomment (CommentF comment)
            in curledP inline id lined

          NextEmpty space_ lined ->
            curledP (EmptyI space_) id lined

          NextFin -> pure $ Contents (EmptyI (Whitespace 0)) []

    curledP inline acc lined =
      nextP Curled $ \next ->
        case next of
          NextLine (Whitespace space_) -> do
            (txt, lined') <- lineP Curled
            curledP inline (acc . (:) (Line (Offset space_) txt)) lined'

          NextComment comment lined'   ->
            curledP inline (acc . (:) (CommentL comment)) lined'

          NextEmpty space_ lined'      ->
            curledP inline (acc . (:) (EmptyL space_)) lined'

          NextFin                      ->
            let base = case lined of
                         Newline  -> [EmptyL (Whitespace 0)]
                         Trailing -> []

            in pure $ Contents inline (acc base)


    normalP curliness anchor inline acc flow lined =
      nextP curliness $ \next ->
        case next of
          NextLine space_            ->
            case relative anchor space_ of
              Belongs off -> do
                (txt, lined') <- lineP curliness

                normalP curliness anchor inline
                        (acc . lineOverflow flow . (:) (Line off txt))
                        NoOverflow
                        lined'

              Above -> do
                let !nodes = acc []
                pure
                  ( NormalF (Contents inline nodes)
                  , flow
                  , MidLine space_
                  )

          NextComment comment lined' ->
            normalP curliness anchor inline acc (flowComment comment flow) lined'

          NextEmpty space_ lined'    ->
            normalP curliness anchor inline acc (flowEmpty space_ flow) lined'

          NextFin                    ->
            pure (NormalF (Contents inline (acc [])), flow, LineEnd lined)
