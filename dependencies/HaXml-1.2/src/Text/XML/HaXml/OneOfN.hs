module Text.XML.HaXml.OneOfN where

import Text.XML.HaXml.Xml2Haskell

data OneOf2 a b
    = OneOf2 a | TwoOf2 b
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b)
    => XmlContent (OneOf2 a b)
  where
    fromElem cs =
        (choice OneOf2 $ choice TwoOf2
        $ (\c->(Nothing,c))) cs
    toElem (OneOf2 x) = toElem x
    toElem (TwoOf2 x) = toElem x

----
data OneOf3 a b c
    = OneOf3 a | TwoOf3 b | ThreeOf3 c
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c)
    => XmlContent (OneOf3 a b c)
  where
    fromElem cs =
        (choice OneOf3 $ choice TwoOf3 $ choice ThreeOf3
        $ (\c->(Nothing,c))) cs
    toElem (OneOf3 x) = toElem x
    toElem (TwoOf3 x) = toElem x
    toElem (ThreeOf3 x) = toElem x

----
data OneOf4 a b c d
    = OneOf4 a | TwoOf4 b | ThreeOf4 c | FourOf4 d
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d)
    => XmlContent (OneOf4 a b c d)
  where
    fromElem cs =
        (choice OneOf4 $ choice TwoOf4 $ choice ThreeOf4 $ choice FourOf4
        $ (\c->(Nothing,c))) cs
    toElem (OneOf4 x) = toElem x
    toElem (TwoOf4 x) = toElem x
    toElem (ThreeOf4 x) = toElem x
    toElem (FourOf4 x) = toElem x

----
data OneOf5 a b c d e
    = OneOf5 a | TwoOf5 b | ThreeOf5 c | FourOf5 d | FiveOf5 e
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e)
    => XmlContent (OneOf5 a b c d e)
  where
    fromElem cs =
        (choice OneOf5 $ choice TwoOf5 $ choice ThreeOf5 $ choice FourOf5
        $ choice FiveOf5
        $ (\c->(Nothing,c))) cs
    toElem (OneOf5 x) = toElem x
    toElem (TwoOf5 x) = toElem x
    toElem (ThreeOf5 x) = toElem x
    toElem (FourOf5 x) = toElem x
    toElem (FiveOf5 x) = toElem x

----
data OneOf6 a b c d e f
    = OneOf6 a | TwoOf6 b | ThreeOf6 c | FourOf6 d | FiveOf6 e | SixOf6 f
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f)
    => XmlContent (OneOf6 a b c d e f)
  where
    fromElem cs =
        (choice OneOf6 $ choice TwoOf6 $ choice ThreeOf6 $ choice FourOf6
        $ choice FiveOf6 $ choice SixOf6
        $ (\c->(Nothing,c))) cs
    toElem (OneOf6 x) = toElem x
    toElem (TwoOf6 x) = toElem x
    toElem (ThreeOf6 x) = toElem x
    toElem (FourOf6 x) = toElem x
    toElem (FiveOf6 x) = toElem x
    toElem (SixOf6 x) = toElem x

----
data OneOf7 a b c d e f g
    = OneOf7 a | TwoOf7 b | ThreeOf7 c | FourOf7 d | FiveOf7 e | SixOf7 f
    | SevenOf7 g
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g)
    => XmlContent (OneOf7 a b c d e f g)
  where
    fromElem cs =
        (choice OneOf7 $ choice TwoOf7 $ choice ThreeOf7 $ choice FourOf7
        $ choice FiveOf7 $ choice SixOf7 $ choice SevenOf7
        $ (\c->(Nothing,c))) cs
    toElem (OneOf7 x) = toElem x
    toElem (TwoOf7 x) = toElem x
    toElem (ThreeOf7 x) = toElem x
    toElem (FourOf7 x) = toElem x
    toElem (FiveOf7 x) = toElem x
    toElem (SixOf7 x) = toElem x
    toElem (SevenOf7 x) = toElem x

----
data OneOf8 a b c d e f g h
    = OneOf8 a | TwoOf8 b | ThreeOf8 c | FourOf8 d | FiveOf8 e | SixOf8 f
    | SevenOf8 g | EightOf8 h
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h)
    => XmlContent (OneOf8 a b c d e f g h)
  where
    fromElem cs =
        (choice OneOf8 $ choice TwoOf8 $ choice ThreeOf8 $ choice FourOf8
        $ choice FiveOf8 $ choice SixOf8 $ choice SevenOf8 $ choice EightOf8
        $ (\c->(Nothing,c))) cs
    toElem (OneOf8 x) = toElem x
    toElem (TwoOf8 x) = toElem x
    toElem (ThreeOf8 x) = toElem x
    toElem (FourOf8 x) = toElem x
    toElem (FiveOf8 x) = toElem x
    toElem (SixOf8 x) = toElem x
    toElem (SevenOf8 x) = toElem x
    toElem (EightOf8 x) = toElem x

----
data OneOf9 a b c d e f g h i
    = OneOf9 a | TwoOf9 b | ThreeOf9 c | FourOf9 d | FiveOf9 e | SixOf9 f
    | SevenOf9 g | EightOf9 h | NineOf9 i
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i)
    => XmlContent (OneOf9 a b c d e f g h i)
  where
    fromElem cs =
        (choice OneOf9 $ choice TwoOf9 $ choice ThreeOf9 $ choice FourOf9
        $ choice FiveOf9 $ choice SixOf9 $ choice SevenOf9 $ choice EightOf9
        $ choice NineOf9
        $ (\c->(Nothing,c))) cs
    toElem (OneOf9 x) = toElem x
    toElem (TwoOf9 x) = toElem x
    toElem (ThreeOf9 x) = toElem x
    toElem (FourOf9 x) = toElem x
    toElem (FiveOf9 x) = toElem x
    toElem (SixOf9 x) = toElem x
    toElem (SevenOf9 x) = toElem x
    toElem (EightOf9 x) = toElem x
    toElem (NineOf9 x) = toElem x

----
data OneOf10 a b c d e f g h i j
    = OneOf10 a | TwoOf10 b | ThreeOf10 c | FourOf10 d | FiveOf10 e
    | SixOf10 f | SevenOf10 g | EightOf10 h | NineOf10 i | TenOf10 j
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j)
    => XmlContent (OneOf10 a b c d e f g h i j)
  where
    fromElem cs =
        (choice OneOf10 $ choice TwoOf10 $ choice ThreeOf10 $ choice FourOf10
        $ choice FiveOf10 $ choice SixOf10 $ choice SevenOf10
        $ choice EightOf10 $ choice NineOf10 $ choice TenOf10
        $ (\c->(Nothing,c))) cs
    toElem (OneOf10 x) = toElem x
    toElem (TwoOf10 x) = toElem x
    toElem (ThreeOf10 x) = toElem x
    toElem (FourOf10 x) = toElem x
    toElem (FiveOf10 x) = toElem x
    toElem (SixOf10 x) = toElem x
    toElem (SevenOf10 x) = toElem x
    toElem (EightOf10 x) = toElem x
    toElem (NineOf10 x) = toElem x
    toElem (TenOf10 x) = toElem x

----
data OneOf11 a b c d e f g h i j k
    = OneOf11 a | TwoOf11 b | ThreeOf11 c | FourOf11 d | FiveOf11 e
    | SixOf11 f | SevenOf11 g | EightOf11 h | NineOf11 i | TenOf11 j
    | ElevenOf11 k
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k)
    => XmlContent (OneOf11 a b c d e f g h i j k)
  where
    fromElem cs =
        (choice OneOf11 $ choice TwoOf11 $ choice ThreeOf11 $ choice FourOf11
        $ choice FiveOf11 $ choice SixOf11 $ choice SevenOf11
        $ choice EightOf11 $ choice NineOf11 $ choice TenOf11
        $ choice ElevenOf11
        $ (\c->(Nothing,c))) cs
    toElem (OneOf11 x) = toElem x
    toElem (TwoOf11 x) = toElem x
    toElem (ThreeOf11 x) = toElem x
    toElem (FourOf11 x) = toElem x
    toElem (FiveOf11 x) = toElem x
    toElem (SixOf11 x) = toElem x
    toElem (SevenOf11 x) = toElem x
    toElem (EightOf11 x) = toElem x
    toElem (NineOf11 x) = toElem x
    toElem (TenOf11 x) = toElem x
    toElem (ElevenOf11 x) = toElem x

----
data OneOf12 a b c d e f g h i j k l
    = OneOf12 a | TwoOf12 b | ThreeOf12 c | FourOf12 d | FiveOf12 e
    | SixOf12 f | SevenOf12 g | EightOf12 h | NineOf12 i | TenOf12 j
    | ElevenOf12 k | TwelveOf12 l
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l)
    => XmlContent (OneOf12 a b c d e f g h i j k l)
  where
    fromElem cs =
        (choice OneOf12 $ choice TwoOf12 $ choice ThreeOf12 $ choice FourOf12
        $ choice FiveOf12 $ choice SixOf12 $ choice SevenOf12
        $ choice EightOf12 $ choice NineOf12 $ choice TenOf12
        $ choice ElevenOf12 $ choice TwelveOf12
        $ (\c->(Nothing,c))) cs
    toElem (OneOf12 x) = toElem x
    toElem (TwoOf12 x) = toElem x
    toElem (ThreeOf12 x) = toElem x
    toElem (FourOf12 x) = toElem x
    toElem (FiveOf12 x) = toElem x
    toElem (SixOf12 x) = toElem x
    toElem (SevenOf12 x) = toElem x
    toElem (EightOf12 x) = toElem x
    toElem (NineOf12 x) = toElem x
    toElem (TenOf12 x) = toElem x
    toElem (ElevenOf12 x) = toElem x
    toElem (TwelveOf12 x) = toElem x

----
data OneOf13 a b c d e f g h i j k l m
    = OneOf13 a | TwoOf13 b | ThreeOf13 c | FourOf13 d | FiveOf13 e
    | SixOf13 f | SevenOf13 g | EightOf13 h | NineOf13 i | TenOf13 j
    | ElevenOf13 k | TwelveOf13 l | ThirteenOf13 m
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m)
    => XmlContent (OneOf13 a b c d e f g h i j k l m)
  where
    fromElem cs =
        (choice OneOf13 $ choice TwoOf13 $ choice ThreeOf13 $ choice FourOf13
        $ choice FiveOf13 $ choice SixOf13 $ choice SevenOf13
        $ choice EightOf13 $ choice NineOf13 $ choice TenOf13
        $ choice ElevenOf13 $ choice TwelveOf13 $ choice ThirteenOf13
        $ (\c->(Nothing,c))) cs
    toElem (OneOf13 x) = toElem x
    toElem (TwoOf13 x) = toElem x
    toElem (ThreeOf13 x) = toElem x
    toElem (FourOf13 x) = toElem x
    toElem (FiveOf13 x) = toElem x
    toElem (SixOf13 x) = toElem x
    toElem (SevenOf13 x) = toElem x
    toElem (EightOf13 x) = toElem x
    toElem (NineOf13 x) = toElem x
    toElem (TenOf13 x) = toElem x
    toElem (ElevenOf13 x) = toElem x
    toElem (TwelveOf13 x) = toElem x
    toElem (ThirteenOf13 x) = toElem x

----
data OneOf14 a b c d e f g h i j k l m n
    = OneOf14 a | TwoOf14 b | ThreeOf14 c | FourOf14 d | FiveOf14 e
    | SixOf14 f | SevenOf14 g | EightOf14 h | NineOf14 i | TenOf14 j
    | ElevenOf14 k | TwelveOf14 l | ThirteenOf14 m | FourteenOf14 n
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n)
    => XmlContent (OneOf14 a b c d e f g h i j k l m n)
  where
    fromElem cs =
        (choice OneOf14 $ choice TwoOf14 $ choice ThreeOf14 $ choice FourOf14
        $ choice FiveOf14 $ choice SixOf14 $ choice SevenOf14
        $ choice EightOf14 $ choice NineOf14 $ choice TenOf14
        $ choice ElevenOf14 $ choice TwelveOf14 $ choice ThirteenOf14
        $ choice FourteenOf14
        $ (\c->(Nothing,c))) cs
    toElem (OneOf14 x) = toElem x
    toElem (TwoOf14 x) = toElem x
    toElem (ThreeOf14 x) = toElem x
    toElem (FourOf14 x) = toElem x
    toElem (FiveOf14 x) = toElem x
    toElem (SixOf14 x) = toElem x
    toElem (SevenOf14 x) = toElem x
    toElem (EightOf14 x) = toElem x
    toElem (NineOf14 x) = toElem x
    toElem (TenOf14 x) = toElem x
    toElem (ElevenOf14 x) = toElem x
    toElem (TwelveOf14 x) = toElem x
    toElem (ThirteenOf14 x) = toElem x
    toElem (FourteenOf14 x) = toElem x

----
data OneOf15 a b c d e f g h i j k l m n o
    = OneOf15 a | TwoOf15 b | ThreeOf15 c | FourOf15 d | FiveOf15 e
    | SixOf15 f | SevenOf15 g | EightOf15 h | NineOf15 i | TenOf15 j
    | ElevenOf15 k | TwelveOf15 l | ThirteenOf15 m | FourteenOf15 n
    | FifteenOf15 o
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o)
    => XmlContent (OneOf15 a b c d e f g h i j k l m n o)
  where
    fromElem cs =
        (choice OneOf15 $ choice TwoOf15 $ choice ThreeOf15 $ choice FourOf15
        $ choice FiveOf15 $ choice SixOf15 $ choice SevenOf15
        $ choice EightOf15 $ choice NineOf15 $ choice TenOf15
        $ choice ElevenOf15 $ choice TwelveOf15 $ choice ThirteenOf15
        $ choice FourteenOf15 $ choice FifteenOf15
        $ (\c->(Nothing,c))) cs
    toElem (OneOf15 x) = toElem x
    toElem (TwoOf15 x) = toElem x
    toElem (ThreeOf15 x) = toElem x
    toElem (FourOf15 x) = toElem x
    toElem (FiveOf15 x) = toElem x
    toElem (SixOf15 x) = toElem x
    toElem (SevenOf15 x) = toElem x
    toElem (EightOf15 x) = toElem x
    toElem (NineOf15 x) = toElem x
    toElem (TenOf15 x) = toElem x
    toElem (ElevenOf15 x) = toElem x
    toElem (TwelveOf15 x) = toElem x
    toElem (ThirteenOf15 x) = toElem x
    toElem (FourteenOf15 x) = toElem x
    toElem (FifteenOf15 x) = toElem x

----
data OneOf16 a b c d e f g h i j k l m n o p
    = OneOf16 a | TwoOf16 b | ThreeOf16 c | FourOf16 d | FiveOf16 e
    | SixOf16 f | SevenOf16 g | EightOf16 h | NineOf16 i | TenOf16 j
    | ElevenOf16 k | TwelveOf16 l | ThirteenOf16 m | FourteenOf16 n
    | FifteenOf16 o | SixteenOf16 p
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p)
    => XmlContent (OneOf16 a b c d e f g h i j k l m n o p)
  where
    fromElem cs =
        (choice OneOf16 $ choice TwoOf16 $ choice ThreeOf16 $ choice FourOf16
        $ choice FiveOf16 $ choice SixOf16 $ choice SevenOf16
        $ choice EightOf16 $ choice NineOf16 $ choice TenOf16
        $ choice ElevenOf16 $ choice TwelveOf16 $ choice ThirteenOf16
        $ choice FourteenOf16 $ choice FifteenOf16 $ choice SixteenOf16
        $ (\c->(Nothing,c))) cs
    toElem (OneOf16 x) = toElem x
    toElem (TwoOf16 x) = toElem x
    toElem (ThreeOf16 x) = toElem x
    toElem (FourOf16 x) = toElem x
    toElem (FiveOf16 x) = toElem x
    toElem (SixOf16 x) = toElem x
    toElem (SevenOf16 x) = toElem x
    toElem (EightOf16 x) = toElem x
    toElem (NineOf16 x) = toElem x
    toElem (TenOf16 x) = toElem x
    toElem (ElevenOf16 x) = toElem x
    toElem (TwelveOf16 x) = toElem x
    toElem (ThirteenOf16 x) = toElem x
    toElem (FourteenOf16 x) = toElem x
    toElem (FifteenOf16 x) = toElem x
    toElem (SixteenOf16 x) = toElem x

----
data OneOf17 a b c d e f g h i j k l m n o p q
    = OneOf17 a | TwoOf17 b | ThreeOf17 c | FourOf17 d | FiveOf17 e
    | SixOf17 f | SevenOf17 g | EightOf17 h | NineOf17 i | TenOf17 j
    | ElevenOf17 k | TwelveOf17 l | ThirteenOf17 m | FourteenOf17 n
    | FifteenOf17 o | SixteenOf17 p | SeventeenOf17 q
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q)
    => XmlContent (OneOf17 a b c d e f g h i j k l m n o p q)
  where
    fromElem cs =
        (choice OneOf17 $ choice TwoOf17 $ choice ThreeOf17 $ choice FourOf17
        $ choice FiveOf17 $ choice SixOf17 $ choice SevenOf17
        $ choice EightOf17 $ choice NineOf17 $ choice TenOf17
        $ choice ElevenOf17 $ choice TwelveOf17 $ choice ThirteenOf17
        $ choice FourteenOf17 $ choice FifteenOf17 $ choice SixteenOf17
        $ choice SeventeenOf17
        $ (\c->(Nothing,c))) cs
    toElem (OneOf17 x) = toElem x
    toElem (TwoOf17 x) = toElem x
    toElem (ThreeOf17 x) = toElem x
    toElem (FourOf17 x) = toElem x
    toElem (FiveOf17 x) = toElem x
    toElem (SixOf17 x) = toElem x
    toElem (SevenOf17 x) = toElem x
    toElem (EightOf17 x) = toElem x
    toElem (NineOf17 x) = toElem x
    toElem (TenOf17 x) = toElem x
    toElem (ElevenOf17 x) = toElem x
    toElem (TwelveOf17 x) = toElem x
    toElem (ThirteenOf17 x) = toElem x
    toElem (FourteenOf17 x) = toElem x
    toElem (FifteenOf17 x) = toElem x
    toElem (SixteenOf17 x) = toElem x
    toElem (SeventeenOf17 x) = toElem x

----
data OneOf18 a b c d e f g h i j k l m n o p q r
    = OneOf18 a | TwoOf18 b | ThreeOf18 c | FourOf18 d | FiveOf18 e
    | SixOf18 f | SevenOf18 g | EightOf18 h | NineOf18 i | TenOf18 j
    | ElevenOf18 k | TwelveOf18 l | ThirteenOf18 m | FourteenOf18 n
    | FifteenOf18 o | SixteenOf18 p | SeventeenOf18 q | EighteenOf18 r
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r)
    => XmlContent (OneOf18 a b c d e f g h i j k l m n o p q r)
  where
    fromElem cs =
        (choice OneOf18 $ choice TwoOf18 $ choice ThreeOf18 $ choice FourOf18
        $ choice FiveOf18 $ choice SixOf18 $ choice SevenOf18
        $ choice EightOf18 $ choice NineOf18 $ choice TenOf18
        $ choice ElevenOf18 $ choice TwelveOf18 $ choice ThirteenOf18
        $ choice FourteenOf18 $ choice FifteenOf18 $ choice SixteenOf18
        $ choice SeventeenOf18 $ choice EighteenOf18
        $ (\c->(Nothing,c))) cs
    toElem (OneOf18 x) = toElem x
    toElem (TwoOf18 x) = toElem x
    toElem (ThreeOf18 x) = toElem x
    toElem (FourOf18 x) = toElem x
    toElem (FiveOf18 x) = toElem x
    toElem (SixOf18 x) = toElem x
    toElem (SevenOf18 x) = toElem x
    toElem (EightOf18 x) = toElem x
    toElem (NineOf18 x) = toElem x
    toElem (TenOf18 x) = toElem x
    toElem (ElevenOf18 x) = toElem x
    toElem (TwelveOf18 x) = toElem x
    toElem (ThirteenOf18 x) = toElem x
    toElem (FourteenOf18 x) = toElem x
    toElem (FifteenOf18 x) = toElem x
    toElem (SixteenOf18 x) = toElem x
    toElem (SeventeenOf18 x) = toElem x
    toElem (EighteenOf18 x) = toElem x

----
data OneOf19 a b c d e f g h i j k l m n o p q r s
    = OneOf19 a | TwoOf19 b | ThreeOf19 c | FourOf19 d | FiveOf19 e
    | SixOf19 f | SevenOf19 g | EightOf19 h | NineOf19 i | TenOf19 j
    | ElevenOf19 k | TwelveOf19 l | ThirteenOf19 m | FourteenOf19 n
    | FifteenOf19 o | SixteenOf19 p | SeventeenOf19 q | EighteenOf19 r
    | NineteenOf19 s
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s)
    => XmlContent (OneOf19 a b c d e f g h i j k l m n o p q r s)
  where
    fromElem cs =
        (choice OneOf19 $ choice TwoOf19 $ choice ThreeOf19 $ choice FourOf19
        $ choice FiveOf19 $ choice SixOf19 $ choice SevenOf19
        $ choice EightOf19 $ choice NineOf19 $ choice TenOf19
        $ choice ElevenOf19 $ choice TwelveOf19 $ choice ThirteenOf19
        $ choice FourteenOf19 $ choice FifteenOf19 $ choice SixteenOf19
        $ choice SeventeenOf19 $ choice EighteenOf19 $ choice NineteenOf19
        $ (\c->(Nothing,c))) cs
    toElem (OneOf19 x) = toElem x
    toElem (TwoOf19 x) = toElem x
    toElem (ThreeOf19 x) = toElem x
    toElem (FourOf19 x) = toElem x
    toElem (FiveOf19 x) = toElem x
    toElem (SixOf19 x) = toElem x
    toElem (SevenOf19 x) = toElem x
    toElem (EightOf19 x) = toElem x
    toElem (NineOf19 x) = toElem x
    toElem (TenOf19 x) = toElem x
    toElem (ElevenOf19 x) = toElem x
    toElem (TwelveOf19 x) = toElem x
    toElem (ThirteenOf19 x) = toElem x
    toElem (FourteenOf19 x) = toElem x
    toElem (FifteenOf19 x) = toElem x
    toElem (SixteenOf19 x) = toElem x
    toElem (SeventeenOf19 x) = toElem x
    toElem (EighteenOf19 x) = toElem x
    toElem (NineteenOf19 x) = toElem x

----
data OneOf20 a b c d e f g h i j k l m n o p q r s t
    = OneOf20 a | TwoOf20 b | ThreeOf20 c | FourOf20 d | FiveOf20 e
    | SixOf20 f | SevenOf20 g | EightOf20 h | NineOf20 i | TenOf20 j
    | ElevenOf20 k | TwelveOf20 l | ThirteenOf20 m | FourteenOf20 n
    | FifteenOf20 o | SixteenOf20 p | SeventeenOf20 q | EighteenOf20 r
    | NineteenOf20 s | TwentyOf20 t
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t)
    => XmlContent (OneOf20 a b c d e f g h i j k l m n o p q r s t)
  where
    fromElem cs =
        (choice OneOf20 $ choice TwoOf20 $ choice ThreeOf20 $ choice FourOf20
        $ choice FiveOf20 $ choice SixOf20 $ choice SevenOf20
        $ choice EightOf20 $ choice NineOf20 $ choice TenOf20
        $ choice ElevenOf20 $ choice TwelveOf20 $ choice ThirteenOf20
        $ choice FourteenOf20 $ choice FifteenOf20 $ choice SixteenOf20
        $ choice SeventeenOf20 $ choice EighteenOf20 $ choice NineteenOf20
        $ choice TwentyOf20
        $ (\c->(Nothing,c))) cs
    toElem (OneOf20 x) = toElem x
    toElem (TwoOf20 x) = toElem x
    toElem (ThreeOf20 x) = toElem x
    toElem (FourOf20 x) = toElem x
    toElem (FiveOf20 x) = toElem x
    toElem (SixOf20 x) = toElem x
    toElem (SevenOf20 x) = toElem x
    toElem (EightOf20 x) = toElem x
    toElem (NineOf20 x) = toElem x
    toElem (TenOf20 x) = toElem x
    toElem (ElevenOf20 x) = toElem x
    toElem (TwelveOf20 x) = toElem x
    toElem (ThirteenOf20 x) = toElem x
    toElem (FourteenOf20 x) = toElem x
    toElem (FifteenOf20 x) = toElem x
    toElem (SixteenOf20 x) = toElem x
    toElem (SeventeenOf20 x) = toElem x
    toElem (EighteenOf20 x) = toElem x
    toElem (NineteenOf20 x) = toElem x
    toElem (TwentyOf20 x) = toElem x

----
