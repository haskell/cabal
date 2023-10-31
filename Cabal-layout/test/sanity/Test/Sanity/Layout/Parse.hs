{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sanity.Layout.Parse
  ( parseT
  ) where

import           Codec.Manifest.Cabal.Internal.Layout
import           Codec.Manifest.Cabal.Internal.Parse

import           Data.String
import qualified Data.Text.Lazy as Lazy (Text)
import           Text.Parsec hiding (Line)
import           Test.Tasty
import           Test.Tasty.HUnit



deriving instance Eq Offset
deriving instance Eq Whitespace
deriving instance Eq Comment
deriving instance Eq Heading
deriving instance Eq Inline
deriving instance Eq Line
deriving instance Eq Filler
deriving instance Eq Section
deriving instance Eq Contents
deriving instance Eq Field
deriving instance Eq Node
deriving instance Eq Layout

deriving instance Num Offset
deriving instance Num Whitespace

deriving instance IsString Heading



(==>) :: Lazy.Text -> [Node] -> Assertion
input ==> output = parse layoutP "" input @?= Right (Layout output)

fails :: Lazy.Text -> Assertion
fails input =
  case parse layoutP "" input of
    Left _    -> pure ()
    Right res -> assertFailure . showString "Succeeded: " $ show res

parseT :: TestTree
parseT = do
  testGroup "Parse"
    [ testCase "Empty" $
        "" ==> []

    , testCase "Lone C0" $
        fails "\0"

    , testCase "Lone colon" $
        fails ":"

    , testCase "Lone left curly" $
        fails "{"

    , testCase "Lone right curly" $
        fails "{"

    , testCase "Lone bracket section" $
        fails "{}"

    , testGroup "Newlines"
        [ testCase "n"
            newline1

        , testCase "1n2"
            newline2

        , testCase "4n2n3n1n"
            newline3
        ]

    , testGroup "Comments"
        [ testCase "C0" $
            fails "--c\0mment"

        , testCase "Curlies and colons"
            comment1

        , testCase "c"
            comment2

        , testCase "c1+2"
            comment3

        , testCase "c4+2 c3+1 n"
            comment4
        ]

    , testGroup "Fields"
        [ testGroup "Normal"
            [ testCase "f"
                fieldN1

            , testCase "f1+2 n"
                fieldN2

            , testCase "f c1+2"
                fieldN3

            , testCase "f c4+3 n c2+1 n"
                fieldN4

            , testCase "f c n n l"
                fieldN5

            , testCase "f n n c n f"
                fieldN6
            ]

        , testGroup "Curled"
            [ testCase "f"
                fieldC1

            , testCase "f1+2+3{4}"
                fieldC2

            , testCase "f c n { l n c }"
                fieldC3
            ]
        ]

    , testGroup "Section"
        [ testGroup "Normal"
            [ testCase "s"
                sectionN1

            , testCase "s1+2 n"
                sectionN2

            , testCase "s c1+2"
                sectionN3

            , testCase "s c4+3 n c2+1 n"
                sectionN4

            , testCase "s c n n s'"
                sectionN5

            , testCase "s n n c n s"
                sectionN6
            ]

        , testGroup "Curled"
            [ testCase "f"
                sectionC1

            , testCase "f1+2+3{4}"
                sectionC2

            , testCase "f c n { l n c }"
                sectionC3
            ]

        ]
    ]



newline1 :: Assertion
newline1 = "\n" ==> [EmptyN 0, EmptyN 0]

newline2 :: Assertion
newline2 = " \n  " ==> [EmptyN 1, EmptyN 2]

newline3 :: Assertion
newline3 = "    \n  \n   \n \n" ==> [EmptyN 4, EmptyN 2, EmptyN 3, EmptyN 1, EmptyN 0]



comment1 :: Assertion
comment1 = "--c{m:e}t" ==> [CommentN $ Comment 0 0 "c{m:e}t"]

comment2 :: Assertion
comment2 = "--comment" ==> [CommentN $ Comment 0 0 "comment"]

comment3 :: Assertion
comment3 = " --  com   ment" ==> [CommentN $ Comment 1 2 "com   ment"]

comment4 :: Assertion
comment4 =
  "    --  this   \n   -- that\n"
    ==> [ CommentN (Comment 4 2 "this   ")
        , CommentN (Comment 3 1 "that")
        , EmptyN 0
        ]



fieldN1 :: Assertion
fieldN1 =
  "field:" ==> [ Field 0 "field" 0 $
                   NormalF (Contents (EmptyI 0) [])
               ]

fieldN2 :: Assertion
fieldN2 =
  " field  :\n"
    ==> [ Field 1 "field" 2 $
            NormalF (Contents (EmptyI 0) [])
        , EmptyN 0
        ]

fieldN3 :: Assertion
fieldN3 =
  "field: --  comment\n"
    ==> [ Field 0 "field" 0 $
            NormalF (Contents (Inline 1 "--  comment") [])
        , EmptyN 0
        ]

fieldN4 :: Assertion
fieldN4 =
  "field:    --   comment\n  -- comment \n"
    ==> [ Field 0 "field" 0 $
            NormalF (Contents (Inline 4 "--   comment") [])
        , CommentN $ Comment 2 1 "comment "
        , EmptyN 0
        ]

fieldN5 :: Assertion
fieldN5 =
  "field:    --   comment\n  \n \n foo"
    ==> [ Field 0 "field" 0 $
            NormalF $ Contents (Inline 4 "--   comment")
                        [ EmptyL 2
                        , EmptyL 1
                        , Line 1 "foo"
                        ]
        ]

fieldN6 :: Assertion
fieldN6 =
  "field:  \n   \n  -- comment \nfoo:"
    ==> [ Field 0 "field" 0 $
            NormalF (Contents (EmptyI 2) [])
        , EmptyN 3
        , CommentN $ Comment 2 1 "comment "
        , Field 0 "foo" 0 $
            NormalF (Contents (EmptyI 0) [])
        ]


fieldC1 :: Assertion
fieldC1 =
  "field:{}"
    ==> [ Field 0 "field" 0 $
            CurlF [EmptyF 0] (Contents (EmptyI 0) [])
        ]

fieldC2 :: Assertion
fieldC2 =
  " field  :   {    }"
    ==> [ Field 1 "field" 2 $
            CurlF [EmptyF 3] (Contents (EmptyI 4) [])
        ]

fieldC3 :: Assertion
fieldC3 =
  "foo: --  bar   \n {  ba{z: \n  --   th    is \n}"
     ==> [ Field 0 "foo" 0 $
             CurlF
               [ CommentF (Comment 1 2 "bar   ")
               , EmptyF 1
               ]
               $ Contents (Inline 2 "ba{z: ")
                   [ CommentL (Comment 2 3 "th    is ")
                   , EmptyL 0
                   ]
         ]



sectionN1 :: Assertion
sectionN1 =
  "section" ==> [ Section 0 "section" $
                    NormalS (EmptyF 0) []
                ]

sectionN2 :: Assertion
sectionN2 =
  " section  \n"
    ==> [ Section 1 "section" $
            NormalS (EmptyF 2) []
        , EmptyN 0
        ]

sectionN3 :: Assertion
sectionN3 =
  "section --  comment\n"
    ==> [ Section 0 "section" $
            NormalS (CommentF $ Comment 1 2 "comment") []
        , EmptyN 0
        ]

sectionN4 :: Assertion
sectionN4 =
  "section    --   comment\n  -- comment \n"
    ==> [ Section 0 "section" $
            NormalS (CommentF $ Comment 4 3 "comment") []
        , CommentN $ Comment 2 1 "comment "
        , EmptyN 0
        ]

sectionN5 :: Assertion
sectionN5 =
  "section    --   comment\n  \n \n foo"
    ==> [ Section 0 "section" $
            NormalS (CommentF $ Comment 4 3 "comment")
              [ EmptyN 2
              , EmptyN 1
              , Section 1 "foo" $
                  NormalS (EmptyF 0) []
              ]
        ]

sectionN6 :: Assertion
sectionN6 =
  "section  \n   \n  -- comment \nfoo:"
    ==> [ Section 0 "section" $
            NormalS (EmptyF 2) []
        , EmptyN 3
        , CommentN $ Comment 2 1 "comment "
        , Field 0 "foo" 0 $
            NormalF (Contents (EmptyI 0) [])
        ]


sectionC1 :: Assertion
sectionC1 =
  "field:{}" ==> [ Field 0 "field" 0 $
                     CurlF [EmptyF 0] (Contents (EmptyI 0) [])
                 ]

sectionC2 :: Assertion
sectionC2 =
  " field  :   {    }"
    ==> [ Field 1 "field" 2 $
            CurlF [EmptyF 3] (Contents (EmptyI 4) [])
        ]

sectionC3 :: Assertion
sectionC3 =
  "foo: --  bar   \n {  ba{z: \n  --   th    is \n}"
    ==> [ Field 0 "foo" 0 $
            CurlF
              [ CommentF (Comment 1 2 "bar   ")
              , EmptyF 1
              ]
              $ Contents (Inline 2 "ba{z: ")
                  [ CommentL (Comment 2 3 "th    is ")
                  , EmptyL 0
                  ]
        ]
