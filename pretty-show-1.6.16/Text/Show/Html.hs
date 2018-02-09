{-# LANGUAGE Safe #-}
module Text.Show.Html
  ( HtmlOpts(..), defaultHtmlOpts
  , valToHtml, valToHtmlPage, htmlPage
  , Html(..)
  ) where

import Text.Show.Value
import Prelude hiding (span)

-- | Make an Html page representing the given value.
valToHtmlPage :: HtmlOpts -> Value -> String
valToHtmlPage opts = htmlPage opts . valToHtml opts

-- | Options on how to generate Html (more to come).
data HtmlOpts = HtmlOpts
  { dataDir :: FilePath   -- ^ Path for extra files.  If empty, we look in
                          -- directory `style`, relative to document.
  , wideListWidth :: Int  -- ^ Max. number of columns in wide lists.
  } deriving Show

-- | Default options.
defaultHtmlOpts :: HtmlOpts
defaultHtmlOpts = HtmlOpts
  { dataDir       = ""
  , wideListWidth = 80
  }

-- | Convert a value into an Html fragment.
valToHtml :: HtmlOpts -> Value -> Html
valToHtml opts = loop
  where
  loop val =
    case val of
      Con con []  -> span "con" (text con)
      Con con vs  -> tallRecord con (map conLab vs) (map loop         vs)
      Rec con fs  -> tallRecord con (map fst fs)    (map (loop . snd) fs)
      Tuple vs    -> wideTuple                      (map loop vs)

      InfixCons v ms ->
        table "infix tallRecord"
          [ tr $ (th "label" 1 (text " ") :)
               $ map td $ loop v : [ h | (op,u) <- ms
                                   , h <- [ text op, loop u ]
                                   ]
          ]

      List []     -> span "list" (text "[]")
      List vs@(v : vs1) ->
        case v of

          Con c fs
            | all (isCon c) vs1  -> recordList c (map conLab fs)
                                     [ map loop xs | Con _ xs <- vs ]
            | otherwise          -> tallList $ map (loop) vs

          Rec c fs
            | all (isRec c) vs1   -> recordList c (map fst fs)
                                   [ map (loop . snd) xs | Rec _ xs <- vs ]
            | otherwise           -> tallList $ map (loop) vs

          Tuple fs -> tupleList (length fs)
                            [ map (loop) xs | Tuple xs <- vs ]

          List {}    -> tallList    $ map loop vs

          Neg {}     -> wideList (wideListWidth opts) $ map loop vs
          Ratio {}   -> wideList (wideListWidth opts) $ map loop vs
          Integer {} -> wideList (wideListWidth opts) $ map loop vs
          Float {}   -> wideList (wideListWidth opts) $ map loop vs
          Char {}    -> wideList (wideListWidth opts) $ map loop vs
          String {}  -> tallList                      $ map loop vs
          InfixCons {} -> tallList                    $ map loop vs

      Neg v       ->
        case v of
          Integer txt -> span "integer" $ text ('-' : txt)
          Float txt   -> span "float"   $ text ('-' : txt)
          _           -> neg (loop v)

      Ratio v1 v2 -> ratio (loop v1) (loop v2)
      Integer txt -> span "integer" (text txt)
      Float txt   -> span "float"   (text txt)
      Char txt    -> span "char"    (text txt)
      String txt  -> span "string"  (text txt)

  conLab _          = " "

  isCon c (Con d _) = c == d
  isCon _ _         = False

  isRec c (Rec d _) = c == d
  isRec _ _         = False


neg :: Html -> Html
neg e = table "negate" [ tr [td (text "-"), td e] ]

ratio :: Html -> Html -> Html
ratio e1 e2 = table "ratio" [ tr [ td' "numerator" e1 ], tr [td e2] ]

wideTuple :: [Html] -> Html
wideTuple els = table "wideTuple" [ tr $ map td els ]

tallTuple :: [Html] -> Html
tallTuple els = table "tallTuple" $ map (tr . return . td) els

tallRecord :: Name -> [Name] -> [Html] -> Html
tallRecord con labs els = table "tallRecord" $ topHs : zipWith row labs els
  where
  topHs   = tr [ th "con" 2 (text con) ]
  row l e = tr [ th "label" 1 (text l),   td e ]

recordList :: Name -> [Name] -> [[Html]] -> Html
recordList con labs els = table "recordList" $ topHs : zipWith row [0..] els
  where
  topHs    = tr $ th "con" 1 (text con) : map (th "label" 1 . text) labs
  row n es = tr $ th "ix" 1 (int n) : map td es

tupleList :: Int -> [[Html]] -> Html
tupleList n els = recordList " " (replicate n " ") els

tallList :: [Html] -> Html
tallList els = table "tallList" $ top : zipWith row [0..] els
  where
  top     = tr [ th "con" 2 (text " ")]
  row n e = tr [ th "ix" 1 (int n), td e ]

wideList :: Int -> [Html] -> Html
wideList w els = table "wideList" $ topHs : zipWith row [0..] (chop els)
  where
  elNum = length els
  pad   = elNum > w

  chop [] = []
  chop xs = let (as,bs) = splitAt w xs
            in take w (as ++ if pad then repeat empty else []) : chop bs

  topHs     = tr $ th "con" 1 (text " ") : map (th "label" 1 . int)
                                                [ 0 .. min elNum w - 1 ]
  row n es  = tr $ (th "ix" 1 (int (n*w))) : map td es

--------------------------------------------------------------------------------
newtype Html = Html { exportHtml :: String }

table :: String -> [Html] -> Html
table cl body = Html $ "<table class=" ++ show cl ++ ">" ++
                       concatMap exportHtml body ++
                       "</table>"

tr :: [Html] -> Html
tr body = Html $ "<tr>" ++ concatMap exportHtml body ++ "</tr>"

th :: String -> Int -> Html -> Html
th cl n body = Html $ "<th class=" ++ show cl ++
                         " colspan=" ++ show (show n) ++ ">" ++
                      exportHtml body ++
                      "</th>"

td :: Html -> Html
td body = Html $ "<td>" ++ exportHtml body ++ "</td>"

td' :: String -> Html -> Html
td' cl body = Html $ "<td class=" ++ show cl ++ ">" ++
                     exportHtml body ++
                     "</td>"

span :: String -> Html -> Html
span cl body = Html $ "<span class=" ++ show cl ++ ">" ++
                      exportHtml body ++
                      "</span>"

empty :: Html
empty = Html  ""

int :: Int -> Html
int = Html . show

text :: String -> Html
text = Html . concatMap esc
  where
  esc '<' = "&lt;"
  esc '>' = "&gt;"
  esc '&' = "&amp;"
  esc ' ' = "&nbsp;"
  esc c   = [c]

-- | Wrap an Html fragment to make an Html page.
htmlPage :: HtmlOpts -> Html -> String
htmlPage opts body =
  unlines
  [ "<html>"
  , "<head>"
  , "<link href="  ++ show pstyle ++ " rel=" ++ show "stylesheet" ++ ">"
  , "<script src=" ++ show jquery ++ "></script>"
  , "<script src=" ++ show pjs    ++ "></script>"
  , "<body>"
  , exportHtml  body
  , "</body>"
  , "</html>"
  ]
  where
  -- XXX: slashes on Windows?
  dir    = case dataDir opts of
             "" -> ""
             d  -> d ++ "/"
  jquery = dir ++ "style/jquery.js"
  pjs    = dir ++ "style/pretty-show.js"
  pstyle = dir ++ "style/pretty-show.css"


