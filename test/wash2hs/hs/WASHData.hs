module WASHData						    -- derived from HSPData
    ( File (..)
    , Mode (..)
    , Element (..)
    , Text (..)
    , Content (..)
    , CodeFrag (..)
    , Attribute (..)
    , AttrValue (..)
    )
where {


-- Data type.

data File = File { 
    fcode :: [CodeFrag],
    topElem :: Element
    } deriving Show;

data Mode = V | S | F 
  deriving (Eq,Show);

data Element = Element 
    { elemMode :: Mode
    , elemName :: String
    , elemAttrs :: [Attribute]
    , elemContent :: [Content]
    , elemEmptyTag :: Bool }
    deriving Show;

data Text = Text 
    { textMode :: Mode
    , textString :: String
    }
    deriving Show;

data Content 
    = CElement { celem :: Element }
    | CText { ctext :: Text }
    | CReference { creference :: Text }
    | CPI { cpi :: String }
    | CComment { ccomment :: String }
    | CCode { ccode :: [CodeFrag] }
    deriving Show;
    
data CodeFrag 
    = HFrag String
    | EFrag Element
    | HSFrag String
    | CFrag [Content]
    | AFrag [Attribute]
    | VFrag String
    deriving Show;

data Attribute
  = Attribute
    { attrMode :: Mode
    , attrName :: String
    , attrValue :: AttrValue }
  | AttrPattern
    { attrPattern :: String }
    deriving Show;

data AttrValue
    = AText String
    | ACode String
    deriving Show;

data Reference = Reference String deriving Show;
    


}
