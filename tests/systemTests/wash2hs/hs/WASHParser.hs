module WASHParser ( xmlfile, washfile ) where {

import Char ;
import Parsec hiding (letter) ;
import WASHData;
import WASHUtil;


notImplemented = char '\xff' >> return undefined 
    <?> "something that isn't implemented yet";

f <$> p = do { x <- p; return $ f x; };

testParser p s = 
    case parse (do { x <- p; eof; return x; }) "bla" s of {
        Left x -> print x;
        Right y -> print y;
    };

washfile :: Parser [CodeFrag] ;
washfile = 
  do code <- hBody
     eof
     return $ code
  ;

setMode :: Bool -> Mode ;
setMode toplevel = if toplevel then S else F ;

-- The numbers given for each parser identify the section and
-- grammar production within the XML 1.0 definition (W3C 
-- REC-xml-19980210).


-- 2.1 / 1
xmlfile :: Parser File;
xmlfile = do { 
    prolog;
    code <- option [] (do {
        hs <- haskell;
        s0;
        return hs
    });
    elem <- element True;
    many misc;
    eof;
    return $ File { fcode = code, topElem = elem };
};


-- 2.2 / 2
char' = (char '\t' <|> char '\n' <|> char '\r' <|> 
    satisfy (>= ' ')) <?> "character";


-- 2.3 / 3
s = (try $ many1 (char ' ' <|> char '\t' <|> 
    char '\r' <|> char '\n')) <?> "whitespace";
s0 = option "" s;
{-
s0 = (try $ many (char ' ' <|> char '\t' <|> 
    char '\r' <|> char '\n')) <?> "optional whitespace";
-}

-- 2.3 / 4
nameChar = letter <|> digit <|> char '.' <|> char '-' <|> 
    char '_' <|> char ':' <|> combiningChar <|> extender;


-- 2.3 / 5
name :: Parser String;
name = do {
    c <- letter <|> char '_' <|> char ':';
    cs <- many nameChar;
    return $ c:cs;
} <?> "name";


-- 2.3 / 6
names :: Parser [String];
names = sepBy1 name s;


-- 2.3 / 7
nmtoken :: Parser String;
nmtoken = many1 nameChar <?> "nmtoken";


-- 2.3 / 8
nmtokens :: Parser [String];
nmtokens = sepBy1 name s;


-- 2.3 / 10
attValue :: Parser AttrValue;
attValue = (((AText . concat) <$> (
        between (char '\"') (char '\"') (many (p '\"')) 
    <|> between (char '\'') (char '\'') (many (p '\'')) ))
    <|> ACode <$> haskellAttr) <?> "attvalue"
where {
    p end = (\x -> [x]) <$> satisfy (f end) <|> reference;
    f end = \c -> c /= '<' && c /= '&' && c /= end;
};

-- 2.3 / 11
systemLiteral = do{
  char '\'';
  sl <- many (satisfy (\c -> c /= '\''));
  char '\'';
  return sl;
} <|> do{
  char '\"';
  sl <- many (satisfy (\c -> c /= '\"'));
  char '\"';
  return sl;
};

-- 2.3 / 12
pubidLiteral = do {
  char '\'';
  sl <- many (pubidChar False);
  char '\'';
  return sl;
} <|> do{
  char '\"';
  sl <- many (pubidChar True);
  char '\"';
  return sl;
};

-- 2.3 / 13
pubidChar w = satisfy (\c -> c >= 'A' && c <= 'Z' 
		          || c >= 'a' && c <= 'z'
			  || c >= '0' && c <= '9'
			  || c `elem` " \n\r-()+,./:=?;!*#@$_%"
			  || w && c == '\'');

-- 2.4 / 14
charData :: Bool -> Parser Text;
charData toplevel =
  do { s <- many1 charData'; return $ Text (setMode toplevel) $ concat s; }
  <?> "#PCDATA";

charData' :: Parser String;
charData' = do {
    c <- satisfy f;
    return [c];
} <|> do {
    string "]]";
    c <- satisfy (\c -> f c && c /= '>');
    return $ ']':']':[c];
}
where { 
    f c = c /= '<' && c /= '&' && c /= ']';
};


-- 2.5 / 15
comment :: Parser String;
comment = do {
    try $ string "<!--";
    comment';
} <?> "comment";

comment' = 
    (do {
        c <- charInComment;
        cs <- comment';
        return $ c:cs; 
    }) <|>
    (do { 
        char '-';
        (do {
            try $ string "->";
            return "";
        }) <|>
        (do {
            c <- charInComment;
            cs <- comment';
            return $ c:cs;
        });
    });

charInComment = 
   (char '\t' <|> char '\n' <|> char '\r' <|> 
   satisfy (\c -> c >= ' ' && c /= '-')) <?> "character";


-- 2.6 / 16
pI = notImplemented >> return "";


-- 2.7 / 18
cdSect = notImplemented >> return "";


-- 2.8 / 22
prolog = do {
    option ("UTF-8", False) xmlDecl;
    many misc;
    option [[]] (docTypeDecl >> many misc);
    return ();
};


-- 2.8 / 23
xmlDecl = do {
  try $ string "<?xml" ;
  versionInfo ;
  enc <- option [] encodingDecl ;
  sdd <- option False sDDecl ;
  s0;
  string "?>" ;
  return (enc, sdd)
};

-- 2.7 / 24
versionInfo = do {
  s ;
  string "version";
  eq ;
  ( do {char '\"'; versionNum; char '\"' } <|>
    do {char '\''; versionNum; char '\'' } );
};

-- 2.8 / 25
eq = do { s0; char '='; s0; };

-- 2.8 / 26
versionNum = string "1.0" ;

-- 2.8 / 27
misc = comment <|> pI <|> s;


-- 2.8 / 28
-- [28] doctypedecl ::=
--        '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
docTypeDecl = do{
  try $ string "<!DOCTYPE";
  s;
  name;
  option [] (do {s; externalID;});
  s0;
  option [] (do {char '[';intSubset; char ']'; s0; });
  char '>';
};

-- 2.8 / 28b
-- [28b]    intSubset    ::=    (markupdecl | DeclSep)*
intSubset = notImplemented;

-- 2.9 / 32
sDDecl = do {
  s;
  string "standalone";
  eq;
  ( do {char '\"'; x <- yesNo; char '\"'; return x; } <|>
    do {char '\''; x <- yesNo; char '\''; return x; } ) ;
};

yesNo = do {string "yes"; return True;} <|> do {string "no" ; return False;};

-- 3 / 39, 3.1 / 40, 3.1 / 42, 3.1 / 44
element :: Bool -> Parser Element;
element toplevel = do {
    name <- try $ do { char '<'; name };
    attrs <- attributes False;
    (do {
        char '>';
        content <- content False;
        try $ do { string "</"; string name; s0; char '>'; };
        return $ Element (setMode toplevel) name attrs content False;
    }) <|>
    (do {
        try $ string "/>";
        return $ Element (setMode toplevel) name attrs [] True;
    })
} <|> do {
    try $ do { string "<%@" };
    s;
    string "include";
    s;
    AText filename <- attValue;
    s;
    subs <- substitutions;
    string "%>";
    let { str = openFile filename; } ;
    return $ 
      case parse xmlfile filename str of {
	Left err ->
	  Element (setMode toplevel) "include-failed" 
		  [Attribute (setMode toplevel) "file" (AText filename)]
		  [CText (Text (setMode toplevel) (show err))]
		  True;
	Right file ->
	  topElem file;
	}
} <?> "element";

attributes :: Bool -> Parser [Attribute];
attributes toplevel = do { s; attributes' toplevel; } <|> return [];

attributes' :: Bool -> Parser [Attribute];
attributes' toplevel = do { a <- attribute toplevel;
			    as <- attributes toplevel;
			    return (a:as);
			  }
		       <|> return [];

-- 3.1 / 41
attribute :: Bool -> Parser Attribute;
attribute toplevel = try (
  do { string "<%" ;
       pat <- hCode ;
       string "%>" ;
       return $ AttrPattern pat ;
    }
<|> 
  do {
    name <- name;
    eq;
    value <- attValue;
    return $ Attribute (setMode toplevel) name value;
  }
) <?> "attribute";


-- 3.1 / 43
content :: Bool -> Parser [Content];
content toplevel = many (
        (element toplevel  >>= (return . CElement))
    <|> (charData toplevel  >>= (return . CText))
    <|> (haskellText>>= (return . CCode))
    <|> (haskell   >>= (return . CCode))
    <|> (reference >>= (return . CReference . Text (setMode toplevel)))
    <|> (cdSect    >>  (return undefined))
    <|> (pI        >>= (return . CPI))
    <|> (comment   >>= (return . CComment))
);


-- 4.1 / 66, 4.1 / 68
reference = do {
    char '&';
    r <- (do { 
        char '#';
        r <- many1 digit <|> 
            do { char 'x'; r <- many1 hexDigit; return $ 'x':r; };
        return $ '#':r;
    }) <|> name;
    char ';';
    return $ "&" ++ r ++ ";";
} <?> "reference";

-- 4.2.2 / 75
-- [75]    ExternalID    ::=    'SYSTEM' S SystemLiteral
--                            | 'PUBLIC' S PubidLiteral S SystemLiteral
externalID = do{
  string "SYSTEM";
  s;
  systemLiteral;
} <|>
do {
  string "PUBLIC";
  s;
  pubidLiteral;
  s;
  systemLiteral;
};

-- 4.3 / 80
encodingDecl = do {
  s;
  string "encoding";
  eq;
  ( do {char '\"'; x <- encName; char '\"'; return x;} <|>
    do {char '\''; x <- encName; char '\''; return x;});
};

-- 4.3 / 81
-- [81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')*
encName = do {
  c <- satisfy (\c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') ;
  cs <- many (satisfy (\c -> c >= 'A' && c <= 'Z' 
		          || c >= 'a' && c <= 'z'
			  || c >= '0' && c <= '9'
			  || c `elem` "._-"));
  return (c:cs)
};

-- B / 84
letter = baseChar <|> ideographic;

-- B / 85
baseChar = 
    satisfy (\c -> 
        (c >= '\x41' && c <= '\x5a') || 
        (c >= '\x61' && c <= '\x7a') || 
        (c >= '\xc0' && c <= '\xd6') || 
        (c >= '\xd8' && c <= '\xf6') || 
        (c >= '\xf8' && c <= '\xff')); -- and some Unicode characters

-- B / 86
ideographic = notImplemented;

-- B / 87
combiningChar = notImplemented;

-- B / 88
--digit = digit; -- and some Unicode characters

-- B / 89
extender = char '\xb7'; -- and some Unicode characters


haskell :: Parser [CodeFrag];
haskell = do {
    try $ string "<%";
    frags <- hStmt <|> hBody;
    try $ string "%>";
    return frags;
};

hIdentChar = letter <|> digit <|> char '\'' <|> char '_';

hStmt :: Parser [CodeFrag];
hStmt = do {
    var <- try $ do { s0 ;
		      v0 <- letter ;
		      vr <- many hIdentChar ;
		      s0 ;
		      string "<-" ;
		      return (v0:vr)
		    };
    body <- hBody ;
    return (VFrag var : body)
};

hBody :: Parser [CodeFrag];
hBody = many (
            (hCode >>= (return . HFrag))
        <|> (element True >>= (return . EFrag))
        <|> (nakedAttributes >>= (return . AFrag))
        <|> (nakedContent >>= (return . CFrag))
    );

nakedAttributes = do {
    s0 ;
    try $ string "<[" ;
    s0 ;
    attrs <- attributes' True ;
    try $ string "]>" ;
    return attrs;
};

nakedContent = do {
    try $ string "<#>" ;
    cnts <- content True ;
    try $ string "</#>";
    return cnts;
};

haskellAttr :: Parser String;
haskellAttr = haskellUnnested "<%";

haskellText :: Parser [CodeFrag];
haskellText = do {
  str <- haskellUnnested "<%=";
  return [HFrag "text(", HFrag str, HFrag ")"]
};

haskellUnnested :: String -> Parser String;
haskellUnnested start = do {
    try $ string start;
    code <- hCode;
    try $ string "%>";
    return code;
} <?> "haskell code";

hCode = try $ do { sp <- getPosition;
		   str <- hcode' '%' (not . (`elem` "#[%"));
		   return (reindent (sourceColumn sp - 1) str);
		 };
hPatt = try $ hcode' '#' (const True);

reindent :: Int -> String -> String;
reindent 0 str = '\n':str;
reindent n str = reindent (n-1) (' ':str);

hSinglePatt = try $ do {
    s0 ;
    char '[' ;
    id <- name ;
    char ']' ;
    s0 ;
    return id ;
} ;
    

hcode' escChar patC = 
  let stopChars = escChar : "<\"" in
  do { 
    t <- many1 $ (  hcNorm stopChars
                <|> hcString
                <|> hcIsTag patC
                <|> hcIsEnd escChar ) ;
    return $ concat t
};

hcNorm stopChars = try . many1 $ noneOf stopChars;

hcString = try $ do {
    char '\"' ;
    strs <- many ((many1 $ noneOf "\"\\")
              <|> (char '\\' >> (
		   (do x <- satisfy (not . isSpace)
		       return ['\\',x])
 	       <|> (do xs <- many1 $ satisfy isSpace
		       char '\\'
		       return ('\\' : xs ++ "\\"))))) ;
    char '\"' ;
    return ('\"' : concat strs ++ "\"")
};

hcIsTag patC = try $ do { 
    char '<' ;
    t <- satisfy (\c -> (not.isAlpha $ c) && patC c) ;
    return ('<':t:[]) ;
};

hcIsEnd escChar = try $ do { 
    char escChar ;
    t <- satisfy (/='>') ;
    return (escChar:t:[]) ;
};

-- experimental
substitutions = return [];

}
