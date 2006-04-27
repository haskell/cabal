module AlbumDTD where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Album = Album Title Artist (Maybe Recording) Coverart
		   [Catalogno] Personnel [Track] Notes
	   deriving (Eq,Show)
newtype Title = Title String 		deriving (Eq,Show)
newtype Artist = Artist String 		deriving (Eq,Show)
data Recording = Recording
    { recordingDate :: (Maybe String)
    , recordingPlace :: (Maybe String)
    } deriving (Eq,Show)
data Coverart = Coverart Coverart_Attrs (Maybe Location)
	      deriving (Eq,Show)
data Coverart_Attrs = Coverart_Attrs
    { coverartStyle :: String
    } deriving (Eq,Show)
data Location = Location
    { locationThumbnail :: (Maybe String)
    , locationFullsize :: (Maybe String)
    } deriving (Eq,Show)
data Catalogno = Catalogno
    { catalognoLabel :: String
    , catalognoNumber :: String
    , catalognoFormat :: (Maybe Catalogno_Format)
    , catalognoReleasedate :: (Maybe String)
    , catalognoCountry :: (Maybe String)
    } deriving (Eq,Show)
data Catalogno_Format = Catalogno_Format_CD  |  Catalogno_Format_LP
			 |  Catalogno_Format_MiniDisc
		      deriving (Eq,Show)
newtype Personnel = Personnel [Player] 		deriving (Eq,Show)
data Player = Player
    { playerName :: String
    , playerInstrument :: String
    } deriving (Eq,Show)
data Track = Track
    { trackTitle :: String
    , trackCredit :: (Maybe String)
    , trackTiming :: (Maybe String)
    } deriving (Eq,Show)
data Notes = Notes Notes_Attrs [Notes_]
	   deriving (Eq,Show)
data Notes_Attrs = Notes_Attrs
    { notesAuthor :: (Maybe String)
    } deriving (Eq,Show)
data Notes_ = Notes_Str String
	    | Notes_Albumref Albumref
	    | Notes_Trackref Trackref
	    deriving (Eq,Show)
data Albumref = Albumref Albumref_Attrs String
	      deriving (Eq,Show)
data Albumref_Attrs = Albumref_Attrs
    { albumrefLink :: String
    } deriving (Eq,Show)
data Trackref = Trackref Trackref_Attrs String
	      deriving (Eq,Show)
data Trackref_Attrs = Trackref_Attrs
    { trackrefLink :: (Maybe String)
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Album where
    fromElem (CElem (Elem "album" [] c0):rest) =
	(\(a,ca)->
	   (\(b,cb)->
	      (\(c,cc)->
		 (\(d,cd)->
		    (\(e,ce)->
		       (\(f,cf)->
			  (\(g,cg)->
			     (\(h,ch)->
				(Just (Album a b c d e f g h), rest))
			     (definite fromElem "<notes>" "album" cg))
			  (many fromElem cf))
		       (definite fromElem "<personnel>" "album" ce))
		    (many fromElem cd))
		 (definite fromElem "<coverart>" "album" cc))
	      (fromElem cb))
	   (definite fromElem "<artist>" "album" ca))
	(definite fromElem "<title>" "album" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Album a b c d e f g h) =
	[CElem (Elem "album" [] (toElem a ++ toElem b ++ maybe [] toElem c
				 ++ toElem d ++ concatMap toElem e ++ toElem f ++ concatMap toElem g
				 ++ toElem h))]
instance XmlContent Title where
    fromElem (CElem (Elem "title" [] c0):rest) =
	(\(a,ca)->
	   (Just (Title a), rest))
	(definite fromText "text" "title" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Title a) =
	[CElem (Elem "title" [] (toText a))]
instance XmlContent Artist where
    fromElem (CElem (Elem "artist" [] c0):rest) =
	(\(a,ca)->
	   (Just (Artist a), rest))
	(definite fromText "text" "artist" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Artist a) =
	[CElem (Elem "artist" [] (toText a))]
instance XmlContent Recording where
    fromElem (CElem (Elem "recording" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "recording" (toAttrs as) [])]
instance XmlAttributes Recording where
    fromAttrs as =
	Recording
	  { recordingDate = possibleA fromAttrToStr "date" as
	  , recordingPlace = possibleA fromAttrToStr "place" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "date" (recordingDate v)
	, maybeToAttr toAttrFrStr "place" (recordingPlace v)
	]
instance XmlContent Coverart where
    fromElem (CElem (Elem "coverart" as c0):rest) =
	(\(a,ca)->
	   (Just (Coverart (fromAttrs as) a), rest))
	(fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Coverart as a) =
	[CElem (Elem "coverart" (toAttrs as) (maybe [] toElem a))]
instance XmlAttributes Coverart_Attrs where
    fromAttrs as =
	Coverart_Attrs
	  { coverartStyle = definiteA fromAttrToStr "coverart" "style" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "style" (coverartStyle v)
	]
instance XmlContent Location where
    fromElem (CElem (Elem "location" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "location" (toAttrs as) [])]
instance XmlAttributes Location where
    fromAttrs as =
	Location
	  { locationThumbnail = possibleA fromAttrToStr "thumbnail" as
	  , locationFullsize = possibleA fromAttrToStr "fullsize" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "thumbnail" (locationThumbnail v)
	, maybeToAttr toAttrFrStr "fullsize" (locationFullsize v)
	]
instance XmlContent Catalogno where
    fromElem (CElem (Elem "catalogno" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "catalogno" (toAttrs as) [])]
instance XmlAttributes Catalogno where
    fromAttrs as =
	Catalogno
	  { catalognoLabel = definiteA fromAttrToStr "catalogno" "label" as
	  , catalognoNumber = definiteA fromAttrToStr "catalogno" "number" as
	  , catalognoFormat = possibleA fromAttrToTyp "format" as
	  , catalognoReleasedate = possibleA fromAttrToStr "releasedate" as
	  , catalognoCountry = possibleA fromAttrToStr "country" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "label" (catalognoLabel v)
	, toAttrFrStr "number" (catalognoNumber v)
	, maybeToAttr toAttrFrTyp "format" (catalognoFormat v)
	, maybeToAttr toAttrFrStr "releasedate" (catalognoReleasedate v)
	, maybeToAttr toAttrFrStr "country" (catalognoCountry v)
	]
instance XmlAttrType Catalogno_Format where
    fromAttrToTyp n (n',v)
	| n==n'     = translate (attr2str v)
	| otherwise = Nothing
      where translate "CD" = Just Catalogno_Format_CD
	    translate "LP" = Just Catalogno_Format_LP
	    translate "MiniDisc" = Just Catalogno_Format_MiniDisc
	    translate _ = Nothing
    toAttrFrTyp n Catalogno_Format_CD = Just (n, str2attr "CD")
    toAttrFrTyp n Catalogno_Format_LP = Just (n, str2attr "LP")
    toAttrFrTyp n Catalogno_Format_MiniDisc = Just (n, str2attr "MiniDisc")
instance XmlContent Personnel where
    fromElem (CElem (Elem "personnel" [] c0):rest) =
	(\(a,ca)->
	   (Just (Personnel a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Personnel a) =
	[CElem (Elem "personnel" [] (concatMap toElem a))]
instance XmlContent Player where
    fromElem (CElem (Elem "player" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "player" (toAttrs as) [])]
instance XmlAttributes Player where
    fromAttrs as =
	Player
	  { playerName = definiteA fromAttrToStr "player" "name" as
	  , playerInstrument = definiteA fromAttrToStr "player" "instrument" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "name" (playerName v)
	, toAttrFrStr "instrument" (playerInstrument v)
	]
instance XmlContent Track where
    fromElem (CElem (Elem "track" as []):rest) =
	(Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
	[CElem (Elem "track" (toAttrs as) [])]
instance XmlAttributes Track where
    fromAttrs as =
	Track
	  { trackTitle = definiteA fromAttrToStr "track" "title" as
	  , trackCredit = possibleA fromAttrToStr "credit" as
	  , trackTiming = possibleA fromAttrToStr "timing" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "title" (trackTitle v)
	, maybeToAttr toAttrFrStr "credit" (trackCredit v)
	, maybeToAttr toAttrFrStr "timing" (trackTiming v)
	]
instance XmlContent Notes where
    fromElem (CElem (Elem "notes" as c0):rest) =
	(\(a,ca)->
	   (Just (Notes (fromAttrs as) a), rest))
	(many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Notes as a) =
	[CElem (Elem "notes" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Notes_Attrs where
    fromAttrs as =
	Notes_Attrs
	  { notesAuthor = possibleA fromAttrToStr "author" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "author" (notesAuthor v)
	]
instance XmlContent Notes_ where
    fromElem c0 =
	case (fromText c0) of
	(Just a,rest) -> (Just (Notes_Str a), rest)
	(Nothing,_) ->
		case (fromElem c0) of
		(Just a,rest) -> (Just (Notes_Albumref a), rest)
		(Nothing,_) ->
			case (fromElem c0) of
			(Just a,rest) -> (Just (Notes_Trackref a), rest)
			(Nothing,_) ->
			    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Notes_Str a) = toText a
    toElem (Notes_Albumref a) = toElem a
    toElem (Notes_Trackref a) = toElem a
instance XmlContent Albumref where
    fromElem (CElem (Elem "albumref" as c0):rest) =
	(\(a,ca)->
	   (Just (Albumref (fromAttrs as) a), rest))
	(definite fromText "text" "albumref" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Albumref as a) =
	[CElem (Elem "albumref" (toAttrs as) (toText a))]
instance XmlAttributes Albumref_Attrs where
    fromAttrs as =
	Albumref_Attrs
	  { albumrefLink = definiteA fromAttrToStr "albumref" "link" as
	  }
    toAttrs v = catMaybes 
	[ toAttrFrStr "link" (albumrefLink v)
	]
instance XmlContent Trackref where
    fromElem (CElem (Elem "trackref" as c0):rest) =
	(\(a,ca)->
	   (Just (Trackref (fromAttrs as) a), rest))
	(definite fromText "text" "trackref" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Trackref as a) =
	[CElem (Elem "trackref" (toAttrs as) (toText a))]
instance XmlAttributes Trackref_Attrs where
    fromAttrs as =
	Trackref_Attrs
	  { trackrefLink = possibleA fromAttrToStr "link" as
	  }
    toAttrs v = catMaybes 
	[ maybeToAttr toAttrFrStr "link" (trackrefLink v)
	]


{-Done-}
