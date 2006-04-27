{- |
   Maintainer  :  simons@cryp.to
   Stability   :  experimental
   Portability :  portable

   The preferred method for rendering a 'Document' or single 'Content'
   is by using the pretty printing facility defined in "Pretty".
   Pretty-printing does not work well for cases, however, where the
   formatting in the XML document is significant. Examples of this
   case are XHTML's @\<pre\>@ tag, Docbook's @\<literallayout\>@ tag,
   and many more.

   Theoretically, the document author could avoid this problem by
   wrapping the contents of these tags in a \<![CDATA[...]]\> section,
   but often this is not practical, for instance when the
   literal-layout section contains other elements. Finally, program
   writers could manually format these elements by transforming them
   into a 'literal' string in their 'CFliter', etc., but this is
   annoying to do and prone to omissions and formatting errors.

   As an alternative, this module provides the function 'verbatim',
   which will format XML 'Content' as a 'String' while retaining the
   formatting of the input document unchanged.

   /Know problems/:

    * HaXml's parser eats line feeds between two tags.

    * 'Attribute's should be formatted by making them an instance of
      'Verbatim' as well, but since an 'Attribute' is just a tuple,
      not a full data type, the helper function 'verbAttr' must be
      used instead.

    * 'CMisc' is not yet supported.

    * 'Element's, which contain no content, are formatted as
       @\<element-name\/\>@, even if they were not defined as being of
       type @EMPTY@. In XML this perfectly alright, but in SGML it is
       not. Those, who wish to use 'verbatim' to format parts of say
       an HTML page will have to (a) replace problematic elements by
       'literal's /before/ running 'verbatim' or (b) use a second
       search-and-replace stage to fix this.
 -}

module Text.XML.HaXml.Verbatim where

import Text.XML.HaXml.Types

-- |This class promises that the function 'verbatim' knows how to
-- format this data type into a string without changing the
-- formatting.

class Verbatim a where
    verbatim :: a -> String

instance (Verbatim a) => Verbatim [a] where
    verbatim  = concat . (map verbatim)

instance Verbatim Char where
    verbatim c = [c]

instance (Verbatim a, Verbatim b) => Verbatim (Either a b) where
    verbatim (Left v)  = verbatim v
    verbatim (Right v) = verbatim v

instance Verbatim Content where
    verbatim (CElem c)     = verbatim c
    verbatim (CString _ c) = c
    verbatim (CRef c)      = verbatim c
    verbatim (CMisc _)     = error "NYI: verbatim not defined for CMisc"

instance Verbatim Element where
    verbatim (Elem nam att [])   = "<" ++ nam ++ (concat . (map verbAttr)) att
                                   ++ "/>"
    verbatim (Elem nam att cont) = "<" ++ nam ++ (concat . (map verbAttr)) att
                                   ++ ">" ++ verbatim cont ++ "</" ++ nam ++ ">"

instance Verbatim Reference where
    verbatim (RefEntity r) = "&" ++ verbatim r ++ ";"
    verbatim (RefChar c)   = "&#" ++ show c ++ ";"


-- |This is a helper function is required because Haskell does not
-- allow to make an ordinary tuple (like 'Attribute') an instance of a
-- class. The resulting output will preface the actual attribute with
-- a single blank so that lists of 'Attribute's can be handled
-- implicitly by the definition for lists of 'Verbatim' data types.

verbAttr :: Attribute -> String
verbAttr (n, AttValue v) = " " ++ n ++ "=\"" ++ verbatim v ++ "\""

