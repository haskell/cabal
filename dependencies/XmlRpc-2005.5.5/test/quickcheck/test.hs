import Network.XmlRpc.Internals

import Control.Monad
import Control.Monad.Identity
import Data.Char
import Data.List
import Data.Maybe
import System.Time

import Debug.QuickCheck

{-
xmlRpcId1 :: (XmlRpcType a, XmlRpcType b, Eq b) => (a -> b) -> a -> Bool
xmlRpcId1 f x = f x == f x'
    where MethodCall _ [x'] $ parseCall $ renderCall $ MethodCall "test" [toValue x]
-}

liftArb :: Arbitrary a => (a -> b) -> Gen b
liftArb f = liftM f arbitrary

liftArb2 :: (Arbitrary a,Arbitrary b) => (a -> b -> c) -> Gen c
liftArb2 f = liftM2 f arbitrary arbitrary

arbitraryList :: Gen a -> Int -> Gen [a]
arbitraryList g 0 = liftM (:[]) g
arbitraryList g n = oneof [liftM (:[]) g, liftM2 (:) g (arbitraryList g (n-1))]

instance Arbitrary Month where
    arbitrary = oneof (map return months)
    coarbitrary m = variant (fromJust (findIndex (==m) months))

months = [January, February, March, 
	  April, May, June,
	  July, August, September, 
	  October, November, December]

instance Arbitrary CalendarTime where
    arbitrary = do
		y <- arbitrary
		mo <- arbitrary
		d <- arbitrary
		h <- arbitrary
		mi <- arbitrary
		s <- arbitrary
		p <- arbitrary
		return CalendarTime {
				     ctYear = abs y,
				     ctMonth = mo,
				     ctDay = abs d,
				     ctHour = abs h,
				     ctMin = abs mi,
				     ctSec = abs s,
				     ctPicosec = abs p,
				     -- don't care about these:
				     ctWDay = Monday,
				     ctYDay = 0,
				     ctTZName = "UTC",
				     ctTZ = 0,
				     ctIsDST = False
				    }



arbitraryId :: Gen String
arbitraryId = sized (arbitraryList (elements nameChars))
    where nameChars = ['a'..'z'] ++ ['0'..'9'] ++ "_.:/"

arbitraryPrintableString :: Gen String
arbitraryPrintableString = sized (arbitraryList (elements printableChars))
    where printableChars = filter (not . isControl) ['\0'..'\255']

arbitraryString :: Gen String
arbitraryString = sized (arbitraryList (elements ['\0'..'\255']))


instance Arbitrary MethodCall where
    arbitrary = liftM2 MethodCall arbitraryId arbitrary

instance Arbitrary MethodResponse where
    arbitrary = oneof [
		       liftArb Return
		      , liftM2 Fault arbitrary arbitraryPrintableString
		      ]

instance Arbitrary Value where
    arbitrary = sized arbValue
	where arbValue n = 
 		  oneof [
			 liftArb ValueInt
			, liftArb ValueBool
			, liftM ValueString arbitraryPrintableString
			 -- exposes bugs
			 --	      , liftM ValueString arbitraryString
			, liftArb ValueDouble
			 -- exposes bugs
			 --	      , liftArb ValueDateTime
			, liftM ValueBase64 arbitraryString
			, liftM ValueStruct 
	                 (arbitraryList (liftM2 (,) 
					 arbitraryId (arbValue (n-1)))
			                (n-1)
			 )
			, liftM ValueArray (arbitraryList (arbValue (n-1)) (n-1))
			]

testErr :: Err Identity Bool -> Bool
testErr m = runIdentity (handleError (\_ -> return False) m)

prop_CallParseRender c = 
    testErr $ liftM (==c) (parseCall (renderCall c))

prop_ResponseParseRender r = 
    testErr $ liftM (==r) (parseResponse (renderResponse r))
