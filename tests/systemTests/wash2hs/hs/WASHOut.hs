module WASHOut where

-- output monad

data Out a = Out a ShowS

instance Monad Out where
  return a = Out a id
  m >>= f  = case m of
	       Out x shw1 ->
		 case f x of
                   Out y shw2 ->
		     Out y (shw1 . shw2)

runOut :: Out a -> ShowS
runOut (Out a shw) = shw

wrapper = (Out () .)

outString :: String -> Out ()
outString = wrapper showString

outChar :: Char -> Out ()
outChar = wrapper showChar

outs :: Show a => a -> Out ()
outs = wrapper shows

outShowS :: ShowS -> Out ()
outShowS = Out ()
