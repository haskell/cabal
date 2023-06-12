import Data.Map (Map)
import qualified Data.Map as Map
import Foo

main = print $ f (+ 1) (Map.fromList [(0, 1), (2, 3)] :: Map Int Int)
