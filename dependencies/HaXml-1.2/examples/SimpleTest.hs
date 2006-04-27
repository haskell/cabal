module Main where

import List (isPrefixOf)
import Text.XML.HaXml.Haskell2Xml
import Text.XML.HaXml.Types
import Text.PrettyPrint.HughesPJ (render)
import Text.XML.HaXml.Pretty     (document)

-- Test stuff
value1 :: ([(Bool,Int)],(String,Maybe Char))
value1 = ([(True,42),(False,0)],("Hello World",Just 'x'))

data MyType a = ConsA Int a
              | ConsB String deriving Eq
              {-! derive : Haskell2Xml !-}


instance Haskell2Xml a => Haskell2Xml (MyType a) where
    toHType v = Defined "MyType" [toHType a]
                    [Constr "ConsA" [toHType a] [Prim "Int" "int", toHType a]
                    ,Constr "ConsB" [] [String]
                    ]
              where (ConsA _ a) = v
    toContents v@(ConsA n a) = [mkElemC (showConstr 0 (toHType v))
                                  (concat [toContents n, toContents a])]
    toContents v@(ConsB s) = [mkElemC (showConstr 1 (toHType v)) (toContents s)]
    fromContents (CElem (Elem constr [] cs) : etc)
      | "ConsA-" `isPrefixOf` constr =
        (\(i,cs')-> (\(a,_) -> (ConsA i a,etc))
          (fromContents cs')) (fromContents cs)
      | "ConsB" `isPrefixOf` constr =
        (\(s,_)-> (ConsB s, etc)) (fromContents cs)


value2 :: (MyType [Int], MyType ())
value2  = (ConsA 2 [42,0], ConsB "hello world")

--main = do (putStrLn . render . document . toXml) value2

main = putStrLn
         (if value2 == (fst . fromContents . toContents) value2 then "success"
          else "failure")
        
