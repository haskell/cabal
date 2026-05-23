{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax (ForeignSrcLang (LangC), addForeignSource)

$( do
    addForeignSource LangC "int test_one_c() { return ONE; }"
    pure []
 )

main :: IO ()
main = pure ()
