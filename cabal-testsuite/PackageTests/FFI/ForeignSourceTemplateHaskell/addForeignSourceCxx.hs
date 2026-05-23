{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax (ForeignSrcLang (LangCxx), addForeignSource)

$( do
    addForeignSource LangCxx "int test_two_cxx() { return TWO; }"
    pure []
 )

main :: IO ()
main = pure ()
