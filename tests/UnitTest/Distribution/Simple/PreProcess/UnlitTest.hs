
module UnitTest.Distribution.Simple.PreProcess.Unlit where

import Distribution.Simple.PreProcess.Unlit
import Control.Exception

cases =
  ( "", "" ) :
  -- latex state
  ( "\\begin{code}\n\\end{code}\na\n", "\n\n-- a\n") :  -- latex -> comment
  ( "\\begin{code}\nx=x\n\\end{code}\n", "\nx=x\n\n") :  -- latex -> latex (code)
  ( "\\begin{code}\n\\begin{code}\n", "\\begin{code} in code section") :  -- latex -> error
  -- blank state
  ( "\\end{code}\n", "\\end{code} without \\begin{code}") :  -- blank -> error
  ( "\\begin{code}\n\\end{code}\n", "\n\n") :  -- blank -> latex
  ( " \n#pre\n \n", " \n#pre\n \n" ) :  -- blank -> blank (CPP)
  ( "\n \n#pre\n \n", "\n \n#pre\n \n" ) :  -- blank -> blank (CPP)
  ( "\n> x=x\n", "\n  x=x\n" ) :  -- blank -> bird (> )
  ( "\n>x=x\n", "\n x=x\n" ) :  -- blank -> bird (>)
  ( "\n", "\n" ) :  -- blank -> blank
  ( " \n", " \n" ) :  -- blank -> blank
  ( " \na\n", " \n-- a\n" ) :  -- blank -> comment
  -- bird state
  ( "> x=x\n\\end{code}\n", "\\end{code} without \\begin{code}") :  -- bird -> error
  ( "> x=x\n\\begin{code}\ny=y\n", "  x=x\n\ny=y\n" ) :  -- bird -> latex
  ( "> x=x\n#abc\n> y=y\n", "  x=x\n#abc\n  y=y\n" ) :  -- bird -> bird (CPP)
  ( "> x=x\n> y=y\n", "  x=x\n  y=y\n" ) :  -- bird -> bird (> )
  ( ">x=x\n>y=y\n", " x=x\n y=y\n" ) :  -- bird -> bird (>)
  ( "> x=x\n  \n", "  x=x\n  \n" ) :  -- bird -> empty
  ( "> x=x\na\n", "program line before comment line" ) :  -- bird -> error
  -- comment state
    -- comment -> error
  ( "a\n\\end{code}\n", "\\end{code} without \\begin{code}") :
    -- comment -> latex
  ( "a\n\\begin{code}\nx=x\n\\end{code}\nb\n", "-- a\n\nx=x\n\n-- b\n" ) :
  ( "a\n#pre\nb\n", "-- a\n#pre\n-- b\n" ) :  -- comment -> comment (CPP)
  ( "a\n> x=x\n", "comment line before program line" ) :  -- comment -> error
  ( "abc\n", "-- abc\n" ) :
  ( "a\nb\n", "-- a\n-- b\n") :
  ( "a\n\n", "-- a\n\n") :  -- comment -> blank
  ( "a\n\nb\n", "-- a\n--\n-- b\n" ) : -- comment -> blank
  ( "a\n \n\n", "-- a\n \n\n" ) :  -- comment -> comment
  ( "a\n \n> x=x\n", "-- a\n \n  x=x\n" ) :  -- comment -> blank (> )
  ( "a\n \n>x=x\n", "-- a\n \n x=x\n" ) :  -- comment -> blank (>)
  ( "a\n \nb\n", "-- a\n--  \n-- b\n" ) :  -- comment -> comment
  []


assertEq :: Int -> String -> String -> IO ()
assertEq n actual expect =
  if actual /= expect
    then putStrLn ("Test "++show n++" failed:\n  expect: "++expect++"\n  actual: "++(take 200 actual)++"\n")
    else putStrLn ("Test "++show n++" passed.")

runTest (n, (input, expect)) = do
  let actual = either id stripErr $ unlit ("test"++show n) input
  assertEq n actual (expect ++ "\n")
  where stripErr = (++"\n") . drop 2 . dropWhile (/= ':') . tail . dropWhile (/= ':')

runTests = mapM_ runTest (zip [1..] cases)
