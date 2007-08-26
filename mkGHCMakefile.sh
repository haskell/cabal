#!/bin/sh
file=Distribution/Simple/GHC/Makefile.hs
echo "-- DO NOT EDIT: change Makefile.in, and run ../../../mkGHCMakefile.sh" >$file
echo "module Distribution.Simple.GHC.Makefile where {" >>$file
echo "makefileTemplate :: String; makefileTemplate=unlines" >>$file
ghc -e "readFile \"Distribution/Simple/GHC/Makefile.in\" >>= print . lines" >>$file
echo "}" >>$file
