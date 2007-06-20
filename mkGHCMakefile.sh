#!/bin/sh
file=Distribution/Simple/GHCMakefile.hs
echo "-- DO NOT EDIT: change GHCMakefile.in, and run ../../mkGHCMakefile.sh" >$file
echo "module Distribution.Simple.GHCMakefile where{ makefileTemplate=unlines" >>$file
ghc -e "readFile \"Distribution/Simple/GHCMakefile.in\" >>= print . lines" >>$file
echo "}" >>$file
