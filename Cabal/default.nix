{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, parsec, pretty, process, QuickCheck, stdenv, tagged, tar, tasty
, tasty-golden, tasty-hunit, tasty-quickcheck, time, transformers
, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    parsec pretty process time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans bytestring containers Diff
    directory filepath pretty QuickCheck tagged tar tasty tasty-golden
    tasty-hunit tasty-quickcheck
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
