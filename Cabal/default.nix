{ mkDerivation, array, base, binary, bytestring, containers
, deepseq, directory, filepath, pretty, process, QuickCheck, stdenv
, tagged, tasty, tasty-hunit, tasty-quickcheck, time, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    pretty process time unix
  ];
  testHaskellDepends = [
    array base containers directory filepath pretty QuickCheck tagged
    tasty tasty-hunit tasty-quickcheck
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
