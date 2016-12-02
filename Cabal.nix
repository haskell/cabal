{ mkDerivation, array, base, binary, bytestring, containers
, deepseq, directory, exceptions, filepath, old-time, pretty
, process, QuickCheck, regex-posix, stdenv, tagged, tasty
, tasty-hunit, tasty-quickcheck, time, transformers, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "1.25.0.0";
  src = ./Cabal;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    pretty process time unix
  ];
  testHaskellDepends = [
    array base bytestring containers directory exceptions filepath
    old-time pretty process QuickCheck regex-posix tagged tasty
    tasty-hunit tasty-quickcheck time transformers unix
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
