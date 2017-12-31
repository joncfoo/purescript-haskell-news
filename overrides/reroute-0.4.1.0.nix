{ mkDerivation, base, criterion, deepseq, graph-core, hashable
, hspec, http-api-data, hvect, mtl, random, regex-compat, stdenv
, text, unordered-containers, vector
}:
mkDerivation {
  pname = "reroute";
  version = "0.4.1.0";
  sha256 = "34a83f0d0240610b3e6867f02859d77a8255783e2225389bf025865d5d4c2508";
  libraryHaskellDepends = [
    base deepseq hashable http-api-data hvect mtl text
    unordered-containers
  ];
  testHaskellDepends = [
    base hspec hvect mtl text unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq graph-core hashable http-api-data hvect mtl
    random regex-compat text unordered-containers vector
  ];
  doCheck = false;
  homepage = "http://github.com/agrafix/Spock";
  description = "abstract implementation of typed and untyped web routing";
  license = stdenv.lib.licenses.mit;
}
