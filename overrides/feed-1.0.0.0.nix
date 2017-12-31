{ mkDerivation, base, base-compat, bytestring, HUnit, old-locale
, old-time, safe, stdenv, test-framework, test-framework-hunit
, text, time, time-locale-compat, utf8-string, xml-conduit
, xml-types
}:
mkDerivation {
  pname = "feed";
  version = "1.0.0.0";
  sha256 = "05rgg7x1984mgfhkmz792xj8lhwjgznixhygzr8blf517lns2nck";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base base-compat bytestring old-locale old-time safe text time
    time-locale-compat utf8-string xml-conduit xml-types
  ];
  testHaskellDepends = [
    base base-compat HUnit old-locale old-time test-framework
    test-framework-hunit text time time-locale-compat utf8-string
    xml-conduit xml-types
  ];
  homepage = "https://github.com/bergmark/feed";
  description = "Interfacing with RSS (v 0.9x, 2.x, 1.0) + Atom feeds.";
  license = stdenv.lib.licenses.bsd3;
}
