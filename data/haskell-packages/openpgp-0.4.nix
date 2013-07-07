{ cabal, binary, bzlib, HUnit, QuickCheck, quickcheck-instances, test-framework
, test-framework-hunit, test-framework-quickcheck2, utf8-string, zlib
}:

cabal.mkDerivation (self: {
  pname = "openpgp";
  version = "0.4";
  sha256 = "0v1dq133x015af2n1zsl1g26pdcw25p3r1bm4bglmr5m8fizpwqc";
  buildDepends = [ binary bzlib utf8-string zlib ];
  testDepends = [
    binary bzlib HUnit QuickCheck quickcheck-instances test-framework
    test-framework-hunit test-framework-quickcheck2 utf8-string zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/singpolyma/OpenPGP-Haskell";
    description = "Implementation of the OpenPGP message format";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
