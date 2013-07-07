{ cabal, attoparsec, HUnit, test-framework-doctest, test-framework-hunit
, test-framework-th-prime, time
}:

cabal.mkDerivation (self: {
  pname = "http-date";
  version = "0.0.2";
  sha256 = "18nijf3hswbvcd5amhbccqmgnanmslsmqpd2iixlvwz5hqb3ncvj";
  buildDepends = [ attoparsec ];
  testDepends = [
    HUnit test-framework-doctest test-framework-hunit test-framework-th-prime
    time
  ];
  doCheck = false;
  meta = {
    description = "HTTP Date parser/formatter";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
