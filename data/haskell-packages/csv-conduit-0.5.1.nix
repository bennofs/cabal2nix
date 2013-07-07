{ cabal, attoparsec, attoparsec-conduit, conduit, data-default, HUnit
, monad-control, test-framework, test-framework-hunit, text
}:

cabal.mkDerivation (self: {
  pname = "csv-conduit";
  version = "0.5.1";
  sha256 = "1jgzd9wrfcdjhqilp5rgdvq1n8c3mjy52bws4pg3zwajfwpqcaa2";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec attoparsec-conduit conduit data-default monad-control text
  ];
  testDepends = [ HUnit test-framework test-framework-hunit text ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ozataman/csv-conduit";
    description = "A flexible, fast, conduit-based CSV parser library for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
