{ cabal, cereal, crypto-api, HUnit, QuickCheck, tagged, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:

cabal.mkDerivation (self: {
  pname = "cryptohash";
  version = "0.8.4";
  sha256 = "0h58apxcrvpkyj3s2c7l68g7biv80snkn53g6jya1s687xff8fmn";
  buildDepends = [ cereal crypto-api tagged ];
  testDepends = [
    HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/vincenthz/hs-cryptohash";
    description = "collection of crypto hashes, fast, pure and practical";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
