{ cabal, QuickCheck, random, test-framework, test-framework-quickcheck2, text
}:

cabal.mkDerivation (self: {
  pname = "hashable";
  version = "1.1.2.5";
  sha256 = "0gbiaj5ck2bvvinndp2pg7qsm2h2izbnz9wi97dbm7i8r4qd9d9z";
  buildDepends = [ text ];
  testDepends = [
    QuickCheck random test-framework test-framework-quickcheck2 text
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/tibbe/hashable";
    description = "A class for types that can be converted to a hash value";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
