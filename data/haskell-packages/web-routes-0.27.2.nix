{ cabal, blaze-builder, http-types, HUnit, mtl, network, parsec, QuickCheck
, split, test-framework, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "web-routes";
  version = "0.27.2";
  sha256 = "17kzxccw6pz0zh0cfn1hd0lfwz5v9zjx4fjzp68ai9labd2kb05h";
  buildDepends = [
    blaze-builder http-types mtl network parsec split text utf8-string
  ];
  testDepends = [
    HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th
  ];
  doCheck = false;
  meta = {
    description = "Library for maintaining correctness and composability of URLs within an application";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
