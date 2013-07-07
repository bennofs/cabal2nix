{ cabal, HUnit, QuickCheck, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:

cabal.mkDerivation (self: {
  pname = "largeword";
  version = "1.0.5";
  sha256 = "0icwqwpn59xd0qfpaihvwz1waax617qqcl05jv9f26sjdr8688dl";
  testDepends = [
    HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/idontgetoutmuch/largeword";
    description = "Provides Word128, Word192 and Word256 and a way of producing other large words if required";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
