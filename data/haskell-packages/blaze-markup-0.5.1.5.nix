{ cabal, blaze-builder, HUnit, QuickCheck, test-framework, test-framework-hunit
, test-framework-quickcheck2, text
}:

cabal.mkDerivation (self: {
  pname = "blaze-markup";
  version = "0.5.1.5";
  sha256 = "0g3smm1ym7h45bkzx94b77ssyg0z0gqfwbnap3ywa2381rb39l74";
  buildDepends = [ blaze-builder text ];
  testDepends = [
    blaze-builder HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  doCheck = false;
  meta = {
    homepage = "http://jaspervdj.be/blaze";
    description = "A blazingly fast markup combinator library for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
