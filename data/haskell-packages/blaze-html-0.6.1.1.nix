{ cabal, blaze-builder, blaze-markup, HUnit, QuickCheck, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
}:

cabal.mkDerivation (self: {
  pname = "blaze-html";
  version = "0.6.1.1";
  sha256 = "08zfmkvahmm613r0nrabwl5zv9ragcrhdqsa8jfdrfdkrf6ckbrc";
  buildDepends = [ blaze-builder blaze-markup text ];
  testDepends = [
    blaze-builder blaze-markup HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  doCheck = false;
  meta = {
    homepage = "http://jaspervdj.be/blaze";
    description = "A blazingly fast HTML combinator library for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
