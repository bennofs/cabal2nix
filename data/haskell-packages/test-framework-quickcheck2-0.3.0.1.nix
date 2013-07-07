{ cabal, extensible-exceptions, QuickCheck, random, test-framework }:

cabal.mkDerivation (self: {
  pname = "test-framework-quickcheck2";
  version = "0.3.0.1";
  sha256 = "1177cvlb4qsa5x2k12dd60y7b14dyd3jr1ygb49aackhjx52c41s";
  buildDepends = [ extensible-exceptions QuickCheck random test-framework ];
  doCheck = false;
  meta = {
    homepage = "http://batterseapower.github.com/test-framework/";
    description = "QuickCheck2 support for the test-framework package";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
