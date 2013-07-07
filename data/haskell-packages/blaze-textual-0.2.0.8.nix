{ cabal, blaze-builder, double-conversion, QuickCheck, test-framework
, test-framework-quickcheck2, text, time, vector
}:

cabal.mkDerivation (self: {
  pname = "blaze-textual";
  version = "0.2.0.8";
  sha256 = "0zzqh53ywzz8ss58glp3i8is8gry5yqzvdjw4xh95fbbzcz4cgc7";
  buildDepends = [ blaze-builder text time vector ];
  testDepends = [
    blaze-builder double-conversion QuickCheck test-framework
    test-framework-quickcheck2
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/bos/blaze-textual";
    description = "Fast rendering of common datatypes";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
