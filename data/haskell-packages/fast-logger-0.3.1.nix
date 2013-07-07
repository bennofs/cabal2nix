{ cabal, blaze-builder, date-cache, filepath, hspec, text, unix-time }:

cabal.mkDerivation (self: {
  pname = "fast-logger";
  version = "0.3.1";
  sha256 = "0sjn3vad0fbchv1fhap71wfnihlwnfhk6p9h9hpnbr0i4b32f1ks";
  buildDepends = [ blaze-builder date-cache filepath text unix-time ];
  testDepends = [ hspec ];
  doCheck = false;
  meta = {
    description = "A fast logging system";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
