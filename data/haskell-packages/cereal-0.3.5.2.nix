{ cabal }:

cabal.mkDerivation (self: {
  pname = "cereal";
  version = "0.3.5.2";
  sha256 = "0yjxvhagsm552idfrs6d9lzlz6x5878xj1vfg0zxf8w1kixgghil";
  doCheck = false;
  meta = {
    description = "A binary serialization library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
