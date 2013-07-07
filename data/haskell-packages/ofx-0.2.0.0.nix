{ cabal, explicit-exception, parsec, time }:

cabal.mkDerivation (self: {
  pname = "ofx";
  version = "0.2.0.0";
  sha256 = "0d1f87wam5n6ay612l5gzkrprssim7ipbqj9ks6ngmy1dpwgqbzl";
  buildDepends = [ explicit-exception parsec time ];
  doCheck = false;
  meta = {
    homepage = "http://www.github.com/massysett/ofx";
    description = "Parser for OFX data";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
