{ cabal }:

cabal.mkDerivation (self: {
  pname = "numtype";
  version = "1.0";
  sha256 = "07sfa3n631x15p8bvcxaxxgyyvy32gawizz6n5vbmw6fgcfyh1i6";
  doCheck = false;
  meta = {
    homepage = "http://dimensional.googlecode.com/";
    description = "Type-level (low cardinality) integers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
