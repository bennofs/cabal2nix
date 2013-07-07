{ cabal, cereal, text, time }:

cabal.mkDerivation (self: {
  pname = "safecopy";
  version = "0.7.1";
  sha256 = "0hg7lbz4i2d2cwr3c0ix2sfrk5p2fhblj5fmpmscpjfwcddci9cv";
  buildDepends = [ cereal text time ];
  doCheck = false;
  meta = {
    homepage = "http://acid-state.seize.it/safecopy";
    description = "Binary serialization with version control";
    license = self.stdenv.lib.licenses.publicDomain;
    platforms = self.ghc.meta.platforms;
  };
})
