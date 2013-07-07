{ cabal, binary }:

cabal.mkDerivation (self: {
  pname = "bytestring-show";
  version = "0.3.5.4";
  sha256 = "1hyss8wacchxix6wrnnqpafrf00w5671ygbaczra8phf4i6skv1f";
  buildDepends = [ binary ];
  doCheck = false;
  meta = {
    homepage = "http://code.haskell.org/~dolio/";
    description = "Efficient conversion of values into readable byte strings";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
