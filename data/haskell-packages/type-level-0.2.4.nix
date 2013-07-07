{ cabal, syb }:

cabal.mkDerivation (self: {
  pname = "type-level";
  version = "0.2.4";
  sha256 = "1cgph4y6j7wnzglzz89zd60f1mv8v82vp0n1bmkp4yqq0w2wmg9v";
  buildDepends = [ syb ];
  doCheck = false;
  meta = {
    homepage = "http://code.haskell.org/type-level";
    description = "Type-level programming library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
