{ cabal, mtl, text }:

cabal.mkDerivation (self: {
  pname = "reform";
  version = "0.1.2";
  sha256 = "02baspissx7j1hp2j3ads2rcq06w2i72hbpm7myd67ha87l895qv";
  buildDepends = [ mtl text ];
  doCheck = false;
  meta = {
    homepage = "http://www.happstack.com/";
    description = "reform is an HTML form generation and validation library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
