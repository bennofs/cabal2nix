{ cabal, mtl, QuickCheck, random, random-shuffle }:

cabal.mkDerivation (self: {
  pname = "HCard";
  version = "0.0";
  sha256 = "0bvj1vc8m69bsnjz8xb4nbbpbd4xbqp4kfab0bmk6a4ixgnqk14b";
  buildDepends = [ mtl QuickCheck random random-shuffle ];
  doCheck = false;
  meta = {
    homepage = "http://patch-tag.com/publicrepos/HCard";
    description = "A library for implementing a Deck of Cards";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
