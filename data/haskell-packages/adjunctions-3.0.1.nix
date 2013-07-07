{ cabal, comonad, comonad-transformers, contravariant, distributive, keys, mtl
, representable-functors, semigroupoids, transformers, void
}:

cabal.mkDerivation (self: {
  pname = "adjunctions";
  version = "3.0.1";
  sha256 = "0zirb5lixrls9dw6p3dp91k58p4hwi5kbvc8s2n4vqgr9gs8fj19";
  buildDepends = [
    comonad comonad-transformers contravariant distributive keys mtl
    representable-functors semigroupoids transformers void
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/adjunctions/";
    description = "Adjunctions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
