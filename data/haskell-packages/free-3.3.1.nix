{ cabal, bifunctors, comonad, comonad-transformers, comonads-fd, distributive
, mtl, semigroupoids, semigroups, transformers
}:

cabal.mkDerivation (self: {
  pname = "free";
  version = "3.3.1";
  sha256 = "0gqm052zxid48ba6k9y8l2rdy1hh1p12yb8ph1r8731f1h0ab9jb";
  buildDepends = [
    bifunctors comonad comonad-transformers comonads-fd distributive mtl
    semigroupoids semigroups transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/free/";
    description = "Monads for free";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
