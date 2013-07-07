{ cabal, comonad, comonad-transformers, mtl, semigroups, transformers }:

cabal.mkDerivation (self: {
  pname = "comonads-fd";
  version = "3.0.1";
  sha256 = "0ap9sw7h130bza43091mbl9a5bsin6342zawgycdcsag49wm3dyy";
  buildDepends = [ comonad comonad-transformers mtl semigroups transformers ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/comonads-fd/";
    description = "Comonad transformers using functional dependencies";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
