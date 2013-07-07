{ cabal, adjunctions, bifunctors, comonad, comonad-transformers, distributive
, keys, mtl, representable-functors, semigroupoids, semigroups, transformers
}:

cabal.mkDerivation (self: {
  pname = "representable-tries";
  version = "3.0.2";
  sha256 = "07qjm04xf9qndyybph4mhjp65yjvz54pia4y8kj7wps75gjail2m";
  buildDepends = [
    adjunctions bifunctors comonad comonad-transformers distributive keys mtl
    representable-functors semigroupoids semigroups transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/representable-tries/";
    description = "Tries from representations of polynomial functors";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
