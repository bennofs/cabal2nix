{ cabal, bifunctors, deepseq, lifted-base, monad-control, mtl, profunctors
, random, semigroups, tagged, time, vector-space
}:

cabal.mkDerivation (self: {
  pname = "netwire";
  version = "4.0.7";
  sha256 = "035jz7j19c7n4za9xi8vj6yl9m2b2v6d53mp711gf8609wv0cx0c";
  buildDepends = [
    bifunctors deepseq lifted-base monad-control mtl profunctors random
    semigroups tagged time vector-space
  ];
  doCheck = false;
  meta = {
    description = "Flexible wire arrows for FRP";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
