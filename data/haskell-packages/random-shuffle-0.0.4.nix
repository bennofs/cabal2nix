{ cabal, MonadRandom, random }:

cabal.mkDerivation (self: {
  pname = "random-shuffle";
  version = "0.0.4";
  sha256 = "0586bnlh0g2isc44jbjvafkcl4yw6lp1db8x6vr0pza0y08l8w2j";
  buildDepends = [ MonadRandom random ];
  doCheck = false;
  meta = {
    description = "Random shuffle implementation";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
