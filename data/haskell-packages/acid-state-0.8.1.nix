{ cabal, cereal, extensible-exceptions, filepath, mtl, network, safecopy, stm
}:

cabal.mkDerivation (self: {
  pname = "acid-state";
  version = "0.8.1";
  sha256 = "18g16pi2az8my1fqm8xlyrm5a6i37x0p5q076z2dy49g1bmjhj15";
  buildDepends = [
    cereal extensible-exceptions filepath mtl network safecopy stm
  ];
  doCheck = false;
  meta = {
    homepage = "http://acid-state.seize.it/";
    description = "Add ACID guarantees to any serializable Haskell data structure";
    license = self.stdenv.lib.licenses.publicDomain;
    platforms = self.ghc.meta.platforms;
  };
})
