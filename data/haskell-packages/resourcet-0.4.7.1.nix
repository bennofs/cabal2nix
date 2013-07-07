{ cabal, hspec, lifted-base, mmorph, monad-control, mtl, transformers
, transformers-base
}:

cabal.mkDerivation (self: {
  pname = "resourcet";
  version = "0.4.7.1";
  sha256 = "1x9njf5amxv04fvn7fsgpagvzl09sl6bnnx686i554frg66b2azh";
  buildDepends = [
    lifted-base mmorph monad-control mtl transformers transformers-base
  ];
  testDepends = [ hspec lifted-base transformers ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/conduit";
    description = "Deterministic allocation and freeing of scarce resources";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
