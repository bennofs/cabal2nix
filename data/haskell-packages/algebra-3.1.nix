{ cabal, categories, distributive, keys, mtl, nats, representable-functors
, representable-tries, semigroupoids, semigroups, tagged, transformers, void
}:

cabal.mkDerivation (self: {
  pname = "algebra";
  version = "3.1";
  sha256 = "0k0sngyagc9bhkjs3nxn8cr26qr64fbn88jh5hg2w7jsk1ffiiwp";
  buildDepends = [
    categories distributive keys mtl nats representable-functors
    representable-tries semigroupoids semigroups tagged transformers void
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/algebra/";
    description = "Constructive abstract algebra";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
