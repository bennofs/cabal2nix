{ cabal, conduit, lifted-base, monad-control, network, transformers }:

cabal.mkDerivation (self: {
  pname = "network-conduit";
  version = "1.0.0";
  sha256 = "16kgg6wkpl10kcwfijm9iqi7r5gababaymxyhmjab6axfzknppk3";
  buildDepends = [ conduit lifted-base monad-control network transformers ];
  testDepends = [ conduit ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/conduit";
    description = "Stream socket data using conduits";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
