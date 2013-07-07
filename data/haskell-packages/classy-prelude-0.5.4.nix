{ cabal, basic-prelude, hashable, hspec, lifted-base, QuickCheck
, system-filepath, text, transformers, unordered-containers, vector
}:

cabal.mkDerivation (self: {
  pname = "classy-prelude";
  version = "0.5.4";
  sha256 = "1p30f1inp0kj2dsvqnxaplxl83kd8bv41jmszvbdbf3vijjpk6kr";
  buildDepends = [
    basic-prelude hashable lifted-base system-filepath text transformers
    unordered-containers vector
  ];
  testDepends = [ hspec QuickCheck transformers ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/snoyberg/classy-prelude";
    description = "A typeclass-based Prelude";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
