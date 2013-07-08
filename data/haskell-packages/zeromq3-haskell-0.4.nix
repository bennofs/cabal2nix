{ cabal, ansi-terminal, async, checkers, MonadCatchIO-transformers, QuickCheck
, semigroups, transformers, zeromq3
}:

cabal.mkDerivation (self: {
  pname = "zeromq3-haskell";
  version = "0.4";
  sha256 = "0kwf4bzqp02vk78c9rphyamwbmvzff1z9hqp70dhfggm0q2d655s";
  buildDepends = [ async MonadCatchIO-transformers semigroups transformers ];
  testDepends = [
    ansi-terminal checkers MonadCatchIO-transformers QuickCheck transformers
  ];
  extraLibraries = [ zeromq3 ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/twittner/zeromq-haskell/";
    description = "Bindings to ZeroMQ 3.x";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
