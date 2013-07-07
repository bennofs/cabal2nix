{ cabal, data-accessor, event-list, haskell-src, markov-chain, midi
, non-negative, parsec, random, transformers, utility-ht
}:

cabal.mkDerivation (self: {
  pname = "haskore";
  version = "0.2.0.3";
  sha256 = "0vg4m2cmy1fabfnck9v22jicflb0if64h0wjvyrgpn2ykb9wwbpa";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    data-accessor event-list haskell-src markov-chain midi non-negative parsec
    random transformers utility-ht
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.haskell.org/haskellwiki/Haskore";
    description = "The Haskore Computer Music System";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
