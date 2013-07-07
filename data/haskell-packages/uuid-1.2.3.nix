{ cabal, binary, criterion, Crypto, deepseq, HUnit, maccatcher
, mersenne-random-pure64, QuickCheck, random, time
}:

cabal.mkDerivation (self: {
  pname = "uuid";
  version = "1.2.3";
  sha256 = "0n6fs5hfnz9ymvyvg2209fgzkgvd6cx0p5as6nmsa735l92b8wzb";
  buildDepends = [ binary Crypto maccatcher random time ];
  testDepends = [
    criterion deepseq HUnit mersenne-random-pure64 QuickCheck random
  ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/uuid/";
    description = "For creating, comparing, parsing and printing Universally Unique Identifiers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
