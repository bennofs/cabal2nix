{ cabal, Cabal, deepseq, mtl, parallel, parsec, QuickCheck, random, terminfo
, utf8-string, vector
}:

cabal.mkDerivation (self: {
  pname = "vty";
  version = "4.7.0.20";
  sha256 = "15slw4zpvfkx9qwj2g5ndcxwbw0kkhyq8frvh9kharqd0zqzgqzb";
  buildDepends = [ deepseq mtl parallel parsec terminfo utf8-string vector ];
  testDepends = [
    Cabal deepseq mtl parallel parsec QuickCheck random terminfo utf8-string
    vector
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/coreyoconnor/vty";
    description = "A simple terminal UI library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
