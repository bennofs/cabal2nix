{ cabal, binary, deepseq, filepath, hxt, SHA, time }:

cabal.mkDerivation (self: {
  pname = "hxt-cache";
  version = "9.1.0";
  sha256 = "16205mp6kilq3s6y08sz45nl23cpwnnbb83v561nais4clc99m7y";
  buildDepends = [ binary deepseq filepath hxt SHA time ];
  doCheck = false;
  meta = {
    homepage = "http://www.fh-wedel.de/~si/HXmlToolbox/index.html";
    description = "Cache for HXT XML Documents and other binary data";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
