{ cabal, attoparsec, attoparsec-enumerator, binary, binary-shared, Cabal
, deepseq, enumerator, filepath, haddock, hslogger, HUnit, ltk, network, parsec
, process-leksah, strict, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "leksah-server";
  version = "0.12.1.2";
  sha256 = "0fzfyq1g1jrfl40nklgvkahlcv32m4gjbcyw52dky2qzc05b0g6m";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec attoparsec-enumerator binary binary-shared Cabal deepseq
    enumerator filepath haddock hslogger ltk network parsec process-leksah
    strict time transformers
  ];
  testDepends = [ enumerator hslogger HUnit transformers ];
  doCheck = false;
  meta = {
    homepage = "http://leksah.org";
    description = "Metadata collection for leksah";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
