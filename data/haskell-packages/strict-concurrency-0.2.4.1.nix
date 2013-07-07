{ cabal, deepseq }:

cabal.mkDerivation (self: {
  pname = "strict-concurrency";
  version = "0.2.4.1";
  sha256 = "128sbh5fnv02v2xdjlk7cb525bfckqyj7fyz5399nfycs0nj2f89";
  buildDepends = [ deepseq ];
  doCheck = false;
  meta = {
    homepage = "http://code.haskell.org/~dons/code/strict-concurrency";
    description = "Strict concurrency abstractions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
