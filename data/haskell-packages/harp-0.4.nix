{ cabal }:

cabal.mkDerivation (self: {
  pname = "harp";
  version = "0.4";
  sha256 = "0fk3prqai1ynm5wdfsn9f700i9r499jc2z9fbsgy81k1rci2mrxh";
  doCheck = false;
  meta = {
    homepage = "http://www.cs.chalmers.se/~d00nibro/harp/";
    description = "HaRP allows pattern-matching with regular expressions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
