{ cabal, syb-with-class, text }:

cabal.mkDerivation (self: {
  pname = "syb-with-class-instances-text";
  version = "0.0.1";
  sha256 = "0vnpqk89nxs0anx62mzasl9wrcscw18vwc284y067ryb086aj2hf";
  buildDepends = [ syb-with-class text ];
  doCheck = false;
  meta = {
    description = "Scrap Your Boilerplate With Class Text instance";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
