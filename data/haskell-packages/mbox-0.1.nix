{ cabal, safe, time }:

cabal.mkDerivation (self: {
  pname = "mbox";
  version = "0.1";
  sha256 = "1nbdflh3nz9x53cflny4lfq220l1m0lya8kifhnw4mw28rlchckz";
  buildDepends = [ safe time ];
  doCheck = false;
  meta = {
    description = "Read and write standard mailbox files";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
