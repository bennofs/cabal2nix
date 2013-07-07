{ cabal, conduit, control-monad-loop, hspec, mtl, shakespeare-text, text }:

cabal.mkDerivation (self: {
  pname = "process-conduit";
  version = "1.0.0.0";
  sha256 = "013m3jbkx9s8i4ymvaz4jj4p509jly5ylv71f0nzp8kab4mr6ij9";
  buildDepends = [ conduit control-monad-loop mtl shakespeare-text text ];
  testDepends = [ conduit hspec ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/tanakh/process-conduit";
    description = "Conduits for processes";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
