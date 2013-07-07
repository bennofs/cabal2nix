{ cabal, zlib }:

cabal.mkDerivation (self: {
  pname = "digest";
  version = "0.0.1.0";
  sha256 = "1p2fk950ivdj7pvc624y0fx48rdh0ax3rw9606926n60mxi9fca0";
  extraLibraries = [ zlib ];
  doCheck = false;
  meta = {
    description = "Various cryptographic hashes for bytestrings; CRC32 and Adler32 for now";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
