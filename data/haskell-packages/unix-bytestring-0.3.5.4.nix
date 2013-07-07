{ cabal }:

cabal.mkDerivation (self: {
  pname = "unix-bytestring";
  version = "0.3.5.4";
  sha256 = "1hsgw9qyrlhvkpmwwxcxk6xwad7524g10yzs1s7d7g0zrpwgy7qg";
  doCheck = false;
  meta = {
    homepage = "http://code.haskell.org/~wren/";
    description = "Unix/Posix-specific functions for ByteStrings";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
