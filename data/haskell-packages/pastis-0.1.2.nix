{ cabal, HTTP, network }:

cabal.mkDerivation (self: {
  pname = "pastis";
  version = "0.1.2";
  sha256 = "1425gzss5maqzrphrvvsw60lkapwg3wfjx10c59qkylx63k5ixjl";
  buildDepends = [ HTTP network ];
  doCheck = false;
  meta = {
    description = "Interface to the past.is URL shortening service";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
