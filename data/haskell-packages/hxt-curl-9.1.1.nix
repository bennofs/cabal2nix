{ cabal, curl, hxt, parsec }:

cabal.mkDerivation (self: {
  pname = "hxt-curl";
  version = "9.1.1";
  sha256 = "0dbm71ja9g0asj221vlpdvlhz1ipv3z0x2g4zmv5f03dcs97c11b";
  buildDepends = [ curl hxt parsec ];
  doCheck = false;
  meta = {
    homepage = "http://www.fh-wedel.de/~si/HXmlToolbox/index.html";
    description = "LibCurl interface for HXT";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
