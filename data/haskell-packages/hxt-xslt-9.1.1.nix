{ cabal, filepath, hxt, hxt-xpath, parsec }:

cabal.mkDerivation (self: {
  pname = "hxt-xslt";
  version = "9.1.1";
  sha256 = "0klyqxgq46dca8ia3xygd194hw8p51a4iz8746zz4kifjc1fyprs";
  buildDepends = [ filepath hxt hxt-xpath parsec ];
  doCheck = false;
  meta = {
    homepage = "http://www.fh-wedel.de/~si/HXmlToolbox/index.html";
    description = "The XSLT modules for HXT";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
