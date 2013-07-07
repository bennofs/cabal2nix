{ cabal, filepath, hxt, parsec }:

cabal.mkDerivation (self: {
  pname = "hxt-xpath";
  version = "9.1.2";
  sha256 = "0q2shyndc182xvrih4dkvlsqx1ql7g46fq8ndv15nh0cljbvfjij";
  buildDepends = [ filepath hxt parsec ];
  doCheck = false;
  meta = {
    homepage = "http://www.fh-wedel.de/~si/HXmlToolbox/index.html";
    description = "The XPath modules for HXT";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
