{ cabal, xml-conduit }:

cabal.mkDerivation (self: {
  pname = "xml2html";
  version = "0.2.0";
  sha256 = "1kf4vjg4cfkd4vx8jpikbb0ib4pglmyf5vqrg3j0yllmycj22ska";
  buildDepends = [ xml-conduit ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/xml";
    description = "blaze-html instances for xml-conduit types (deprecated)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
