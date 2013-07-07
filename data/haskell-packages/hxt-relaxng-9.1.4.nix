{ cabal, hxt, hxt-charproperties, hxt-regex-xmlschema, network, parsec }:

cabal.mkDerivation (self: {
  pname = "hxt-relaxng";
  version = "9.1.4";
  sha256 = "0mw4987gpihk7bk35fhwf7vaa9dxmr9v7c9wc0fq9ahl9yib605f";
  buildDepends = [ hxt hxt-charproperties hxt-regex-xmlschema network parsec ];
  doCheck = false;
  meta = {
    homepage = "http://www.fh-wedel.de/~si/HXmlToolbox/index.html";
    description = "The HXT RelaxNG validator";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
