{ cabal, conduit, filesystem-conduit, hspec, HUnit, resourcet, system-filepath
, tagstream-conduit, text, transformers, xml-conduit, xml-types
}:

cabal.mkDerivation (self: {
  pname = "html-conduit";
  version = "1.1.0";
  sha256 = "11mkr7plhbn8kyki0h362habzhsnlb7yrg4ypy48d0l3i7p7vm53";
  buildDepends = [
    conduit filesystem-conduit resourcet system-filepath tagstream-conduit text
    transformers xml-conduit xml-types
  ];
  testDepends = [ hspec HUnit xml-conduit ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/snoyberg/xml";
    description = "Parse HTML documents using xml-conduit datatypes";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
