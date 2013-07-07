{ cabal, attoparsec, attoparsec-conduit, blaze-builder, blaze-builder-conduit
, blaze-html, blaze-markup, conduit, data-default, failure, hspec, HUnit
, monad-control, resourcet, system-filepath, text, transformers, xml-types
}:

cabal.mkDerivation (self: {
  pname = "xml-conduit";
  version = "1.1.0.3";
  sha256 = "04mnn6j9bbkhvav04gl9cbd4rldl7bwgcapvykwvf2p3nb3d7bi4";
  buildDepends = [
    attoparsec attoparsec-conduit blaze-builder blaze-builder-conduit
    blaze-html blaze-markup conduit data-default failure monad-control
    resourcet system-filepath text transformers xml-types
  ];
  testDepends = [
    blaze-markup conduit hspec HUnit text transformers xml-types
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/snoyberg/xml";
    description = "Pure-Haskell utilities for dealing with XML with the conduit package";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
