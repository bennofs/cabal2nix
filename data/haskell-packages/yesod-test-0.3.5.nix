{ cabal, attoparsec, blaze-builder, blaze-html, blaze-markup, case-insensitive
, cookie, hspec, html-conduit, http-types, HUnit, monad-control, network
, persistent, pool-conduit, text, time, transformers, wai, wai-test
, xml-conduit, xml-types
}:

cabal.mkDerivation (self: {
  pname = "yesod-test";
  version = "0.3.5";
  sha256 = "095hwl1dm4mk467la68x3lilj0c056603kl0nn8ra4glcr86273j";
  buildDepends = [
    attoparsec blaze-builder blaze-html blaze-markup case-insensitive cookie
    hspec html-conduit http-types HUnit monad-control network persistent
    pool-conduit text time transformers wai wai-test xml-conduit xml-types
  ];
  testDepends = [ hspec html-conduit HUnit xml-conduit ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com";
    description = "integration testing for WAI/Yesod Applications";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
