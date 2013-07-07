{ cabal, aeson, attoparsec-conduit, blaze-builder, conduit, hspec, safe
, shakespeare-js, text, transformers, vector, wai, wai-extra, wai-test
, yesod-core, yesod-routes
}:

cabal.mkDerivation (self: {
  pname = "yesod-json";
  version = "1.1.2.2";
  sha256 = "1bl4rs3biv2man0n8ijldil32lyswjqk5ykz0nv06qsaaafjpc3g";
  buildDepends = [
    aeson attoparsec-conduit blaze-builder conduit safe shakespeare-js text
    transformers vector wai wai-extra yesod-core yesod-routes
  ];
  testDepends = [ hspec text wai-test yesod-core ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Generate content for Yesod using the aeson package";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
