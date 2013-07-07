{ cabal, aeson, attoparsec, blaze-builder, blaze-html, blaze-markup, crypto-api
, data-default, email-validate, hamlet, hspec, network, persistent
, shakespeare-css, shakespeare-js, text, time, transformers, wai, xss-sanitize
, yesod-core, yesod-persistent
}:

cabal.mkDerivation (self: {
  pname = "yesod-form";
  version = "1.2.1.3";
  sha256 = "1mrqr6pfrhkk0vv9lhd4wxiic8d1hsraiqmjcqsfbpdyrc6fqypq";
  buildDepends = [
    aeson attoparsec blaze-builder blaze-html blaze-markup crypto-api
    data-default email-validate hamlet network persistent shakespeare-css
    shakespeare-js text time transformers wai xss-sanitize yesod-core
    yesod-persistent
  ];
  testDepends = [ hspec text time ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Form handling support for Yesod Web Framework";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
