{ cabal, base64-bytestring, blaze-builder, blaze-html, blaze-markup, cereal
, crypto-conduit, cryptohash, file-embed, hspec, http-date, http-types
, mime-types, network, system-fileio, system-filepath, text, time, transformers
, unix-compat, wai, wai-test
}:

cabal.mkDerivation (self: {
  pname = "wai-app-static";
  version = "1.3.1.2";
  sha256 = "1i0j3n0cp1zq43g06nb3cxajd4k9isg4chdwzzlyimm078a9w54x";
  buildDepends = [
    base64-bytestring blaze-builder blaze-html blaze-markup cereal
    crypto-conduit cryptohash file-embed http-date http-types mime-types
    system-fileio system-filepath text time transformers unix-compat wai
  ];
  testDepends = [
    hspec http-date http-types mime-types network text time transformers
    unix-compat wai wai-test
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/web-application-interface";
    description = "WAI application for static serving";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
