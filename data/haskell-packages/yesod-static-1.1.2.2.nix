{ cabal, base64-bytestring, cereal, conduit, crypto-conduit, cryptohash
, file-embed, hspec, http-types, system-filepath, text, transformers
, unix-compat, wai, wai-app-static, yesod-core
}:

cabal.mkDerivation (self: {
  pname = "yesod-static";
  version = "1.1.2.2";
  sha256 = "1z1afpr9xbclpwswlbys7f7w8761vvr06hxhqhnqfzf4ky8g3671";
  buildDepends = [
    base64-bytestring cereal conduit crypto-conduit cryptohash file-embed
    http-types system-filepath text transformers unix-compat wai wai-app-static
    yesod-core
  ];
  testDepends = [
    base64-bytestring cereal conduit crypto-conduit cryptohash file-embed hspec
    http-types system-filepath text transformers unix-compat wai wai-app-static
    yesod-core
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Static file serving subsite for Yesod Web Framework";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
