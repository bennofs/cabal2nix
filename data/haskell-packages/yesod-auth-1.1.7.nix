{ cabal, aeson, authenticate, blaze-html, blaze-markup, file-embed, hamlet
, http-conduit, http-types, lifted-base, mime-mail, network, persistent
, persistent-template, pureMD5, pwstore-fast, random, SHA, shakespeare-css
, shakespeare-js, text, transformers, unordered-containers, wai, yesod-core
, yesod-form, yesod-json, yesod-persistent
}:

cabal.mkDerivation (self: {
  pname = "yesod-auth";
  version = "1.1.7";
  sha256 = "0vsgpvmsmdzbwj5l862bzm21ld3adkmbwgkffbpjr4yylbvvyvrn";
  buildDepends = [
    aeson authenticate blaze-html blaze-markup file-embed hamlet http-conduit
    http-types lifted-base mime-mail network persistent persistent-template
    pureMD5 pwstore-fast random SHA shakespeare-css shakespeare-js text
    transformers unordered-containers wai yesod-core yesod-form yesod-json
    yesod-persistent
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Authentication for Yesod";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
