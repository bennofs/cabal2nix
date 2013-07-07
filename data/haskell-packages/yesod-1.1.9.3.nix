{ cabal, aeson, attoparsec, base64-bytestring, blaze-builder, blaze-html
, blaze-markup, Cabal, conduit, file-embed, filepath, fsnotify, ghc-paths
, hamlet, http-conduit, http-reverse-proxy, http-types, lifted-base
, monad-control, network, network-conduit, optparse-applicative, parsec
, project-template, resourcet, shakespeare, shakespeare-css, shakespeare-js
, shakespeare-text, split, system-fileio, system-filepath, tar, text, time
, transformers, unix-compat, unordered-containers, wai, wai-extra, warp, yaml
, yesod-auth, yesod-core, yesod-default, yesod-form, yesod-json
, yesod-persistent, zlib
}:

cabal.mkDerivation (self: {
  pname = "yesod";
  version = "1.1.9.3";
  sha256 = "16w96lyd0z9slhzggbvmb23yfr3jkwywpmp15kbilwn7ghjlkm41";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base64-bytestring blaze-builder blaze-html blaze-markup
    Cabal conduit file-embed filepath fsnotify ghc-paths hamlet http-conduit
    http-reverse-proxy http-types lifted-base monad-control network
    network-conduit optparse-applicative parsec project-template resourcet
    shakespeare shakespeare-css shakespeare-js shakespeare-text split
    system-fileio system-filepath tar text time transformers unix-compat
    unordered-containers wai wai-extra warp yaml yesod-auth yesod-core
    yesod-default yesod-form yesod-json yesod-persistent zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Creation of type-safe, RESTful web applications";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
