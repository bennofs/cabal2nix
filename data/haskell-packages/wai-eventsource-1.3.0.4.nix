{ cabal, blaze-builder, conduit, http-types, transformers, wai, warp }:

cabal.mkDerivation (self: {
  pname = "wai-eventsource";
  version = "1.3.0.4";
  sha256 = "0716scb7jh0q08drzxbjn8dzz8775yws0bdbh297nm4v1bh6a1hi";
  buildDepends = [ blaze-builder conduit http-types transformers wai warp ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/web-application-interface";
    description = "WAI support for server-sent events";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
