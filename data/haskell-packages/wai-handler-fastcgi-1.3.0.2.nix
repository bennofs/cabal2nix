{ cabal, fcgi, wai, wai-extra }:

cabal.mkDerivation (self: {
  pname = "wai-handler-fastcgi";
  version = "1.3.0.2";
  sha256 = "044gzyygnrj83f6fj989783rcqa6h1jqky2x6z7gk7d0zw0cx23q";
  buildDepends = [ wai wai-extra ];
  extraLibraries = [ fcgi ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/web-application-interface";
    description = "Wai handler to fastcgi";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
