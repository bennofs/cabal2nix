{ cabal, wai, wai-extra }:

cabal.mkDerivation (self: {
  pname = "wai-handler-scgi";
  version = "1.3.0.2";
  sha256 = "082q0wk8d76aqj2g2w6n7yqqnhjxj36fbk4n8jqr0wsbz29xxzni";
  buildDepends = [ wai wai-extra ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/web-application-interface";
    description = "Wai handler to SCGI";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
