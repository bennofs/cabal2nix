{ cabal, base64-bytestring, blaze-html, extensible-exceptions, filepath
, hslogger, html, HUnit, monad-control, mtl, network, parsec, sendfile, syb
, system-filepath, text, threads, time, time-compat, transformers
, transformers-base, utf8-string, xhtml, zlib
}:

cabal.mkDerivation (self: {
  pname = "happstack-server";
  version = "7.1.7";
  sha256 = "1gd7qa224z04aya44fdrbhpiy5iw88wjcvqihz6l95ks9ilbzmg7";
  buildDepends = [
    base64-bytestring blaze-html extensible-exceptions filepath hslogger html
    monad-control mtl network parsec sendfile syb system-filepath text threads
    time time-compat transformers transformers-base utf8-string xhtml zlib
  ];
  testDepends = [ HUnit parsec zlib ];
  doCheck = false;
  meta = {
    homepage = "http://happstack.com";
    description = "Web related tools and services";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
