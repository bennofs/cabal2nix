{ cabal, blaze-builder, blaze-builder-conduit, case-insensitive, conduit
, cookie, http-types, HUnit, network, text, transformers, wai
}:

cabal.mkDerivation (self: {
  pname = "wai-test";
  version = "1.3.0.5";
  sha256 = "0v0x1sk7q36xy8syinc96y1jsyjvw6756cich4hxq86l2jfbgccw";
  buildDepends = [
    blaze-builder blaze-builder-conduit case-insensitive conduit cookie
    http-types HUnit network text transformers wai
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/book/web-application-interface";
    description = "Unit test framework (built on HUnit) for WAI applications";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
