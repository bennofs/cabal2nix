{ cabal, ansi-terminal, blaze-builder, blaze-builder-conduit, case-insensitive
, conduit, data-default, date-cache, fast-logger, hspec, http-types, HUnit
, network, resourcet, stringsearch, text, time, transformers, void, wai
, wai-logger, wai-test, zlib, zlib-bindings, zlib-conduit
}:

cabal.mkDerivation (self: {
  pname = "wai-extra";
  version = "1.3.3.2";
  sha256 = "0kw1v68a8dxpxg87r2vjah6n5906mw6cnyy2xx0jbk95qx5g5z8y";
  buildDepends = [
    ansi-terminal blaze-builder blaze-builder-conduit case-insensitive conduit
    data-default date-cache fast-logger http-types network resourcet
    stringsearch text time transformers void wai wai-logger zlib-conduit
  ];
  testDepends = [
    blaze-builder conduit data-default fast-logger hspec http-types HUnit text
    transformers wai wai-test zlib zlib-bindings
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/wai";
    description = "Provides some basic WAI handlers and middleware";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
