{ cabal, blaze-builder, blaze-builder-conduit, case-insensitive, conduit
, hashable, hspec, http-types, HUnit, lifted-base, network, network-conduit
, QuickCheck, simple-sendfile, transformers, unix-compat, void, wai
}:

cabal.mkDerivation (self: {
  pname = "warp";
  version = "1.3.7.4";
  sha256 = "0z043v1fjblbvkn44hzp7j8iawqzbwf67qxq7gsv7jfq1q58xbrx";
  buildDepends = [
    blaze-builder blaze-builder-conduit case-insensitive conduit hashable
    http-types lifted-base network network-conduit simple-sendfile transformers
    unix-compat void wai
  ];
  testDepends = [
    blaze-builder blaze-builder-conduit case-insensitive conduit hashable hspec
    http-types HUnit lifted-base network network-conduit QuickCheck
    simple-sendfile transformers unix-compat void wai
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/wai";
    description = "A fast, light-weight web server for WAI applications";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
