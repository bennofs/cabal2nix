{ cabal, aeson, blaze-builder, blaze-html, blaze-markup, case-insensitive
, cereal, clientsession, conduit, cookie, failure, fast-logger, hamlet, hspec
, http-types, HUnit, lifted-base, monad-control, monad-logger, parsec
, path-pieces, QuickCheck, random, resourcet, shakespeare, shakespeare-css
, shakespeare-i18n, shakespeare-js, text, time, transformers, transformers-base
, vector, wai, wai-extra, wai-test, yesod-routes
}:

cabal.mkDerivation (self: {
  pname = "yesod-core";
  version = "1.1.8.3";
  sha256 = "116vglpqh2561g0gzhm4ijwx829c50ai1hh715vwi5j5i01y2rkr";
  buildDepends = [
    aeson blaze-builder blaze-html blaze-markup case-insensitive cereal
    clientsession conduit cookie failure fast-logger hamlet http-types
    lifted-base monad-control monad-logger parsec path-pieces random resourcet
    shakespeare shakespeare-css shakespeare-i18n shakespeare-js text time
    transformers transformers-base vector wai wai-extra yesod-routes
  ];
  testDepends = [
    blaze-builder conduit hamlet hspec http-types HUnit QuickCheck random
    shakespeare-css shakespeare-js text transformers wai wai-test
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Creation of type-safe, RESTful web applications";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
