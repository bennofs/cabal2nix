{ cabal, deepseq, filepath, http-conduit, http-date, http-types, HUnit, network
, network-conduit, parsec, test-framework-hunit, test-framework-th-prime, time
, transformers, unix-bytestring, unordered-containers, wai, wai-app-file-cgi
, wai-logger, wai-logger-prefork, warp
}:

cabal.mkDerivation (self: {
  pname = "mighttpd2";
  version = "2.5.9";
  sha256 = "06x8vpv7dmg9n7slgyjm7wanp09j0fk0p6l5fr4yf669l19fp82l";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    deepseq filepath http-conduit http-date http-types network network-conduit
    parsec time transformers unix-bytestring unordered-containers wai
    wai-app-file-cgi wai-logger wai-logger-prefork warp
  ];
  testDepends = [
    deepseq filepath http-conduit http-date http-types HUnit network
    network-conduit parsec test-framework-hunit test-framework-th-prime time
    transformers unix-bytestring unordered-containers wai wai-app-file-cgi
    wai-logger wai-logger-prefork warp
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.mew.org/~kazu/proj/mighttpd/";
    description = "High performance web server on WAI/warp";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
