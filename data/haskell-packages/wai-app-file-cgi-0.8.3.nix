{ cabal, attoparsec, attoparsec-conduit, blaze-builder, blaze-html
, case-insensitive, conduit, date-cache, doctest, fast-logger, filepath, hspec
, http-conduit, http-date, http-types, io-choice, lifted-base, mime-types
, network, resourcet, static-hash, text, transformers, wai, wai-logger, warp
, word8
}:

cabal.mkDerivation (self: {
  pname = "wai-app-file-cgi";
  version = "0.8.3";
  sha256 = "17a9lsn1q6qdrji24lv75gaczdsg7xbmky032ml8hhndb8vwq0s5";
  buildDepends = [
    attoparsec attoparsec-conduit blaze-builder blaze-html case-insensitive
    conduit date-cache fast-logger filepath http-conduit http-date http-types
    io-choice lifted-base mime-types network resourcet static-hash text
    transformers wai wai-logger word8
  ];
  testDepends = [
    conduit doctest filepath hspec http-conduit http-types wai warp
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.mew.org/~kazu/proj/mighttpd/";
    description = "File/CGI/Rev Proxy App of WAI";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
