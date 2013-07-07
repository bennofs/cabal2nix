{ cabal, attoparsec, css-text, hspec, HUnit, network, tagsoup, text
, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "xss-sanitize";
  version = "0.3.2";
  sha256 = "0m7gl232i06i090kbrlz67cs4q3pqf8169m9kjdj41kj6jay1dcx";
  buildDepends = [ attoparsec css-text network tagsoup text utf8-string ];
  testDepends = [
    attoparsec css-text hspec HUnit network tagsoup text utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/yesodweb/haskell-xss-sanitize";
    description = "sanitize untrusted HTML to prevent XSS attacks";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
