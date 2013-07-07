{ cabal, blaze-html, blaze-markup, hamlet, text, time, xml-conduit, yesod-core
}:

cabal.mkDerivation (self: {
  pname = "yesod-newsfeed";
  version = "1.1.0.1";
  sha256 = "1mmv38709p82xvcq7cz83mf84dl3w2w9a7yhzplh4bzjbnwlx84v";
  buildDepends = [
    blaze-html blaze-markup hamlet text time xml-conduit yesod-core
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.yesodweb.com/";
    description = "Helper functions and data types for producing News feeds";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
