{ cabal, blaze-html, blaze-markup, hamlet, pandoc, persistent, text
, xss-sanitize, yesod-core, yesod-form
}:

cabal.mkDerivation (self: {
  pname = "yesod-markdown";
  version = "0.7.2";
  sha256 = "1ahwk6d58gpn7ypr075wnf1ha0h2c2hzlzz0wxf02780np3k09i1";
  buildDepends = [
    blaze-html blaze-markup hamlet pandoc persistent text xss-sanitize
    yesod-core yesod-form
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/pbrisbin/yesod-markdown";
    description = "Tools for using markdown in a yesod application";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
