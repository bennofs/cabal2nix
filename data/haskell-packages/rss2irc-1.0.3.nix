{ cabal, cabal-file-th, cmdargs, deepseq, feed, http-conduit, http-types
, io-storage, irc, network, parsec, regexpr, resourcet, safe, split, text, time
, transformers, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "rss2irc";
  version = "1.0.3";
  sha256 = "07z57c4xzx57z6f34qzlgbrdvxdzcv41adrvkvwvcx0c7g0cf0b7";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    cabal-file-th cmdargs deepseq feed http-conduit http-types io-storage irc
    network parsec regexpr resourcet safe split text time transformers
    utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://hackage.haskell.org/package/rss2irc";
    description = "watches an RSS/Atom feed and writes it to an IRC channel";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
