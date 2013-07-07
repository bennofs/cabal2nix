{ cabal, binary, haskell-src, mtl, network, random, regex-compat, regex-posix
, syb, tagsoup, utf8-string, zlib
}:

cabal.mkDerivation (self: {
  pname = "lambdabot-utils";
  version = "4.2.2";
  sha256 = "0mmz9rn6vv8xnavmz66g164h1liir3rzg1n7lmbcsgwcyhm925d7";
  buildDepends = [
    binary haskell-src mtl network random regex-compat regex-posix syb tagsoup
    utf8-string zlib
  ];
  doCheck = false;
  meta = {
    homepage = "http://haskell.org/haskellwiki/Lambdabot";
    description = "Utility libraries for the advanced IRC bot, Lambdabot";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
