{ cabal, haskell-src-exts, language-haskell-extract, regex-posix
, test-framework
}:

cabal.mkDerivation (self: {
  pname = "test-framework-th";
  version = "0.2.2";
  sha256 = "0nzfvxr5bnxinx41a5w5mwhyxzz2936dl0xhd80cv9plx19ylh0w";
  buildDepends = [
    haskell-src-exts language-haskell-extract regex-posix test-framework
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/finnsson/test-generator";
    description = "Automagically generate the HUnit- and Quickcheck-bulk-code using Template Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
