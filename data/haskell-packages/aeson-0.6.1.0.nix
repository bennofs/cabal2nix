{ cabal, attoparsec, blaze-builder, deepseq, dlist, hashable, mtl, QuickCheck
, syb, test-framework, test-framework-quickcheck2, text, time
, unordered-containers, vector
}:

cabal.mkDerivation (self: {
  pname = "aeson";
  version = "0.6.1.0";
  sha256 = "16hjwcybmgmk1sg8x02r9bxisx4gl61rlq8w2zsxfgkxwjpfhkbx";
  buildDepends = [
    attoparsec blaze-builder deepseq dlist hashable mtl syb text time
    unordered-containers vector
  ];
  testDepends = [
    attoparsec QuickCheck test-framework test-framework-quickcheck2 text time
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/aeson";
    description = "Fast JSON parsing and encoding";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
