{ cabal, blaze-builder, deepseq, filepath, mtl, parsec, syb, syb-with-class
, text, time, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "HStringTemplate";
  version = "0.6.12";
  sha256 = "02jx02qbs4jxpf1s8nzc9lbaz0flkfcy6xj475v77i45xc1hc71p";
  buildDepends = [
    blaze-builder deepseq filepath mtl parsec syb syb-with-class text time
    utf8-string
  ];
  doCheck = false;
  meta = {
    description = "StringTemplate implementation in Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
