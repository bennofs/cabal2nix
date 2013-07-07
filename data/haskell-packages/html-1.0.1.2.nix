{ cabal }:

cabal.mkDerivation (self: {
  pname = "html";
  version = "1.0.1.2";
  sha256 = "0q9hmfii62kc82ijlg238fxrzxhsivn42x5wd6ffcr9xldg4jd8c";
  doCheck = false;
  meta = {
    description = "HTML combinator library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
