{ cabal, filepath, hexpat, hs-bibutils, HTTP, json, mtl, network, pandoc-types
, parsec, syb, time, utf8-string
}:

cabal.mkDerivation (self: {
  pname = "citeproc-hs";
  version = "0.3.8";
  sha256 = "0wlfwjxg852qcgx54m99xm7hxsmcw8c8r7fyrsxyxl3054xnfwz8";
  buildDepends = [
    filepath hexpat hs-bibutils HTTP json mtl network pandoc-types parsec syb
    time utf8-string
  ];
  doCheck = false;
  meta = {
    homepage = "http://gorgias.mine.nu/repos/citeproc-hs/";
    description = "A Citation Style Language implementation in Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
