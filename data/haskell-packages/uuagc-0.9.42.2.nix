{ cabal, filepath, haskell-src-exts, mtl, uuagc-cabal, uulib }:

cabal.mkDerivation (self: {
  pname = "uuagc";
  version = "0.9.42.2";
  sha256 = "1l7w3gimcx079giw5ri4qfr1xfi1wfj93v29r8hvs8q8a6ffjifn";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ filepath haskell-src-exts mtl uuagc-cabal uulib ];
  doCheck = false;
  meta = {
    homepage = "http://www.cs.uu.nl/wiki/HUT/WebHome";
    description = "Attribute Grammar System of Universiteit Utrecht";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
