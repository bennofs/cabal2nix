{ cabal, comonad, comonad-transformers, semigroupoids, transformers }:

cabal.mkDerivation (self: {
  pname = "data-lens";
  version = "2.10.2";
  sha256 = "1mv4lh5rhmq09d89ci07kj9anv7ckrlqh9x3xr4jq9idf7sf54b5";
  buildDepends = [ comonad comonad-transformers semigroupoids transformers ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/roconnor/data-lens/";
    description = "Haskell 98 Lenses";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
