{ cabal, text }:

cabal.mkDerivation (self: {
  pname = "polyparse";
  version = "1.7";
  sha256 = "0jrngb8z4nkwnck4ls1qlbs8v8pc49xppn9xg2q1py7iak7d13ny";
  buildDepends = [ text ];
  doCheck = false;
  meta = {
    homepage = "http://code.haskell.org/~malcolm/polyparse/";
    description = "A variety of alternative parser combinator libraries";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
