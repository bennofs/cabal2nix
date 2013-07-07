{ cabal, ListLike, monad-control, MonadCatchIO-transformers, parallel
, transformers, transformers-base
}:

cabal.mkDerivation (self: {
  pname = "iteratee";
  version = "0.8.9.4";
  sha256 = "0j8q5i3kf1ld7630z65hj55p2jlhl23f6qjag4zwrhrh38bfr531";
  buildDepends = [
    ListLike monad-control MonadCatchIO-transformers parallel transformers
    transformers-base
  ];
  doCheck = false;
  meta = {
    homepage = "http://www.tiresiaspress.us/haskell/iteratee";
    description = "Iteratee-based I/O";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
