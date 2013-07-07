{ cabal, bifunctors, comonad, comonad-transformers, comonads-fd, contravariant
, deepseq, distributive, doctest, filepath, generic-deriving, hashable, HUnit
, MonadCatchIO-transformers, mtl, nats, parallel, profunctor-extras
, profunctors, QuickCheck, reflection, semigroupoids, semigroups
, simple-reflect, split, tagged, test-framework, test-framework-hunit
, test-framework-quickcheck2, test-framework-th, text, transformers
, transformers-compat, unordered-containers, vector, void
}:

cabal.mkDerivation (self: {
  pname = "lens";
  version = "3.9.0.2";
  sha256 = "0ch38z33zgdyzjmc7jqwabwq68iidicpzhbwzpd4fvcqdp27apl2";
  buildDepends = [
    bifunctors comonad comonad-transformers comonads-fd contravariant
    distributive filepath generic-deriving hashable MonadCatchIO-transformers
    mtl parallel profunctor-extras profunctors reflection semigroupoids
    semigroups split tagged text transformers transformers-compat
    unordered-containers vector void
  ];
  testDepends = [
    deepseq doctest filepath generic-deriving HUnit mtl nats parallel
    QuickCheck semigroups simple-reflect split test-framework
    test-framework-hunit test-framework-quickcheck2 test-framework-th text
    transformers unordered-containers vector
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/lens/";
    description = "Lenses, Folds and Traversals";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
