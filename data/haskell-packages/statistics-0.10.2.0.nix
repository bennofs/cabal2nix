{ cabal, deepseq, erf, HUnit, ieee754, math-functions, monad-par, mwc-random
, primitive, QuickCheck, test-framework, test-framework-hunit
, test-framework-quickcheck2, vector, vector-algorithms
}:

cabal.mkDerivation (self: {
  pname = "statistics";
  version = "0.10.2.0";
  sha256 = "1sv0fhbi52maq9c4ni109m0051a1nndi3ncz9v29mkxqzyckrp9x";
  buildDepends = [
    deepseq erf math-functions monad-par mwc-random primitive vector
    vector-algorithms
  ];
  testDepends = [
    erf HUnit ieee754 math-functions primitive QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 vector vector-algorithms
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/bos/statistics";
    description = "A library of statistical types, data, and functions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
