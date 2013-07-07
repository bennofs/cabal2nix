{ cabal, hashable, HUnit, test-framework, test-framework-hunit, transformers
, unordered-containers, vault
}:

cabal.mkDerivation (self: {
  pname = "reactive-banana";
  version = "0.7.1.2";
  sha256 = "1x4ln3dr937va0ii7lr86d6wsrh2qd1sxany4y9dkpcrsvb3db0l";
  buildDepends = [ hashable transformers unordered-containers vault ];
  testDepends = [
    hashable HUnit test-framework test-framework-hunit transformers
    unordered-containers vault
  ];
  doCheck = false;
  meta = {
    homepage = "http://haskell.org/haskellwiki/Reactive-banana";
    description = "Practical library for functional reactive programming (FRP)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
