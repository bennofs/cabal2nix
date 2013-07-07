{ cabal, comonad, comonad-transformers, comonads-fd, contravariant
, distributive, free, keys, mtl, semigroupoids, semigroups, transformers
}:

cabal.mkDerivation (self: {
  pname = "representable-functors";
  version = "3.0.1";
  sha256 = "17ksh76jn2a4ykqsl81vfavrw05hyj485v2v2aa9z8nif6z4f0q5";
  buildDepends = [
    comonad comonad-transformers comonads-fd contravariant distributive free
    keys mtl semigroupoids semigroups transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/representable-functors/";
    description = "Representable functors";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
