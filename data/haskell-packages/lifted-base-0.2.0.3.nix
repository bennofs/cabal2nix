{ cabal, base-unicode-symbols, HUnit, monad-control, test-framework
, test-framework-hunit, transformers, transformers-base
}:

cabal.mkDerivation (self: {
  pname = "lifted-base";
  version = "0.2.0.3";
  sha256 = "1sfrak4jf3mvns9y6iadyhj8zvy4wyrqiaxihrxv2qby14c45psx";
  buildDepends = [ base-unicode-symbols monad-control transformers-base ];
  testDepends = [
    HUnit monad-control test-framework test-framework-hunit transformers
    transformers-base
  ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/basvandijk/lifted-base";
    description = "lifted IO operations from the base library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
