{ cabal, newtype, QuickCheck, semigroupoids, semigroups, vector-space }:

cabal.mkDerivation (self: {
  pname = "active";
  version = "0.1.0.4";
  sha256 = "0f4bgx7l9vx2kywl87zpxpjbfix79lp8chf6xzfpmh4wrbmfgi1s";
  buildDepends = [ newtype semigroupoids semigroups vector-space ];
  testDepends = [ newtype QuickCheck semigroupoids semigroups vector-space ];
  doCheck = false;
  meta = {
    description = "Abstractions for animation";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
