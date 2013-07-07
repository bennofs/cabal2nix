{ cabal, hashable, primes }:

cabal.mkDerivation (self: {
  pname = "static-hash";
  version = "0.0.1";
  sha256 = "0nkgx4s389027zi23wmbc6wqnmplvjvbrsbyzy7zn41mbwmzqz8l";
  buildDepends = [ hashable primes ];
  doCheck = false;
  meta = {
    description = "Immutable hash";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
