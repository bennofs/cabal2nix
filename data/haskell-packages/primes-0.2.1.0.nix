{ cabal }:

cabal.mkDerivation (self: {
  pname = "primes";
  version = "0.2.1.0";
  sha256 = "0ny6fzr967d1fifk050k95j9snnbjjif2bxf3v9s93k3zdc6bmkl";
  doCheck = false;
  meta = {
    homepage = "http://github.com/sebfisch/primes";
    description = "Efficient, purely functional generation of prime numbers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
