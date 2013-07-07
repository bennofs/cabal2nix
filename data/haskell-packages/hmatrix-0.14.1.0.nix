{ cabal, binary, blas, gsl, liblapack, random, storable-complex, vector }:

cabal.mkDerivation (self: {
  pname = "hmatrix";
  version = "0.14.1.0";
  sha256 = "10fvbk3k2fgac46a86mc8g0s5gsw1p1bz4k57gn6dzgwh73mxjx7";
  buildDepends = [ binary random storable-complex vector ];
  extraLibraries = [ blas gsl liblapack ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/albertoruiz/hmatrix";
    description = "Linear algebra and numerical computation";
    license = "GPL";
    platforms = self.ghc.meta.platforms;
  };
})
