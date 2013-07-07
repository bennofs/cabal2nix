{ cabal, hsql, postgresql }:

cabal.mkDerivation (self: {
  pname = "hsql-postgresql";
  version = "1.8.2";
  sha256 = "0yj0jalpapjvpxmc79yd6bn93ax13pp87dipbg2c9mxf3p38jc9z";
  buildDepends = [ hsql ];
  extraLibraries = [ postgresql ];
  doCheck = false;
  meta = {
    description = "A Haskell Interface to PostgreSQL via the PQ library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
