{ cabal, hsql, odbc }:

cabal.mkDerivation (self: {
  pname = "hsql-odbc";
  version = "1.8.2";
  sha256 = "0lz9zjy1xgjjbabbi2hcrvsnfid6c78y2cb2703qjwr93xy54f1f";
  buildDepends = [ hsql ];
  extraLibraries = [ odbc ];
  doCheck = false;
  meta = {
    description = "A Haskell Interface to ODBC";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
