{ cabal, haskelldb, haskelldb-hdbc, HDBC, HDBC-odbc, mtl }:

cabal.mkDerivation (self: {
  pname = "haskelldb-hdbc-odbc";
  version = "2.1.2";
  sha256 = "1y87iiil6k9ycsni70kvfcq50fiws8aqnhmk4018f6k6q1bx7q6p";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ haskelldb haskelldb-hdbc HDBC HDBC-odbc mtl ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/m4dc4p/haskelldb";
    description = "HaskellDB support for the HDBC ODBC driver";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
