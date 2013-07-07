{ cabal, haskelldb, haskelldb-hdbc, HDBC, HDBC-sqlite3, mtl }:

cabal.mkDerivation (self: {
  pname = "haskelldb-hdbc-sqlite3";
  version = "2.1.2";
  sha256 = "0qdp2lzhk8ywb9pmppvvmwqkaagvrnyjiglahy6mb7rdfq89bas6";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ haskelldb haskelldb-hdbc HDBC HDBC-sqlite3 mtl ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/m4dc4p/haskelldb";
    description = "HaskellDB support for the HDBC SQLite driver";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
