{ cabal, haskelldb, haskelldb-hdbc, HDBC, HDBC-postgresql, mtl, postgresql }:

cabal.mkDerivation (self: {
  pname = "haskelldb-hdbc-postgresql";
  version = "2.1.2";
  sha256 = "08cv3y22zvf0pf1cs57vrma3vp3b5fzzs6nsnvcnfnzsvacanh7g";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ haskelldb haskelldb-hdbc HDBC HDBC-postgresql mtl ];
  extraLibraries = [ postgresql ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/m4dc4p/haskelldb";
    description = "HaskellDB support for the HDBC PostgreSQL driver";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
