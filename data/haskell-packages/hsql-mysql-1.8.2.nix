{ cabal, Cabal, hsql, mysqlclient }:

cabal.mkDerivation (self: {
  pname = "hsql-mysql";
  version = "1.8.2";
  sha256 = "099hacki86zv6kc2jbbjha74jggx7gmywiip11bjhj1qi0wca2vq";
  buildDepends = [ Cabal hsql ];
  extraLibraries = [ mysqlclient ];
  doCheck = false;
  meta = {
    description = "MySQL driver for HSQL";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
