{ cabal, hsql, sqlite }:

cabal.mkDerivation (self: {
  pname = "hsql-sqlite3";
  version = "1.8.2";
  sha256 = "15f7f4f4k1afrpmkw2k6lyx1b81hlwvwv660yh0vm2vz269mxycl";
  buildDepends = [ hsql ];
  extraLibraries = [ sqlite ];
  doCheck = false;
  meta = {
    description = "SQLite3 driver for HSQL";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
