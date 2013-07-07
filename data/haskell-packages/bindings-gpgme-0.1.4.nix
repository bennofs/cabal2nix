{ cabal, bindings-DSL, gpgme }:

cabal.mkDerivation (self: {
  pname = "bindings-gpgme";
  version = "0.1.4";
  sha256 = "0lj84j47y3j4v7zd7z99lb0bq68w12dir9fnbf9fg3si8hvkaj5s";
  buildDepends = [ bindings-DSL ];
  extraLibraries = [ gpgme ];
  doCheck = false;
  meta = {
    homepage = "http://bitbucket.org/mauricio/bindings-gpgme";
    description = "Low level bindings to gpgme";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
