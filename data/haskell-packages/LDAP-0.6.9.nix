{ cabal, lber, ldap }:

cabal.mkDerivation (self: {
  pname = "LDAP";
  version = "0.6.9";
  sha256 = "0v1bkl8lr349m2n1pg47954lxwnnj9n7j4arr70qgz5aw7x8ypc2";
  isLibrary = true;
  isExecutable = true;
  extraLibraries = [ lber ldap ];
  doCheck = false;
  meta = {
    homepage = "https://github.com/ezyang/ldap-haskell";
    description = "Haskell binding for C LDAP API";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
