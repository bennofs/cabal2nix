{ cabal, mtl, network, pureMD5, stm }:

cabal.mkDerivation (self: {
  pname = "AMI";
  version = "0.1";
  sha256 = "00w6kcikc5ac26786fwshwbh8ndj9aq1w7wz263j5rnhdf12irky";
  buildDepends = [ mtl network pureMD5 stm ];
  doCheck = false;
  meta = {
    homepage = "http://redmine.iportnov.ru/projects/ami";
    description = "Low-level bindings for Asterisk Manager Interface (AMI)";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
