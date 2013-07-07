{ cabal, nats }:

cabal.mkDerivation (self: {
  pname = "semigroups";
  version = "0.9";
  sha256 = "0cwyjjlr9zgpxryzdf26pb58dmad0cp8d0493rarhh5zmgighh90";
  buildDepends = [ nats ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/semigroups/";
    description = "Haskell 98 semigroups";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
