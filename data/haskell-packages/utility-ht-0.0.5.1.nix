{ cabal }:

cabal.mkDerivation (self: {
  pname = "utility-ht";
  version = "0.0.5.1";
  sha256 = "1jwpdicqxrjl73m91f38i8zz56f8yz7j0dzyskk05w1i1d2x2pzi";
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  meta = {
    description = "Various small helper functions for Lists, Maybes, Tuples, Functions";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
