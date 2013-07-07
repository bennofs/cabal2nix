{ cabal, void }:

cabal.mkDerivation (self: {
  pname = "categories";
  version = "1.0.4";
  sha256 = "1imyvinw3ax0s51pvqlzdlsiwhh58d64hf1pb6dnbibr9wsgl67x";
  buildDepends = [ void ];
  doCheck = false;
  meta = {
    homepage = "http://github.com/ekmett/categories";
    description = "Categories";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
