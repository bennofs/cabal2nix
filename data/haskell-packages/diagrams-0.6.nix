{ cabal, diagrams-contrib, diagrams-core, diagrams-lib, diagrams-svg }:

cabal.mkDerivation (self: {
  pname = "diagrams";
  version = "0.6";
  sha256 = "1i62jbixjzw82y622ymp6lrp4kzgn7iv55arivvh0y46bbmybqvh";
  buildDepends = [ diagrams-contrib diagrams-core diagrams-lib diagrams-svg ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/diagrams";
    description = "Embedded domain-specific language for declarative vector graphics";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
