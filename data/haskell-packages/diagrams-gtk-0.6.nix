{ cabal, diagrams-cairo, diagrams-lib, gtk }:

cabal.mkDerivation (self: {
  pname = "diagrams-gtk";
  version = "0.6";
  sha256 = "1wjvy2wazzljbwzdank160jhfhk6nf98dwdhx5fw8ywi3n09rkw9";
  buildDepends = [ diagrams-cairo diagrams-lib gtk ];
  doCheck = false;
  meta = {
    homepage = "http://projects.haskell.org/diagrams/";
    description = "Backend for rendering diagrams directly to GTK windows";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
